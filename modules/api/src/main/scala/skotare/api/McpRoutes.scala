package skotare.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.{ACursor, Decoder, Json}
import io.circe.parser.parse
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{`WWW-Authenticate`, Allow, Authorization}
import sbirds.http.Errors
import skotare.core.ErrorCode
import skotare.shared.*

/**
 * MCP（Model Context Protocol）のサーバー. POST /mcp に JSON-RPC を送る（Streamable HTTP. 応答は JSON で、SSE は使わない）.
 * 認証は API トークン（Bearer）だけ. 各ツールは REST API をそのトークンで内部から呼ぶので、権限は REST API と同じになる.
 */
final class McpRoutes(
  api: HttpRoutes[IO],
  sessions: SessionAuth,
  publicUrl: String,
  version: String) extends Http4sDsl[IO] {

  private val base = publicUrl.stripSuffix("/")
  private val Versions = List("2025-06-18", "2025-03-26", "2024-11-05")

  // ------------------------------------------------------------------ REST API の内部呼び出し

  private final class Caller(
    auth: Authorization) {
    def apply(
      method: Method,
      uri: Uri,
      body: Option[Json] = None
    ): IO[Either[String, Json]] = {
      val req0 = Request[IO](method, uri).putHeaders(auth)
      val req = body.fold(req0)(req0.withEntity(_))
      api.run(req).value.flatMap {
        case None => IO.pure(Left("404 見つかりません"))
        case Some(r) =>
          r.bodyText.compile.string.map { s =>
            val j = if s.isEmpty then Json.Null else parse(s).getOrElse(Json.fromString(s))
            if r.status.isSuccess then Right(j)
            else Left(s"${r.status.code} ${j.hcursor.get[String]("error").getOrElse(r.status.reason)}")
          }
      }
    }

    def as[A: Decoder](
      method: Method,
      uri: Uri,
      body: Option[Json] = None
    ): IO[Either[String, A]] =
      apply(method, uri, body).map(_.flatMap(_.as[A].left.map(e => s"応答を読めません: ${e.getMessage}")))
  }

  private def path(
    segments: String*
  ): Uri = Uri(path = Uri.Path(segments.toVector.map(Uri.Path.Segment(_)), absolute = true))

  private final case class Ctx(
    call: Caller,
    me: MeDto)

  // ------------------------------------------------------------------ 引数

  private type Result = IO[Either[String, String]]

  private final case class Tool(
    name: String,
    description: String,
    schema: Json,
    readOnly: Boolean,
    destructive: Boolean = false
  )(
    val run: (Ctx, ACursor) => Result) {
    def json: Json = Json.obj(
      "name" -> name.asJson,
      "description" -> description.asJson,
      "inputSchema" -> schema,
      "annotations" -> Json.obj(
        "readOnlyHint" -> readOnly.asJson,
        "destructiveHint" -> destructive.asJson,
        "openWorldHint" -> (name.contains("test") || name == "check_now").asJson
      )
    )
  }

  private def schema(
    required: List[String],
    props: (String, Json)*
  ): Json =
    Json.obj("type" -> "object".asJson, "properties" -> Json.obj(props*), "required" -> required.asJson)
  private def str(
    d: String
  ): Json = Json.obj("type" -> "string".asJson, "description" -> d.asJson)
  private def int(
    d: String
  ): Json = Json.obj("type" -> "integer".asJson, "description" -> d.asJson)
  private def bool(
    d: String
  ): Json = Json.obj("type" -> "boolean".asJson, "description" -> d.asJson)
  private def obj(
    d: String
  ): Json = Json.obj("type" -> "object".asJson, "description" -> d.asJson)
  private def oneOf(
    d: String,
    vs: String*
  ): Json = Json.obj("type" -> "string".asJson, "enum" -> vs.toList.asJson, "description" -> d.asJson)

  private def need(
    a: ACursor,
    k: String
  ): Either[String, String] = opt(a, k).filter(_.nonEmpty).toRight(s"$k を指定してください")
  private def opt(
    a: ACursor,
    k: String
  ): Option[String] = a.get[String](k).toOption.map(_.trim)

  extension [A](
    io: IO[Either[String, A]]
  ) {
    private def andThen[B](
      f: A => IO[Either[String, B]]
    ): IO[Either[String, B]] = io.flatMap(_.fold(e => IO.pure(Left(e)), f))
    private def mapR[B](
      f: A => B
    ): IO[Either[String, B]] = io.map(_.map(f))
  }

  // ------------------------------------------------------------------ 監視

  private def monitors(
    c: Ctx
  ) = c.call.as[List[MonitorDto]](Method.GET, path("api", "monitors"))

  /**
   * 監視の指定（ID か、ただ1つに決まる名前）. 名前が一致しない・いくつも当たるときは候補を示して失敗にする
   * （変更・削除のツールで、思っていない監視に当たらないように. 部分一致では選ばない）.
   */
  private def monitor(
    c: Ctx,
    a: ACursor
  ): IO[Either[String, MonitorDto]] =
    IO.pure(need(a, "monitor")).andThen(v =>
      monitors(c).mapR { ms =>
        ms.find(_.id.toString == v).map(Right(_)).getOrElse(ms.filter(_.name == v) match {
          case List(m) => Right(m)
          case Nil =>
            val near = ms.filter(_.name.contains(v)).take(10)
            Left(s"監視「$v」が見つかりません" +
              (if near.isEmpty then "（list_monitors で確かめてください）"
               else "。候補（id で指定してください）:\n" + near.map(line).mkString("\n")))
          case many => Left(s"「$v」という名前の監視が ${many.size} 件あります。id で指定してください:\n" + many.map(line).mkString("\n"))
        })
      }.map(_.flatten)
    )

  private def pct(
    v: Option[Double]
  ): String = v.fold("-")(x => f"$x%.2f%%")

  private def line(
    m: MonitorDto
  ): String =
    s"- ${m.name} [${MonStatus.label(m.status)}] ${Kind.label(m.kind)} ${m.target} / 24時間 ${pct(m.uptime24h)} / 30日 ${pct(m.uptime30d)}" +
      m.lastLatencyMs.fold("")(ms => s" / ${ms}ms") +
      m.lastMessage.filter(_ => m.status == MonStatus.Down).fold("")(x => s" / $x") + s" (id: ${m.id})"

  private val configKeys = List(
    "url",
    "method",
    "headers",
    "body",
    "expectStatus",
    "bodyContains",
    "bodyNotContains",
    "bodyRegex",
    "followRedirects",
    "certExpiryDays",
    "host",
    "port",
    "recordType",
    "dnsServer",
    "expect"
  )
  private val reqKeys =
    List("name", "kind", "intervalSec", "timeoutMs", "retries", "enabled", "description", "canary", "confirmRegions")

  private val monitorProps = List(
    "name" -> str("名前"),
    "kind" -> oneOf("種類", Kind.all*),
    "url" -> str("HTTP: URL（http:// か https://）"),
    "method" -> str("HTTP: メソッド（既定 GET）"),
    "headers" ->
      Json.obj("type" -> "array".asJson, "items" -> obj("{name, value}"), "description" -> "HTTP: 送るヘッダー".asJson),
    "body" -> str("HTTP: 送る本文"),
    "expectStatus" -> str("HTTP: 期待する応答コード（既定 200-299. 例 200-299,301）"),
    "bodyContains" -> str("HTTP: 本文に含まれるべき文字"),
    "bodyNotContains" -> str("HTTP: 本文に含まれてはいけない文字"),
    "bodyRegex" -> str("HTTP: 本文が合うべき正規表現"),
    "followRedirects" -> bool("HTTP: リダイレクトをたどる（既定 true）"),
    "certExpiryDays" -> int("HTTP: 証明書の期限までの日数がこれを切ったら失敗（0 で見ない）"),
    "host" -> str("TCP・PING: ホスト名か IP. DNS: 引く名前"),
    "port" -> int("TCP: ポート"),
    "recordType" -> oneOf("DNS: レコードの種類（既定 A）", MonitorCheck.RecordTypes*),
    "dnsServer" -> str("DNS: 問い合わせる DNS サーバー（省くとシステムの設定）"),
    "expect" -> str("DNS: 答えに含まれるべき値"),
    "intervalSec" -> int("確認の間隔（秒. 20〜86400. 既定 60）"),
    "timeoutMs" -> int("タイムアウト（ミリ秒. 500〜60000. 既定 10000）"),
    "retries" -> int("停止と判断するまでに続けて失敗してよい回数（0〜10. 既定 1）"),
    "enabled" -> bool("確かめる（false で一時停止）"),
    "description" -> str("説明"),
    "canary" -> bool("基準点にする（拠点そのものが外へ届くかの判定に使う. 既定 false）"),
    "confirmRegions" -> int("停止と判断するのに要る、停止と見ている拠点の数（0 は過半数. 既定 0）")
  )

  /** 引数を、元（変更なら今の監視）に重ねて MonitorReq の JSON にする. */
  private def monitorJson(
    a: ACursor,
    cur: Option[MonitorDto]
  ): Json = {
    val args = a.focus.flatMap(_.asObject).getOrElse(io.circe.JsonObject.empty)
    val cfg0 = cur.map(_.config.asJson).getOrElse(Json.obj())
    // 新しく作るときは、省いた項目を MonitorReq の既定の値にまかせる
    val req0 = cur.fold(Json.obj())(_.toReq.asJson.mapObject(_.remove("config")))
    val cfg = cfg0.deepMerge(Json.fromJsonObject(args.filterKeys(configKeys.contains)))
    req0.deepMerge(Json.fromJsonObject(args.filterKeys(reqKeys.contains))).deepMerge(Json.obj("config" -> cfg))
  }

  private def simple(
    c: Ctx,
    a: ACursor,
    f: MonitorDto => IO[Either[String, Json]],
    done: MonitorDto => String
  ): Result =
    monitor(c, a).andThen(m => f(m).mapR(_ => done(m)))

  private val tools: List[Tool] = List(
    Tool("get_status", "全体の様子（状態ごとの監視の数・続いている障害・停止中の監視・拠点の健全性・動いているノード）", schema(Nil), readOnly = true) {
      (
        c,
        _
      ) =>
        c.call.as[SummaryDto](Method.GET, path("api", "summary")).andThen { s =>
          monitors(c).mapR { ms =>
            val down = ms.filter(_.status == MonStatus.Down)
            (s"正常 ${s.up}・停止 ${s.down}・一時停止 ${s.paused}・未確認 ${s.pending}、続いている障害 ${s.openIncidents} 件" ::
              (if down.isEmpty then Nil else "停止中:" :: down.map(line)) :::
              ("拠点: " + s.regions.map(r =>
                s"${r.name}（${RegionHealth.label(r.health)}・ノード ${r.nodes} 台${
                    if r.reason.isEmpty then "" else s"・${r.reason}"
                  }）"
              ).mkString("、")) ::
              ("ノード: " +
                s.nodes.map(n => s"${n.name}@${n.region}${if n.alive then "" else "（止まっている）"}").mkString("、")) ::
              Nil).mkString("\n")
          }
        }
    },
    Tool(
      "list_monitors",
      "監視の一覧（状態・稼働率・応答時間）",
      schema(Nil, "status" -> oneOf("この状態のものだけ", "up", "down", "paused", "pending")),
      readOnly = true
    ) {
      (
        c,
        a
      ) =>
        monitors(c).mapR { ms =>
          val xs = opt(a, "status").filter(_.nonEmpty).fold(ms)(s => ms.filter(_.status == s))
          if xs.isEmpty then "監視はありません" else xs.map(line).mkString("\n")
        }
    },
    Tool("get_monitor", "監視の詳細（設定・最近の結果・障害）", schema(List("monitor"), "monitor" -> str("監視の名前か ID")), readOnly = true) {
      (
        c,
        a
      ) =>
        monitor(c, a).andThen(m =>
          c.call.as[MonitorDetailDto](Method.GET, path("api", "monitors", m.id.toString))
        ).mapR { d =>
          (line(d.monitor) :: s"設定: ${d.monitor.config.asJson.deepDropNullValues.noSpaces}" ::
            s"間隔 ${d.monitor.intervalSec}秒・タイムアウト ${d.monitor.timeoutMs}ms・再試行 ${d.monitor.retries}回" ::
            "拠点ごと:" :: d.monitor.regions.map(r =>
              s"- ${r.region}: ${MonStatus.label(r.status)}${r.message.filter(_ => r.status == MonStatus.Down).fold("")(
                  m => s" / $m"
                )}${if r.counted then "" else "（判定に数えていない）"}"
            ) :::
            "最近の結果:" :: d.checks.take(10).map(x =>
              s"- ${x.at} ${if x.ok then "OK" else "NG"} ${x.latencyMs.fold("")(ms => s"${ms}ms ")}${x.message} (${x.region}/${x.node})"
            ) :::
            (if d.incidents.isEmpty then Nil
             else
               "障害:" ::
                 d.incidents.map(i => s"- ${i.startedAt} 〜 ${i.resolvedAt.getOrElse("続いている")} ${i.cause}"))).mkString(
            "\n"
          )
        }
    },
    Tool("test_monitor", "監視の設定を保存せずに今すぐ試す（相手に1回だけ問い合わせる）", schema(List("kind"), monitorProps*), readOnly = false) {
      (
        c,
        a
      ) =>
        c.call.as[ProbeResultDto](
          Method.POST,
          path("api", "monitors", "test"),
          Some(monitorJson(a, None).deepMerge(Json.obj("name" -> "test".asJson)))
        )
          .mapR(r => s"${if r.ok then "成功" else "失敗"}${r.latencyMs.fold("")(ms => s"（${ms}ms）")}: ${r.message}")
    },
    Tool("create_monitor", "監視を作る", schema(List("name", "kind"), monitorProps*), readOnly = false) {
      (
        c,
        a
      ) =>
        c.call.as[MonitorDto](Method.POST, path("api", "monitors"), Some(monitorJson(a, None))).mapR(m =>
          s"作りました: ${line(m)}\n$base/monitors/${m.id}"
        )
    },
    Tool(
      "update_monitor",
      "監視を変える（指定した項目だけ）",
      schema(List("monitor"), ("monitor" -> str("監視の名前か ID")) :: monitorProps*),
      readOnly = false
    ) {
      (
        c,
        a
      ) =>
        monitor(c, a).andThen(m =>
          c.call.as[MonitorDto](
            Method.PUT,
            path("api", "monitors", m.id.toString),
            Some(monitorJson(a, Some(m)))
          )
        ).mapR(m => s"変えました: ${line(m)}")
    },
    Tool("pause_monitor", "監視を一時停止する", schema(List("monitor"), "monitor" -> str("監視の名前か ID")), readOnly = false) {
      (
        c,
        a
      ) =>
        monitor(c, a).andThen(m =>
          c.call(
            Method.PUT,
            path("api", "monitors", m.id.toString),
            Some(monitorJson(Json.obj("enabled" -> false.asJson).hcursor, Some(m)))
          )
            .mapR(_ => s"一時停止しました: ${m.name}")
        )
    },
    Tool("resume_monitor", "一時停止した監視を再開する", schema(List("monitor"), "monitor" -> str("監視の名前か ID")), readOnly = false) {
      (
        c,
        a
      ) =>
        monitor(c, a).andThen(m =>
          c.call(
            Method.PUT,
            path("api", "monitors", m.id.toString),
            Some(monitorJson(Json.obj("enabled" -> true.asJson).hcursor, Some(m)))
          )
            .mapR(_ => s"再開しました: ${m.name}")
        )
    },
    Tool(
      "check_now",
      "監視をすぐに確かめる（結果は数秒後に get_monitor で見られる）",
      schema(List("monitor"), "monitor" -> str("監視の名前か ID")),
      readOnly = false
    ) {
      (
        c,
        a
      ) =>
        simple(
          c,
          a,
          m => c.call(Method.POST, path("api", "monitors", m.id.toString, "check")),
          m => s"確かめます: ${m.name}"
        )
    },
    Tool(
      "delete_monitor",
      "監視を削除する（結果・障害も消える）",
      schema(List("monitor"), "monitor" -> str("監視の名前か ID")),
      readOnly = false,
      destructive = true
    ) {
      (
        c,
        a
      ) =>
        simple(c, a, m => c.call(Method.DELETE, path("api", "monitors", m.id.toString)), m => s"削除しました: ${m.name}")
    },
    Tool(
      "list_incidents",
      "障害の一覧（新しい順）",
      schema(Nil, "open" -> bool("続いているものだけ"), "monitor" -> str("この監視だけ（名前か ID）")),
      readOnly = true
    ) {
      (
        c,
        a
      ) =>
        val target =
          if opt(a, "monitor").exists(_.nonEmpty) then monitor(c, a).mapR(m => Some(m.id)) else IO.pure(Right(None))
        target.andThen { id =>
          val u0 = path("api", "incidents").withQueryParam("limit", "50")
          val u1 = if a.get[Boolean]("open").contains(true) then u0.withQueryParam("open", "true") else u0
          c.call.as[List[IncidentDto]](Method.GET, id.fold(u1)(x => u1.withQueryParam("monitor", x.toString)))
        }.mapR(is =>
          if is.isEmpty then "障害はありません"
          else
            is.map(i => s"- ${i.monitorName}: ${i.startedAt} 〜 ${i.resolvedAt.getOrElse("続いている")} ${i.cause}").mkString(
              "\n"
            )
        )
    },
    Tool("list_status_pages", "ステータスページの一覧（ゲストが見る URL）", schema(Nil), readOnly = true) {
      (
        c,
        _
      ) =>
        c.call.as[List[StatusPageDto]](Method.GET, path("api", "pages")).mapR(ps =>
          if ps.isEmpty then "ステータスページはありません"
          else
            ps.map(p =>
              s"- ${p.title}: $base/s/${p.slug}（${if p.public then "公開" else "ログインした人だけ"}・監視 ${p.items.size} 件. id: ${p.id}）"
            ).mkString("\n")
        )
    },
    Tool(
      "list_api_endpoints",
      "REST API の一覧. 専用のツールがない操作（通知先・ステータスページの編集など）は call_api で行える.",
      schema(Nil),
      readOnly = true
    ) {
      (
        _,
        _
      ) =>
        IO.pure(Right(ApiCatalog.all.map(e => s"${e.method} ${e.path} — ${e.description}").mkString("\n")))
    },
    Tool(
      "call_api",
      "任意の REST API を呼ぶ（list_api_endpoints を参照）. 権限はこのトークンの持ち主と同じ.",
      schema(
        List("method", "path"),
        "method" -> str("GET / POST / PUT / PATCH / DELETE"),
        "path" -> str("/api/ で始まるパス（クエリを含めてよい）"),
        "body" -> obj("JSON の本文")
      ),
      readOnly = false
    ) {
      (
        c,
        a
      ) =>
        (need(a, "method").map(_.toUpperCase), need(a, "path")).tupled match {
          case Left(e) => IO.pure(Left(e))
          case Right((m, p)) =>
            (
              Method.fromString(m).toOption.filter(x => Set("GET", "POST", "PUT", "PATCH", "DELETE")(x.name)),
              Uri.fromString(p).toOption.filter(u =>
                u.path.renderString.startsWith("/api/") && !u.path.renderString.contains("..")
              )
            ) match {
              case (Some(method), Some(uri)) =>
                c.call(method, uri, a.downField("body").focus.filterNot(_.isNull)).mapR(_.spaces2)
              case (None, _) => IO.pure(Left("method は GET / POST / PUT / PATCH / DELETE です"))
              case _ => IO.pure(Left("path は /api/ で始めてください"))
            }
        }
    }
  )

  private val byName = tools.map(t => t.name -> t).toMap

  // ------------------------------------------------------------------ JSON-RPC

  private def ok(
    id: Json,
    result: Json
  ): Json = Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "result" -> result)
  private def rpcFail(
    id: Json,
    code: Int,
    msg: String
  ): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "error" -> Json.obj("code" -> code.asJson, "message" -> msg.asJson))
  private def toolResult(
    r: Either[String, String]
  ): Json =
    Json.obj(
      "content" -> Json.arr(Json.obj("type" -> "text".asJson, "text" -> r.merge.asJson)),
      "isError" -> r.isLeft.asJson
    )

  private def handle(
    call: Caller,
    p: Principal,
    msg: Json
  ): IO[Option[Json]] = {
    val c = msg.hcursor
    val params = c.downField("params")
    c.downField("id").focus match {
      case None => IO.pure(None) // 通知（notifications/initialized など）には返さない
      case Some(id) =>
        c.get[String]("method").getOrElse("") match {
          case "initialize" =>
            val asked = params.get[String]("protocolVersion").toOption
            IO.pure(Some(ok(
              id,
              Json.obj(
                "protocolVersion" -> asked.filter(Versions.contains).getOrElse(Versions.head).asJson,
                "capabilities" -> Json.obj("tools" -> Json.obj("listChanged" -> false.asJson)),
                "serverInfo" -> Json.obj("name" -> "skotare".asJson, "version" -> version.asJson),
                "instructions" ->
                  ("skotare（外部監視）の監視・障害・ステータスページを読み書きするツールです。全体の様子は get_status、監視は list_monitors で探し、" +
                    "名前か ID で get_monitor・update_monitor・pause_monitor・resume_monitor・check_now を呼びます。新しい監視は test_monitor で設定を試してから create_monitor で作れます。" +
                    "専用のツールがない操作（通知先・ステータスページの編集）は list_api_endpoints で REST API を調べ、call_api で行えます。" +
                    (if p.canWrite then "" else "このトークンは参照のみなので、変更するツールは使えません。")).asJson
              )
            )))
          case "ping" => IO.pure(Some(ok(id, Json.obj())))
          case "tools/list" =>
            IO.pure(Some(ok(id, Json.obj("tools" -> tools.filter(t => p.canWrite || t.readOnly).map(_.json).asJson))))
          case "tools/call" =>
            val name = params.get[String]("name").getOrElse("")
            byName.get(name) match {
              case None => IO.pure(Some(rpcFail(id, -32602, s"unknown tool: $name")))
              case Some(t) if !p.canWrite && !t.readOnly =>
                IO.pure(Some(ok(id, toolResult(Left("参照のみの API トークンでは変更できません")))))
              case Some(t) =>
                val args = params.downField("arguments").focus.filter(_.isObject).getOrElse(Json.obj()).hcursor
                t.run(Ctx(call, p.user.me), args).handleError(e => Left(s"内部エラー: ${e.getMessage}")).map(r =>
                  Some(ok(id, toolResult(r)))
                )
            }
          case "resources/list" => IO.pure(Some(ok(id, Json.obj("resources" -> Json.arr()))))
          case "prompts/list" => IO.pure(Some(ok(id, Json.obj("prompts" -> Json.arr()))))
          case other => IO.pure(Some(rpcFail(id, -32601, s"method not found: $other")))
        }
    }
  }

  private val MaxBatch = 20

  private def unauthorized: IO[Response[IO]] =
    Errors.respond(ErrorCode.ApiTokenRequired).map(_.putHeaders(`WWW-Authenticate`(Challenge("Bearer", "skotare"))))

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "mcp" => postMcp(req)
    case (GET | DELETE) -> Root / "mcp" => mcpMethodNotAllowed()
    case GET -> Root / "api" / "endpoints" => getEndpoints()
  }

  private def postMcp(
    req: Request[IO]
  ): IO[Response[IO]] =
    req.headers.get[Authorization] match {
      case Some(auth @ Authorization(Credentials.Token(AuthScheme.Bearer, _))) =>
        sessions.fromRequest(Request[IO](Method.POST, Uri.unsafeFromString("/mcp")).putHeaders(auth)).flatMap {
          case None => unauthorized
          case Some(p) =>
            val call = Caller(auth)
            req.as[Json].attempt.flatMap {
              case Left(_) => BadRequest(rpcFail(Json.Null, -32700, "parse error"))
              case Right(j) =>
                j.asArray match {
                  case Some(batch) if batch.size > MaxBatch =>
                    BadRequest(rpcFail(Json.Null, -32600, s"一度に送れる要求は ${MaxBatch} 件までです"))
                  case Some(batch) =>
                    batch.toList.traverse(handle(call, p, _)).map(_.flatten).flatMap(rs =>
                      if rs.isEmpty then Accepted() else Ok(rs.asJson)
                    )
                  case None => handle(call, p, j).flatMap(_.fold(Accepted())(Ok(_)))
                }
            }
        }
      case _ => unauthorized
    }

  private def mcpMethodNotAllowed(): IO[Response[IO]] =
    MethodNotAllowed(Allow(Method.POST))

  private def getEndpoints(): IO[Response[IO]] =
    Ok(ApiCatalog.all.map(e =>
      Json.obj("method" -> e.method.asJson, "path" -> e.path.asJson, "description" -> e.description.asJson)
    ))
}
