package skotare.api

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.{Authorization, Cookie, Location}
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import skotare.auth.{OidcClient, OidcConfig, SecretBox}
import skotare.db.*
import doobie.implicits.*
import skotare.devidp.DevIdp
import skotare.shared.*

import java.util.UUID
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean

/**
 * OIDC のログインから、監視・障害・通知（stui・skereru）・ステータスページ・MCP までを、
 * 開発用の IdP（DevIdp）・本物の DB（skotare_test）・手元の HTTP サーバー（監視の相手）で通しで確かめる.
 * DB は docker-compose.dev.yml（:54389）. TEST_DATABASE_URL で変えられる.
 */
class FlowSpec extends munit.CatsEffectSuite:

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.noop.NoOpFactory[IO]

  private val dbCfg  = DbConfig(sys.env.getOrElse("TEST_DATABASE_URL", "jdbc:postgresql://localhost:54389/skotare_test"), "skotare", "skotare", 4)
  private val issuer = "http://localhost:9999"
  private val apiCfg = ApiConfig(cookieSecure = false, publicUrl = "http://localhost:8114")

  /** 監視の相手（/health は healthy が true なら 200 "ok"、false なら 503）. */
  private val healthy = new AtomicBoolean(true)
  private val target: HttpRoutes[IO] =
    import org.http4s.dsl.io.*
    HttpRoutes.of[IO] { case GET -> Root / "health" =>
      if healthy.get then Ok("""{"status":"ok"}""") else ServiceUnavailable("maintenance")
    }

  /** 偽の stui（受信 Webhook）と skereru（送信 API）. 受けた要求を残す. */
  private val received = new ConcurrentLinkedQueue[(String, Json, Option[String])]()
  private val fakeSiblings: HttpRoutes[IO] =
    import org.http4s.dsl.io.*
    HttpRoutes.of[IO] { case r @ POST -> path =>
      r.as[Json].flatMap { j =>
        val auth = r.headers.get[Authorization].map(_.credentials.toString)
        IO(received.add((path.renderString, j, auth))) *> Ok(Json.obj("ok" -> true.asJson))
      }
    }

  private final case class Env(idp: HttpApp[IO], app: HttpApp[IO], xa: doobie.Transactor[IO], worker: Worker, monitors: MonitorStore, nodes: NodeStore,
      targetUrl: String)

  /** IdP・監視の相手・アプリを用意する. DB はまっさらにする. */
  private val fixture = ResourceSuiteLocalFixture("app", for
    _  <- Resource.eval(IO.blocking {
            val c = java.sql.DriverManager.getConnection(dbCfg.url, dbCfg.user, dbCfg.password)
            try c.createStatement().execute("drop schema public cascade; create schema public") finally c.close()
          })
    _  <- Resource.eval(Database.migrate(dbCfg))
    xa <- Database.transactor(dbCfg)
    _  <- Resource.eval(new NodeStore(xa).beat("node-a", "default", "test", java.time.Instant.now()))
    srv <- EmberServerBuilder.default[IO].withHost(ipv4"127.0.0.1").withPort(port"0").withShutdownTimeout(scala.concurrent.duration.Duration(1, "s")).withHttpApp(target.orNotFound).build
    idp <- Resource.eval(DevIdp.create(issuer, "skotare", "skotare-test-secret-0123456789"))
    oidc <- Resource.eval(OidcClient.create(OidcConfig(issuer, "skotare", "skotare-test-secret-0123456789", "http://localhost:8114/auth/callback"),
              Client.fromHttpApp(idp.routes.orNotFound)))
  yield
    val users = new UserStore(xa); val monitors = new MonitorStore(xa); val notifiers = new NotifierStore(xa)
    val pages = new PageStore(xa); val nodes = new NodeStore(xa)
    val sess  = new SessionAuth(users, apiCfg)
    val box   = Some(new SecretBox("test-app-secret-0123456789"))
    val senders = new Senders(Client.fromHttpApp(fakeSiblings.orNotFound), box, apiCfg.publicUrl)
    val announcer = new Announcer(None, apiCfg.publicUrl)
    val worker  = new Worker(monitors, notifiers, nodes, senders, announcer, WorkerConfig("node-a", apiCfg.publicUrl, "test"))
    val app = ApiRoutes(Some(oidc), users, sess, apiCfg, new MonitorRoutes(monitors, notifiers, pages, nodes, senders, box, sess, announcer)).orNotFound
    Env(idp.routes.orNotFound, app, xa, worker, monitors, nodes, s"http://127.0.0.1:${srv.address.port.value}/health"))

  override def munitFixtures = List(fixture)

  private def location(r: Response[IO]): Uri = r.headers.get[Location].map(_.uri).getOrElse(fail(s"no Location (${r.status})"))

  /** IdP でログインして、セッションの Cookie を受け取る. */
  private def login(name: String, email: String): IO[String] =
    val e = fixture()
    for
      r1 <- e.app.run(Request[IO](Method.GET, uri"/auth/login?returnTo=/incidents"))
      _   = assertEquals(r1.status, Status.Found)
      auth = location(r1)
      _   = assertEquals(auth.params.get("code_challenge_method"), Some("S256"))
      form = UrlForm(List("client_id", "redirect_uri", "state", "nonce", "code_challenge").map(k => k -> auth.params(k)) ++
               List("name" -> name, "email" -> email)*)
      r2 <- e.idp.run(Request[IO](Method.POST, uri"/authorize").withEntity(form))
      cb  = location(r2)
      // ログインを始めたブラウザの印がなければ断る（ログイン CSRF）
      r3x <- e.app.run(Request[IO](Method.GET, Uri(path = cb.path, query = cb.query)))
      _   = assertEquals(r3x.status, Status.BadRequest)
      mark = r1.cookies.find(_.name == "skotare_login").getOrElse(fail("no login cookie"))
      r3 <- e.app.run(Request[IO](Method.GET, Uri(path = cb.path, query = cb.query)).putHeaders(Cookie(RequestCookie(mark.name, mark.content))))
      _   = assertEquals(r3.status, Status.Found, "callback")
      _   = assertEquals(location(r3).renderString, "/incidents")
    yield r3.cookies.find(_.name == "skotare_session").map(_.content).getOrElse(fail("no session cookie"))

  private def call(token: String, m: Method, path: String, body: Option[Json] = None): IO[Response[IO]] =
    val base = Request[IO](m, Uri.unsafeFromString(path)).putHeaders(Cookie(RequestCookie("skotare_session", token)), Header.Raw(CIString("X-Skotare"), "1"))
    fixture().app.run(body.fold(base)(base.withEntity(_)))

  private def bearer(token: String, m: Method, path: String, body: Option[Json] = None): IO[Response[IO]] =
    val base = Request[IO](m, Uri.unsafeFromString(path)).putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, token)))
    fixture().app.run(body.fold(base)(base.withEntity(_)))

  private def json[A: io.circe.Decoder](r: Response[IO]): IO[A] =
    if r.status.isSuccess then r.as[A] else r.bodyText.compile.string.flatMap(t => IO(fail(s"${r.status}: $t")))

  /** 監視を今すぐ確かめる（ワーカーの1回分）. */
  private def checkNow(id: UUID): IO[Unit] =
    val e = fixture()
    e.monitors.checkSoon(id) *> e.monitors.claimDue("default", 50).flatMap(_.filter(_.id == id).traverse_(e.worker.check))

  private def httpMonitor(name: String, url: String) =
    MonitorReq(name, Kind.Http, MonitorConfig(url = url, bodyContains = "ok"), intervalSec = 60, timeoutMs = 3000, retries = 0)

  test("OIDC でログインし、ログアウトすると IdP のログアウトへ") {
    for
      t   <- login("山田 太郎", "yamada@example.com")
      me  <- call(t, Method.GET, "/api/me").flatMap(json[MeDto])
      _    = assertEquals(me.name, "山田 太郎")
      out <- call(t, Method.POST, "/auth/logout").flatMap(json[Json])
      url  = out.hcursor.get[String]("redirect").getOrElse("")
      _    = assert(url.startsWith(s"$issuer/logout?") && url.contains("id_token_hint="), url)
      gone <- call(t, Method.GET, "/api/me")
      _    = assertEquals(gone.status, Status.Unauthorized)
    yield ()
  }

  test("不正な state・X-Skotare のない更新・未ログイン・入力の誤りは拒否する") {
    val e = fixture()
    for
      r1 <- e.app.run(Request[IO](Method.GET, uri"/auth/callback?code=x&state=unknown"))
      _   = assertEquals(r1.status, Status.BadRequest)
      t  <- login("佐藤", "sato@example.com")
      r2 <- e.app.run(Request[IO](Method.POST, uri"/api/monitors").putHeaders(Cookie(RequestCookie("skotare_session", t)))
              .withEntity(httpMonitor("x", "https://example.com").asJson))
      _   = assertEquals(r2.status, Status.Forbidden)
      r3 <- e.app.run(Request[IO](Method.GET, uri"/api/monitors"))
      _   = assertEquals(r3.status, Status.Unauthorized)
      bad <- List(
               httpMonitor("x", "ftp://example.com"),
               httpMonitor("x", "https://example.com").copy(intervalSec = 5),
               httpMonitor("x", "https://example.com").copy(config = MonitorConfig(url = "https://example.com", expectStatus = "abc")),
               MonitorReq("x", Kind.Tcp, MonitorConfig(host = "example.com", port = 70000)),
               MonitorReq("x", Kind.Ping, MonitorConfig(host = "a b; rm -rf /")),
               MonitorReq("x", "smtp", MonitorConfig())
             ).traverse(r => call(t, Method.POST, "/api/monitors", Some(r.asJson)).map(_.status))
      _   = assertEquals(bad.distinct, List(Status.BadRequest))
    yield ()
  }

  test("停止・復旧で障害を開いて閉じ、stui と skereru に1回ずつ知らせる") {
    val e = fixture()
    healthy.set(true); received.clear()
    for
      t  <- login("運用 担当", "ops@example.com")
      m  <- call(t, Method.POST, "/api/monitors", Some(httpMonitor("会社のサイト", e.targetUrl).asJson)).flatMap(json[MonitorDto])
      _   = assertEquals(m.status, MonStatus.Pending)
      // 試す（保存しない）
      p  <- call(t, Method.POST, "/api/monitors/test", Some(httpMonitor("x", e.targetUrl).asJson)).flatMap(json[ProbeResultDto])
      _   = assert(p.ok, p.message)
      _  <- call(t, Method.POST, "/api/notifiers", Some(NotifierReq("運用チャンネル", NotifierKind.Stui, NotifierConfig(), Some("https://stui.test/hooks/abc")).asJson))
              .flatMap(json[NotifierDto])
      sk <- call(t, Method.POST, "/api/notifiers", Some(NotifierReq("当番のメール", NotifierKind.Skereru,
              NotifierConfig("https://mail.test", "skotare@example.com", List("oncall@example.com")), Some("skr_secret")).asJson)).flatMap(json[NotifierDto])
      _   = assert(sk.hasSecret)
      // 秘密の値は画面に返さない
      ns <- call(t, Method.GET, "/api/notifiers").flatMap(_.bodyText.compile.string)
      _   = assert(!ns.contains("skr_secret") && !ns.contains("hooks/abc"), ns)
      // 送り先（skereru の URL）を変えるときは、API キーを入れ直さないと断る（今のキーを別の送り先へ送らせない）
      mv <- call(t, Method.PUT, s"/api/notifiers/${sk.id}", Some(NotifierReq("当番のメール", NotifierKind.Skereru,
              NotifierConfig("https://evil.test", "skotare@example.com", List("oncall@example.com")), None).asJson))
      _   = assertEquals(mv.status, Status.BadRequest)
      kp <- call(t, Method.PUT, s"/api/notifiers/${sk.id}", Some(NotifierReq("当番のメール（名前だけ変える）", NotifierKind.Skereru,
              NotifierConfig("https://mail.test", "skotare@example.com", List("oncall@example.com")), None).asJson))
      _   = assertEquals(kp.status, Status.Ok)
      // 1回目: 正常（未確認 → 正常は知らせない）
      _  <- checkNow(m.id)
      _  <- e.worker.notifyTick
      d1 <- call(t, Method.GET, s"/api/monitors/${m.id}").flatMap(json[MonitorDetailDto])
      _   = assertEquals(d1.monitor.status, MonStatus.Up)
      _   = assertEquals(received.size, 0)
      // 相手が 503 → 停止（retries = 0）
      _   = healthy.set(false)
      _  <- checkNow(m.id)
      d2 <- call(t, Method.GET, s"/api/monitors/${m.id}").flatMap(json[MonitorDetailDto])
      _   = assertEquals(d2.monitor.status, MonStatus.Down)
      _   = assert(d2.monitor.lastMessage.exists(_.contains("503")), d2.monitor.lastMessage)
      _   = assertEquals(d2.incidents.map(_.resolvedAt.isEmpty), List(true))
      _  <- e.worker.notifyTick
      down = received.toArray.toList.asInstanceOf[List[(String, Json, Option[String])]]
      _   = assertEquals(down.map(_._1).sorted, List("/api/send", "/hooks/abc"))
      stui = down.find(_._1 == "/hooks/abc").get._2
      _   = assert(stui.hcursor.get[String]("text").exists(t => t.contains("停止: 会社のサイト")), stui.noSpaces)
      mail = down.find(_._1 == "/api/send").get
      _   = assertEquals(mail._2.hcursor.get[List[String]]("to").toOption, Some(List("oncall@example.com")))
      _   = assert(mail._3.exists(_.contains("skr_secret")), mail._3)
      // 続けて失敗しても、もう一度は知らせない
      _  <- checkNow(m.id) *> e.worker.notifyTick
      _   = assertEquals(received.size, 2)
      // 復旧
      _   = healthy.set(true)
      _  <- checkNow(m.id) *> e.worker.notifyTick
      d3 <- call(t, Method.GET, s"/api/monitors/${m.id}").flatMap(json[MonitorDetailDto])
      _   = assertEquals(d3.monitor.status, MonStatus.Up)
      _   = assertEquals(d3.incidents.map(_.resolvedAt.isDefined), List(true))
      _   = assertEquals(received.size, 4)
      up  = received.toArray.toList.asInstanceOf[List[(String, Json, Option[String])]].drop(2).find(_._1 == "/hooks/abc").get._2
      _   = assert(up.hcursor.get[String]("text").exists(_.contains("復旧: 会社のサイト")), up.noSpaces)
      _   = assertEquals(d3.checks.map(_.ok), List(true, false, false, true))
      _   = assertEquals(d3.checks.map(_.node).distinct, List("node-a"))
      is <- call(t, Method.GET, "/api/incidents?open=true").flatMap(json[List[IncidentDto]])
      _   = assertEquals(is, Nil)
    yield ()
  }

  test("複数のノードが同時に取っても、同じ監視を2台で確かめない") {
    val e = fixture()
    for
      t   <- login("冗長 構成", "ha@example.com")
      ids <- (1 to 12).toList.traverse(i => call(t, Method.POST, "/api/monitors",
               Some(MonitorReq(s"tcp-$i", Kind.Tcp, MonitorConfig(host = "127.0.0.1", port = 1)).asJson)).flatMap(json[MonitorDto]).map(_.id))
      _   <- sql"update monitor_region set next_check_at = now() - interval '1 minute'".update.run.transact(e.xa)
      got <- List.fill(4)(e.monitors.claimDue("default", 5)).parSequence
      all  = got.flatten.map(_.id).filter(ids.contains)
      _    = assertEquals(all.size, all.distinct.size)
      _    = assertEquals(all.toSet, ids.toSet)
    yield ()
  }

  test("ステータスページはゲストに見せ、監視の中身（URL）は出さない. 公開しないページはログインした人だけ") {
    val e = fixture()
    healthy.set(true)
    for
      t  <- login("広報", "pr@example.com")
      m  <- call(t, Method.POST, "/api/monitors", Some(httpMonitor("秘密の名前", e.targetUrl).asJson)).flatMap(json[MonitorDto])
      _  <- checkNow(m.id)
      _  <- call(t, Method.POST, "/api/pages", Some(StatusPageReq("acme", "ACME の稼働状況", items = List(PageItem(m.id, Some("Web サイト")))).asJson))
              .flatMap(json[StatusPageDto])
      dup <- call(t, Method.POST, "/api/pages", Some(StatusPageReq("acme", "同じ名前").asJson))
      _   = assertEquals(dup.status, Status.Conflict)
      bad <- call(t, Method.POST, "/api/pages", Some(StatusPageReq("Bad Slug!", "x").asJson))
      _   = assertEquals(bad.status, Status.BadRequest)
      r  <- e.app.run(Request[IO](Method.GET, uri"/api/public/status/acme"))
      body <- r.bodyText.compile.string
      _   = assertEquals(r.status, Status.Ok)
      _   = assert(!body.contains("127.0.0.1") && !body.contains("秘密の名前"), body)
      ps <- IO.fromEither(io.circe.parser.decode[PublicStatusDto](body))
      _   = assertEquals(ps.items.map(i => (i.name, i.status)), List(("Web サイト", MonStatus.Up)))
      _   = assertEquals(ps.overall, MonStatus.Up)
      _   = assertEquals(ps.items.head.days.size, 90)
      _  <- call(t, Method.POST, "/api/pages", Some(StatusPageReq("internal", "社内", public = false).asJson)).flatMap(json[StatusPageDto])
      g  <- e.app.run(Request[IO](Method.GET, uri"/api/public/status/internal"))
      _   = assertEquals(g.status, Status.NotFound)
      li <- call(t, Method.GET, "/api/public/status/internal")
      _   = assertEquals(li.status, Status.Ok)
    yield ()
  }

  test("API トークン: 参照のみは変更できない. MCP でツールを呼べる") {
    for
      t   <- login("AI 利用者", "ai@example.com")
      ro  <- call(t, Method.POST, "/api/tokens", Some(ApiTokenReq("参照", TokenScope.Read, None).asJson)).flatMap(json[ApiTokenCreated])
      rw  <- call(t, Method.POST, "/api/tokens", Some(ApiTokenReq("読み書き", TokenScope.Write, Some(30)).asJson)).flatMap(json[ApiTokenCreated])
      l   <- bearer(ro.token, Method.GET, "/api/monitors")
      _    = assertEquals(l.status, Status.Ok)
      w   <- bearer(ro.token, Method.POST, "/api/monitors", Some(httpMonitor("x", "https://example.com").asJson))
      _    = assertEquals(w.status, Status.Forbidden)
      rpc  = (tok: String, method: String, params: Json) =>
               bearer(tok, Method.POST, "/mcp", Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "method" -> method.asJson, "params" -> params)))
                 .flatMap(json[Json])
      tl  <- rpc(ro.token, "tools/list", Json.obj())
      names = tl.hcursor.downField("result").downField("tools").as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[String]("name").toOption)
      _    = assert(names.contains("get_status") && !names.contains("create_monitor"), names)
      cr  <- rpc(rw.token, "tools/call", Json.obj("name" -> "create_monitor".asJson,
               "arguments" -> Json.obj("name" -> "AI が作った監視".asJson, "kind" -> "tcp".asJson, "host" -> "127.0.0.1".asJson, "port" -> 9.asJson, "intervalSec" -> 120.asJson)))
      _    = assertEquals(cr.hcursor.downField("result").get[Boolean]("isError").toOption, Some(false), cr.noSpaces)
      up  <- rpc(rw.token, "tools/call", Json.obj("name" -> "update_monitor".asJson,
               "arguments" -> Json.obj("monitor" -> "AI が作った監視".asJson, "port" -> 10.asJson)))
      _    = assertEquals(up.hcursor.downField("result").get[Boolean]("isError").toOption, Some(false), up.noSpaces)
      ms  <- call(t, Method.GET, "/api/monitors").flatMap(json[List[MonitorDto]])
      ai   = ms.find(_.name == "AI が作った監視").get
      _    = assertEquals((ai.config.port, ai.intervalSec), (10, 120))
      st  <- rpc(ro.token, "tools/call", Json.obj("name" -> "get_status".asJson))
      text = st.hcursor.downField("result").downField("content").downArray.get[String]("text").getOrElse("")
      _    = assert(text.contains("正常") && text.contains("ノード"), text)
    yield ()
  }

  test("複数の拠点: 1つの拠点だけが届かなければ skotare 側（拠点）の異常とし、監視先の停止は過半数の拠点で判断する") {
    val e = fixture()
    val now = java.time.Instant.now()
    val ok = skotare.core.ProbeResult(true, Some(5), "応答コード 200")
    def ng(m: String) = skotare.core.ProbeResult(false, None, m)
    def rec(id: UUID, region: String, r: skotare.core.ProbeResult) =
      e.monitors.record(id, region, r, s"node-$region", java.time.Instant.now(), apiCfg.publicUrl)
    def health(region: String) = e.nodes.regions.map(_.find(_.name == region).map(_.health))
    def status(id: UUID) = e.monitors.find(id).map(_.get.status)
    def outbox(event: String) = sql"select count(*) from notification_outbox where payload->>'event' = $event".query[Int].unique.transact(e.xa)
    for
      t   <- login("拠点 担当", "region@example.com")
      _   <- List("tokyo", "osaka", "nagoya").traverse_(r => e.nodes.beat(s"node-$r", r, "test", now))
      ms  <- (1 to 4).toList.traverse(i => call(t, Method.POST, "/api/monitors",
               Some(MonitorReq(s"拠点-$i", Kind.Tcp, MonitorConfig(host = "127.0.0.1", port = 1), retries = 0).asJson)).flatMap(json[MonitorDto]).map(_.id))
      ids = ms
      // どの拠点からも正常
      _   <- ids.traverse_(id => List("tokyo", "osaka", "nagoya").traverse_(r => rec(id, r, ok)))
      _   <- ids.traverse(status).map(st => assertEquals(st.distinct, List(MonStatus.Up)))
      // 大阪からだけ届かない → 大阪は停止と見るが、過半数（2/3）に届かないので監視は正常のまま
      _   <- ids.traverse_(id => rec(id, "osaka", ng("接続できません（タイムアウト）")))
      _   <- ids.traverse(status).map(st => assertEquals(st.distinct, List(MonStatus.Up)))
      // 拠点の判定: 大阪だけが、ほかの拠点では正常な監視先に届かない → 大阪を疑う（skotare 側の異常）
      _   <- e.worker.judgeTick
      _   <- health("osaka").map(h => assertEquals(h, Some(RegionHealth.Suspect)))
      _   <- health("tokyo").map(h => assertEquals(h, Some(RegionHealth.Ok)))
      s1  <- call(t, Method.GET, "/api/summary").flatMap(json[SummaryDto])
      _    = assert(s1.regions.find(_.name == "osaka").exists(_.reason.contains("この拠点だけ")), s1.regions)
      m1  <- e.monitors.find(ids.head).map(_.get)
      _    = assertEquals(m1.regions.find(_.region == "osaka").map(r => (r.status, r.counted)), Some((MonStatus.Down, false)))
      _   <- outbox("region.suspect").map(n => assert(n > 0, "拠点の異常を知らせる"))
      _   <- sql"select count(*) from notification_outbox where payload->>'event' = 'down' and payload->'monitor'->>'name' like '拠点-%'"
               .query[Int].unique.transact(e.xa).map(n => assertEquals(n, 0, "監視先の停止とは知らせない"))
      // 監視先が本当に止まった: 健全な東京・名古屋の両方から届かない → 停止（疑わしい大阪の票は数えない）
      _   <- rec(ids.head, "tokyo", ng("接続を拒否されました"))
      _   <- status(ids.head).map(s => assertEquals(s, MonStatus.Up, "東京だけでは過半数に届かない"))
      t1  <- rec(ids.head, "nagoya", ng("接続を拒否されました"))
      _    = assertEquals(t1.map(_.to), Some(MonStatus.Down))
      _    = assert(t1.exists(x => x.message.contains("tokyo:") && x.message.contains("nagoya:") && !x.message.contains("osaka")), t1)
      // 大阪が直った（ほかの監視先にふたたび届く）→ 大阪を健全に戻す
      _   <- ids.tail.traverse_(id => rec(id, "osaka", ok))
      _   <- e.worker.judgeTick
      _   <- health("osaka").map(h => assertEquals(h, Some(RegionHealth.Ok)))
      _   <- outbox("region.ok").map(n => assert(n > 0))
      _   <- status(ids.head).map(s => assertEquals(s, MonStatus.Down))
      // 拠点が止まった（心拍が途絶えた）→ 停止にし、その拠点の状態を未確認に戻す（戻ったときに古い票で判定しない）
      _   <- sql"update node set seen_at = now() - interval '10 minutes' where region = 'nagoya'".update.run.transact(e.xa)
      _   <- e.worker.judgeTick
      _   <- health("nagoya").map(h => assertEquals(h, Some(RegionHealth.Offline)))
      _   <- outbox("region.offline").map(n => assert(n > 0))
      _   <- sql"select count(*) from monitor_region where region = 'nagoya' and status <> 'pending'".query[Int].unique.transact(e.xa)
               .map(n => assertEquals(n, 0))
      _   <- status(ids.head).map(s => assertEquals(s, MonStatus.Down, "残りの拠点（東京・大阪）がどちらも停止と見ている"))
      // 日ごとの集計: 判定の決まった確認だけを数える（この監視はずっと正常）
      d   <- sql"select coalesce(sum(total_n), 0)::int, coalesce(sum(up_n), 0)::int from check_daily where monitor_id = ${ids(1).toString}::uuid"
               .query[(Int, Int)].unique.transact(e.xa)
      _    = assert(d._1 > 0 && d._1 == d._2, d)
      m2  <- e.monitors.find(ids(1)).map(_.get)
      _    = assertEquals(m2.uptime30d, Some(100.0))
      // 停止中に設定を変えると、障害を閉じて closed を知らせる（復旧とは知らせない）
      m0  <- e.monitors.find(ids.head).map(_.get)
      r0  <- call(t, Method.PUT, s"/api/monitors/${ids.head}", Some(m0.toReq.copy(config = m0.config.copy(port = 2)).asJson)).flatMap(json[MonitorDto])
      _    = assertEquals(r0.status, MonStatus.Pending)
      _    = assertEquals(r0.lastMessage, None)
      cl  <- sql"select payload::text from notification_outbox where payload->>'event' = 'closed' and payload->'monitor'->>'id' = ${ids.head.toString}"
               .query[String].to[List].transact(e.xa).map(_.flatMap(io.circe.parser.parse(_).toOption))
      _    = assert(cl.nonEmpty, "通知先ごとに1行")
      _    = assert(Messages.subject(cl.head).contains("障害を閉じました"), Messages.subject(cl.head))
      // 基準点: 東京から基準点に届かなければ、東京を疑う
      c   <- call(t, Method.POST, "/api/monitors", Some(MonitorReq("基準点", Kind.Tcp, MonitorConfig(host = "127.0.0.1", port = 1), retries = 0,
               canary = true).asJson)).flatMap(json[MonitorDto])
      _    = assert(c.canary)
      _   <- rec(c.id, "tokyo", ng("名前を引けません"))
      _   <- e.worker.judgeTick
      _   <- e.nodes.regions.map(rs => assert(rs.find(_.name == "tokyo").exists(r => r.health == RegionHealth.Suspect && r.reason.contains("基準点")), rs))
      // 拠点の通知の文面
      _    = assert(Messages.subject(io.circe.parser.parse("""{"event":"region.suspect","region":{"name":"tokyo"}}""").toOption.get).contains("拠点の異常の疑い: tokyo"))
      // 後片づけ（ほかのテストに拠点を残さない）
      _   <- sql"delete from monitor where name like '拠点-%' or name = '基準点'".update.run.transact(e.xa)
      _   <- sql"delete from region where name in ('tokyo', 'osaka', 'nagoya')".update.run.transact(e.xa)
      _   <- sql"delete from node where region in ('tokyo', 'osaka', 'nagoya')".update.run.transact(e.xa)
    yield ()
  }
