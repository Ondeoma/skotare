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
import org.http4s.headers.{`WWW-Authenticate`, Authorization, Cookie, Location}
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import sbirds.auth.{OidcClient, OidcConfig}
import sbirds.links.IntegrationTokenStore
import skotare.auth.SecretBox
import skotare.db.*
import doobie.implicits.*
import sbirds.devidp.DevIdp
import skotare.shared.*

import java.util.UUID
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean

/**
 * OIDC のログインから、監視・障害・通知（stui・skereru）・ステータスページ・MCP までを、
 * 開発用の IdP（DevIdp）・本物の DB（skotare_test）・手元の HTTP サーバー（監視の相手）で通しで確かめる.
 * DB は docker-compose.dev.yml（:54389）. TEST_DATABASE_URL で変えられる.
 */
class FlowSpec extends munit.CatsEffectSuite {

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.noop.NoOpFactory[IO]

  private val dbCfg = DbConfig(
    sys.env.getOrElse("TEST_DATABASE_URL", "jdbc:postgresql://localhost:54389/skotare_test"),
    "skotare",
    "skotare",
    4
  )
  private val issuer = "http://localhost:9999"
  private val agentCb = "http://localhost:5199/api/connections/callback"
  private val apiCfg = ApiConfig(
    cookieSecure = false,
    publicUrl = "http://localhost:8114",
    oauthClients = List(
      OAuthClient("skakapo", "skakapo-test-secret-0123456789", agentCb, "skakapo", List("read", "write")),
      OAuthClient("viewer", "viewer-test-secret-0123456789", "https://viewer.example.com/cb", "閲覧アプリ", List("read"))
    )
  )

  /** 監視の相手（/health は healthy が true なら 200 "ok"、false なら 503）. */
  private val healthy = new AtomicBoolean(true)
  private val target: HttpRoutes[IO] = {
    import org.http4s.dsl.io.*
    HttpRoutes.of[IO] { case GET -> Root / "health" =>
      if healthy.get then Ok("""{"status":"ok"}""") else ServiceUnavailable("maintenance")
    }
  }

  /** 偽の stui のチャンネル（ops は参加中で管理者、dev は参加中、old はアーカイブ済み、DM は選べない）. */
  private val opsId = UUID.fromString("00000000-0000-0000-0000-0000000000a1")
  private val devId = UUID.fromString("00000000-0000-0000-0000-0000000000a3")
  private val hookId = UUID.fromString("00000000-0000-0000-0000-0000000000b1")
  private val oldId = UUID.fromString("00000000-0000-0000-0000-0000000000a2")
  private val stuiToken = "stui-token-1"

  /** 偽の stui（受信 Webhook・OAuth・チャンネルの一覧・連携サービスからの通知）と skereru（送信 API）. 受けた要求を残す. */
  private val received = new ConcurrentLinkedQueue[(String, Json, Option[String])]()
  private val fakeSiblings: HttpRoutes[IO] = {
    import org.http4s.dsl.io.*
    def authed(
      r: Request[IO]
    ) = r.headers.get[Authorization].exists(_.credentials == Credentials.Token(AuthScheme.Bearer, stuiToken))
    HttpRoutes.of[IO] {
      case r @ POST -> Root / "oauth" / "token" =>
        r.as[UrlForm].flatMap(f =>
          if f.getFirst("code").contains("good-code") && f.getFirst("code_verifier").isDefined
          then Ok(Json.obj("access_token" -> stuiToken.asJson))
          else BadRequest(Json.obj("error" -> "invalid_grant".asJson))
        )
      case POST -> Root / "oauth" / "revoke" => Ok()
      case r @ GET -> Root / "api" / "channels" =>
        if !authed(r) then Unauthorized(`WWW-Authenticate`(Challenge("Bearer", "stui")))
        else
          Ok(Json.arr(
            Json.obj(
              "id" -> opsId.asJson,
              "name" -> "ops".asJson,
              "kind" -> "public".asJson,
              "archived" -> false.asJson,
              "member" -> true.asJson,
              "role" -> "admin".asJson
            ),
            Json.obj(
              "id" -> devId.asJson,
              "name" -> "dev".asJson,
              "kind" -> "public".asJson,
              "archived" -> false.asJson,
              "member" -> true.asJson,
              "role" -> "member".asJson
            ),
            Json.obj(
              "id" -> oldId.asJson,
              "name" -> "old".asJson,
              "kind" -> "private".asJson,
              "archived" -> true.asJson,
              "member" -> true.asJson
            ),
            Json.obj(
              "id" -> UUID.randomUUID().asJson,
              "name" -> "dm".asJson,
              "kind" -> "dm".asJson,
              "archived" -> false.asJson,
              "member" -> true.asJson
            )
          ))
      // 受信 Webhook を作る（URL は利用者向けの公開 URL で返す）・消す
      case r @ POST -> Root / "api" / "hooks" / "incoming" =>
        r.as[Json].flatMap(j =>
          if !authed(r) then Unauthorized(`WWW-Authenticate`(Challenge("Bearer", "stui")))
          else if j.hcursor.get[UUID]("channelId").contains(opsId) then
            IO(received.add(("/api/hooks/incoming", j, None))) *> Created(Json.obj(
              "hook" -> Json.obj("id" -> hookId.asJson),
              "url" -> "https://stui.test/hooks/tok-1".asJson
            ))
          else Forbidden(Json.obj("error" -> "チャンネルの管理者だけが変えられます".asJson))
        )
      case r @ DELETE -> path =>
        IO(received.add((
          s"DELETE ${path.renderString}",
          Json.Null,
          r.headers.get[Authorization].map(_.credentials.toString)
        ))) *> NoContent()
      case r @ POST -> path =>
        r.as[Json].flatMap { j =>
          val auth = r.headers.get[Authorization].map(_.credentials.toString)
          IO(received.add((path.renderString, j, auth))) *> Ok(Json.obj("ok" -> true.asJson))
        }
    }
  }

  private final case class Env(
    idp: HttpApp[IO],
    app: HttpApp[IO],
    xa: doobie.Transactor[IO],
    worker: Worker,
    monitors: MonitorStore,
    nodes: NodeStore,
    targetUrl: String)

  /** IdP・監視の相手・アプリを用意する. DB はまっさらにする. */
  private val fixture = ResourceSuiteLocalFixture(
    "app",
    for {
      _ <- Resource.eval(IO.blocking {
        val c = java.sql.DriverManager.getConnection(dbCfg.url, dbCfg.user, dbCfg.password)
        try c.createStatement().execute("drop schema public cascade; create schema public")
        finally c.close()
      })
      _ <- Resource.eval(Database.migrate(dbCfg))
      xa <- Database.transactor(dbCfg)
      _ <- Resource.eval(new NodeStore(xa).beat("node-a", "default", "test", java.time.Instant.now()))
      srv <- EmberServerBuilder.default[IO].withHost(ipv4"127.0.0.1").withPort(
        port"0"
      ).withShutdownTimeout(scala.concurrent.duration.Duration(1, "s")).withHttpApp(target.orNotFound).build
      idp <- Resource.eval(DevIdp.create(issuer, "skotare", "skotare-test-secret-0123456789"))
      oidc <- Resource.eval(OidcClient.create(
        OidcConfig(issuer, "skotare", "skotare-test-secret-0123456789", "http://localhost:8114/auth/callback"),
        Client.fromHttpApp(idp.routes.orNotFound)
      ))
    } yield {
      val users = new UserStore(xa); val monitors = new MonitorStore(xa); val notifiers = new NotifierStore(xa)
      val pages = new PageStore(xa); val nodes = new NodeStore(xa)
      val sess = new SessionAuth(users, apiCfg)
      val box = Some(SecretBox("test-app-secret-0123456789"))
      val siblings = Client.fromHttpApp(fakeSiblings.orNotFound)
      val stui = new StuiLink(
        Some(LinkService("stui", "https://stui.test", "skotare", "skotare-test-secret-0123456789")),
        new IntegrationTokenStore(xa),
        siblings,
        box,
        apiCfg.publicUrl,
        sess
      )
      val senders = new Senders(siblings, box, apiCfg.publicUrl, Some(stui))
      val announcer = new Announcer(None, apiCfg.publicUrl)
      val worker =
        new Worker(monitors, notifiers, nodes, senders, announcer, WorkerConfig("node-a", apiCfg.publicUrl, "test"))
      val app = ApiRoutes(
        Some(oidc),
        users,
        sess,
        apiCfg,
        new MonitorRoutes(monitors, notifiers, pages, nodes, senders, box, sess, announcer, Some(stui)),
        extra = stui.routes
      ).orNotFound
      Env(idp.routes.orNotFound, app, xa, worker, monitors, nodes, s"http://127.0.0.1:${srv.address.port.value}/health")
    }
  )

  override def munitFixtures = List(fixture)

  private def location(
    r: Response[IO]
  ): Uri = r.headers.get[Location].map(_.uri).getOrElse(fail(s"no Location (${r.status})"))

  /** IdP でログインして、セッションの Cookie を受け取る. */
  private def login(
    name: String,
    email: String
  ): IO[String] = {
    val e = fixture()
    for {
      r1 <- e.app.run(Request[IO](Method.GET, uri"/auth/login?returnTo=/incidents"))
      _ = assertEquals(r1.status, Status.Found)
      auth = location(r1)
      _ = assertEquals(auth.params.get("code_challenge_method"), Some("S256"))
      form =
        UrlForm(List("client_id", "redirect_uri", "state", "nonce", "code_challenge").map(k => k -> auth.params(k)) ++
          List("name" -> name, "email" -> email)*)
      r2 <- e.idp.run(Request[IO](Method.POST, uri"/authorize").withEntity(form))
      cb = location(r2)
      // ログインを始めたブラウザの印がなければ断る（ログイン CSRF）
      r3x <- e.app.run(Request[IO](Method.GET, Uri(path = cb.path, query = cb.query)))
      _ = assertEquals(r3x.status, Status.BadRequest)
      mark = r1.cookies.find(_.name == "skotare_login").getOrElse(fail("no login cookie"))
      r3 <- e.app.run(Request[IO](
        Method.GET,
        Uri(path = cb.path, query = cb.query)
      ).putHeaders(Cookie(RequestCookie(mark.name, mark.content))))
      _ = assertEquals(r3.status, Status.Found, "callback")
      _ = assertEquals(location(r3).renderString, "/incidents")
    } yield r3.cookies.find(_.name == "skotare_session").map(_.content).getOrElse(fail("no session cookie"))
  }

  private def call(
    token: String,
    m: Method,
    path: String,
    body: Option[Json] = None
  ): IO[Response[IO]] = {
    val base = Request[IO](
      m,
      Uri.unsafeFromString(path)
    ).putHeaders(Cookie(RequestCookie("skotare_session", token)), Header.Raw(CIString("X-Skotare"), "1"))
    fixture().app.run(body.fold(base)(base.withEntity(_)))
  }

  private def bearer(
    token: String,
    m: Method,
    path: String,
    body: Option[Json] = None
  ): IO[Response[IO]] = {
    val base =
      Request[IO](m, Uri.unsafeFromString(path)).putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, token)))
    fixture().app.run(body.fold(base)(base.withEntity(_)))
  }

  private def json[A: io.circe.Decoder](
    r: Response[IO]
  ): IO[A] =
    if r.status.isSuccess then r.as[A] else r.bodyText.compile.string.flatMap(t => IO(fail(s"${r.status}: $t")))

  /** 監視を今すぐ確かめる（ワーカーの1回分）. */
  private def checkNow(
    id: UUID
  ): IO[Unit] = {
    val e = fixture()
    e.monitors.checkSoon(id) *>
      e.monitors.claimDue("default", 50).flatMap(_.filter(_.id == id).traverse_(e.worker.check))
  }

  private def httpMonitor(
    name: String,
    url: String
  ) =
    MonitorReq(
      name,
      Kind.Http,
      MonitorConfig(url = url, bodyContains = "ok"),
      intervalSec = 60,
      timeoutMs = 3000,
      retries = 0
    )

  test("OIDC でログインし、ログアウトすると IdP のログアウトへ") {
    for {
      t <- login("山田 太郎", "yamada@example.com")
      me <- call(t, Method.GET, "/api/me").flatMap(json[MeDto])
      _ = assertEquals(me.name, "山田 太郎")
      out <- call(t, Method.POST, "/auth/logout").flatMap(json[Json])
      url = out.hcursor.get[String]("redirect").getOrElse("")
      _ = assert(url.startsWith(s"$issuer/logout?") && url.contains("id_token_hint="), url)
      gone <- call(t, Method.GET, "/api/me")
      _ = assertEquals(gone.status, Status.Unauthorized)
    } yield ()
  }

  test("不正な state・X-Skotare のない更新・未ログイン・入力の誤りは拒否する") {
    val e = fixture()
    for {
      r1 <- e.app.run(Request[IO](Method.GET, uri"/auth/callback?code=x&state=unknown"))
      _ = assertEquals(r1.status, Status.BadRequest)
      t <- login("佐藤", "sato@example.com")
      r2 <-
        e.app.run(Request[IO](Method.POST, uri"/api/monitors").putHeaders(Cookie(RequestCookie("skotare_session", t)))
          .withEntity(httpMonitor("x", "https://example.com").asJson))
      _ = assertEquals(r2.status, Status.Forbidden)
      r3 <- e.app.run(Request[IO](Method.GET, uri"/api/monitors"))
      _ = assertEquals(r3.status, Status.Unauthorized)
      bad <- List(
        httpMonitor("x", "ftp://example.com"),
        httpMonitor("x", "https://example.com").copy(intervalSec = 5),
        httpMonitor("x", "https://example.com").copy(config =
          MonitorConfig(url = "https://example.com", expectStatus = "abc")
        ),
        MonitorReq("x", Kind.Tcp, MonitorConfig(host = "example.com", port = 70000)),
        MonitorReq("x", Kind.Ping, MonitorConfig(host = "a b; rm -rf /")),
        MonitorReq("x", "smtp", MonitorConfig())
      ).traverse(r => call(t, Method.POST, "/api/monitors", Some(r.asJson)).map(_.status))
      _ = assertEquals(bad.distinct, List(Status.BadRequest))
    } yield ()
  }

  test("停止・復旧で障害を開いて閉じ、stui と skereru に1回ずつ知らせる") {
    val e = fixture()
    healthy.set(true); received.clear()
    for {
      t <- login("運用 担当", "ops@example.com")
      m <-
        call(t, Method.POST, "/api/monitors", Some(httpMonitor("会社のサイト", e.targetUrl).asJson)).flatMap(json[MonitorDto])
      _ = assertEquals(m.status, MonStatus.Pending)
      // 試す（保存しない）
      p <- call(
        t,
        Method.POST,
        "/api/monitors/test",
        Some(httpMonitor("x", e.targetUrl).asJson)
      ).flatMap(json[ProbeResultDto])
      _ = assert(p.ok, p.message)
      _ <- call(
        t,
        Method.POST,
        "/api/notifiers",
        Some(NotifierReq("運用チャンネル", NotifierKind.Stui, NotifierConfig(), Some("https://stui.test/hooks/abc")).asJson)
      )
        .flatMap(json[NotifierDto])
      sk <- call(
        t,
        Method.POST,
        "/api/notifiers",
        Some(NotifierReq(
          "当番のメール",
          NotifierKind.Skereru,
          NotifierConfig("https://mail.test", "skotare@example.com", List("oncall@example.com")),
          Some("skr_secret")
        ).asJson)
      ).flatMap(json[NotifierDto])
      _ = assert(sk.hasSecret)
      // 秘密の値は画面に返さない
      ns <- call(t, Method.GET, "/api/notifiers").flatMap(_.bodyText.compile.string)
      _ = assert(!ns.contains("skr_secret") && !ns.contains("hooks/abc"), ns)
      // 送り先（skereru の URL）を変えるときは、API キーを入れ直さないと断る（今のキーを別の送り先へ送らせない）
      mv <- call(
        t,
        Method.PUT,
        s"/api/notifiers/${sk.id}",
        Some(NotifierReq(
          "当番のメール",
          NotifierKind.Skereru,
          NotifierConfig("https://evil.test", "skotare@example.com", List("oncall@example.com")),
          None
        ).asJson)
      )
      _ = assertEquals(mv.status, Status.BadRequest)
      kp <- call(
        t,
        Method.PUT,
        s"/api/notifiers/${sk.id}",
        Some(NotifierReq(
          "当番のメール（名前だけ変える）",
          NotifierKind.Skereru,
          NotifierConfig("https://mail.test", "skotare@example.com", List("oncall@example.com")),
          None
        ).asJson)
      )
      _ = assertEquals(kp.status, Status.Ok)
      // 1回目: 正常（未確認 → 正常は知らせない）
      _ <- checkNow(m.id)
      _ <- e.worker.notifyTick
      d1 <- call(t, Method.GET, s"/api/monitors/${m.id}").flatMap(json[MonitorDetailDto])
      _ = assertEquals(d1.monitor.status, MonStatus.Up)
      _ = assertEquals(received.size, 0)
      // 相手が 503 → 停止（retries = 0）
      _ = healthy.set(false)
      _ <- checkNow(m.id)
      d2 <- call(t, Method.GET, s"/api/monitors/${m.id}").flatMap(json[MonitorDetailDto])
      _ = assertEquals(d2.monitor.status, MonStatus.Down)
      _ = assert(d2.monitor.lastMessage.exists(_.contains("503")), d2.monitor.lastMessage)
      _ = assertEquals(d2.incidents.map(_.resolvedAt.isEmpty), List(true))
      _ <- e.worker.notifyTick
      down = received.toArray.toList.asInstanceOf[List[(String, Json, Option[String])]]
      _ = assertEquals(down.map(_._1).sorted, List("/api/send", "/hooks/abc"))
      stui = down.find(_._1 == "/hooks/abc").get._2
      _ = assert(stui.hcursor.get[String]("text").exists(t => t.contains("停止: 会社のサイト")), stui.noSpaces)
      mail = down.find(_._1 == "/api/send").get
      _ = assertEquals(mail._2.hcursor.get[List[String]]("to").toOption, Some(List("oncall@example.com")))
      _ = assert(mail._3.exists(_.contains("skr_secret")), mail._3)
      // 続けて失敗しても、もう一度は知らせない
      _ <- checkNow(m.id) *> e.worker.notifyTick
      _ = assertEquals(received.size, 2)
      // 復旧
      _ = healthy.set(true)
      _ <- checkNow(m.id) *> e.worker.notifyTick
      d3 <- call(t, Method.GET, s"/api/monitors/${m.id}").flatMap(json[MonitorDetailDto])
      _ = assertEquals(d3.monitor.status, MonStatus.Up)
      _ = assertEquals(d3.incidents.map(_.resolvedAt.isDefined), List(true))
      _ = assertEquals(received.size, 4)
      up = received.toArray.toList.asInstanceOf[List[(String, Json, Option[String])]].drop(2).find(_._1 ==
        "/hooks/abc").get._2
      _ = assert(up.hcursor.get[String]("text").exists(_.contains("復旧: 会社のサイト")), up.noSpaces)
      _ = assertEquals(d3.checks.map(_.ok), List(true, false, false, true))
      _ = assertEquals(d3.checks.map(_.node).distinct, List("node-a"))
      is <- call(t, Method.GET, "/api/incidents?open=true").flatMap(json[List[IncidentDto]])
      _ = assertEquals(is, Nil)
    } yield ()
  }

  test("stui と連携し、参加しているチャンネルを通知先に選ぶと、作った人のトークンでボットとして投稿する") {
    def start(
      t: String
    ) =
      call(t, Method.GET, "/api/integrations/stui/connect?returnTo=/notifiers").map { r =>
        assertEquals(r.status, Status.Found)
        val u = location(r)
        assertEquals(u.renderString.takeWhile(_ != '?'), "https://stui.test/oauth/authorize")
        assertEquals(u.params.get("code_challenge_method"), Some("S256"))
        assertEquals(u.params.get("redirect_uri"), Some("http://localhost:8114/api/integrations/stui/callback"))
        u.params("state")
      }
    def back(
      t: String,
      state: String
    ) =
      call(t, Method.GET, s"/api/integrations/stui/callback?state=$state&code=good-code").map(location(_).renderString)
    val notifier = (ch: UUID) => NotifierReq("", NotifierKind.Stui, NotifierConfig(channelId = Some(ch)), None)
    for {
      a <- login("連携 太郎", "renkei@example.com")
      b <- login("ほか 花子", "hoka@example.com")
      is <- call(a, Method.GET, "/api/integrations").flatMap(json[List[IntegrationDto]])
      _ = assertEquals(is.map(i => i.provider -> i.connected), List("stui" -> false))
      // 連携していなければ、チャンネルは選べない
      n0 <- call(a, Method.GET, "/api/integrations/stui/channels")
      _ = assertEquals(n0.status, Status.Conflict)
      // ほかの人が始めた連携を、自分のアカウントに結び付けられない
      s0 <- start(a)
      x <- back(b, s0)
      _ = assertEquals(x, "/notifiers?integration=stui&result=error")
      s1 <- start(a)
      ok <- back(a, s1)
      _ = assertEquals(ok, "/notifiers?integration=stui&result=connected")
      // state は一度だけ
      again <- back(a, s1)
      _ = assertEquals(again, "/notifiers?integration=stui&result=expired")
      cs <- call(a, Method.GET, "/api/integrations/stui/channels").flatMap(json[List[StuiChannelDto]])
      _ = assertEquals(cs.map(c => c.name -> c.admin), List("dev" -> false, "ops" -> true))
      // 選べないチャンネル（アーカイブ済み）は断る
      bad <- call(a, Method.POST, "/api/notifiers", Some(notifier(oldId).copy(name = "古い").asJson))
      _ = assertEquals(bad.status, Status.BadRequest)
      n <- call(
        a,
        Method.POST,
        "/api/notifiers",
        Some(notifier(opsId).copy(name = "運用", secret = Some("https://ignored.test/x")).asJson)
      )
        .flatMap(json[NotifierDto])
      me <- call(a, Method.GET, "/api/me").flatMap(json[MeDto])
      _ = assertEquals(n.config.channelName, "ops")
      _ = assertEquals(n.config.connectedBy, Some(me.id))
      _ = assert(!n.hasSecret, "チャンネルを選んだら秘密の値は使わない")
      _ <- IO(received.clear())
      t <- call(b, Method.POST, s"/api/notifiers/${n.id}/test")
      _ = assertEquals(t.status, Status.NoContent)
      sent = received.toArray.toList.asInstanceOf[List[(String, Json, Option[String])]]
      _ = assertEquals(sent.map(_._1), List("/api/apps/notify"))
      _ = assertEquals(sent.head._2.hcursor.get[String]("channel"), Right(opsId.toString))
      _ = assert(sent.head._2.hcursor.get[String]("text").exists(_.contains("テスト")), sent.head._2.noSpaces)
      _ = assertEquals(sent.head._3, Some(s"Bearer $stuiToken"))
      // チャンネルの管理者なら、受信 Webhook を作ってその URL で送れる（管理者でないチャンネルには作れない）
      nh <- call(a, Method.POST, "/api/notifiers", Some(notifier(devId).copy(name = "開発", hook = true).asJson))
      _ = assertEquals(nh.status, Status.BadRequest)
      _ <- IO(received.clear())
      h <- call(
        a,
        Method.POST,
        "/api/notifiers",
        Some(notifier(opsId).copy(name = "運用（Webhook）", hook = true).asJson)
      ).flatMap(json[NotifierDto])
      _ = assertEquals(
        (h.config.hookId, h.config.channelName, h.config.connectedBy, h.hasSecret),
        (Some(hookId), "ops", None, true)
      )
      ht <- call(a, Method.POST, s"/api/notifiers/${h.id}/test")
      _ = assertEquals(ht.status, Status.NoContent)
      hs = received.toArray.toList.asInstanceOf[List[(String, Json, Option[String])]]
      _ = assertEquals(hs.map(_._1), List("/api/hooks/incoming", "/hooks/tok-1"))
      _ = assertEquals(hs.last._2.hcursor.get[String]("username"), Right("skotare"))
      // 同じチャンネルのまま変えても作り直さない. トークンでの投稿に替えると、作った Webhook を消す
      _ <- IO(received.clear())
      h2 <- call(
        a,
        Method.PUT,
        s"/api/notifiers/${h.id}",
        Some(notifier(opsId).copy(name = "運用（Webhook・改）", hook = true).asJson)
      ).flatMap(json[NotifierDto])
      _ = assertEquals(h2.config.hookId, Some(hookId))
      _ = assertEquals(received.size, 0)
      h3 <- call(
        a,
        Method.PUT,
        s"/api/notifiers/${h.id}",
        Some(notifier(opsId).copy(name = "運用（トークン）").asJson)
      ).flatMap(json[NotifierDto])
      _ = assertEquals((h3.config.hookId, h3.hasSecret), (None, false))
      _ = assertEquals(
        received.toArray.toList.asInstanceOf[List[(String, Json, Option[String])]].map(_._1),
        List(s"DELETE /api/hooks/incoming/$hookId")
      )
      _ <- call(a, Method.DELETE, s"/api/notifiers/${h.id}")
      // 連携していない人も、同じチャンネルのまま監視の選択などは変えられる（投稿する人は変わらない）
      u1 <- call(
        b,
        Method.PUT,
        s"/api/notifiers/${n.id}",
        Some(notifier(opsId).copy(name = "運用（停止だけ）").asJson)
      ).flatMap(json[NotifierDto])
      _ = assertEquals(u1.config.connectedBy, Some(me.id))
      // 作った人が連携をやめると、ほかの人は、自分が連携していなければ選び直せない
      d <- call(a, Method.DELETE, "/api/integrations/stui/connection")
      _ = assertEquals(d.status, Status.NoContent)
      u2 <- call(b, Method.PUT, s"/api/notifiers/${n.id}", Some(notifier(opsId).copy(name = "運用").asJson))
      _ = assertEquals(u2.status, Status.BadRequest)
      // ほかの人が連携すれば、選び直してその人の連携で投稿する
      s2 <- start(b)
      _ <- back(b, s2)
      u3 <- call(
        b,
        Method.PUT,
        s"/api/notifiers/${n.id}",
        Some(notifier(opsId).copy(name = "運用").asJson)
      ).flatMap(json[NotifierDto])
      meB <- call(b, Method.GET, "/api/me").flatMap(json[MeDto])
      _ = assertEquals(u3.config.connectedBy, Some(meB.id))
      _ <- call(b, Method.DELETE, s"/api/notifiers/${n.id}")
    } yield ()
  }

  test("複数のノードが同時に取っても、同じ監視を2台で確かめない") {
    val e = fixture()
    for {
      t <- login("冗長 構成", "ha@example.com")
      ids <- (1 to 12).toList.traverse(i =>
        call(
          t,
          Method.POST,
          "/api/monitors",
          Some(MonitorReq(s"tcp-$i", Kind.Tcp, MonitorConfig(host = "127.0.0.1", port = 1)).asJson)
        ).flatMap(json[MonitorDto]).map(_.id)
      )
      _ <- sql"update monitor_region set next_check_at = now() - interval '1 minute'".update.run.transact(e.xa)
      got <- List.fill(4)(e.monitors.claimDue("default", 5)).parSequence
      all = got.flatten.map(_.id).filter(ids.contains)
      _ = assertEquals(all.size, all.distinct.size)
      _ = assertEquals(all.toSet, ids.toSet)
    } yield ()
  }

  test("ステータスページはゲストに見せ、監視の中身（URL）は出さない. 公開しないページはログインした人だけ") {
    val e = fixture()
    healthy.set(true)
    for {
      t <- login("広報", "pr@example.com")
      m <-
        call(t, Method.POST, "/api/monitors", Some(httpMonitor("秘密の名前", e.targetUrl).asJson)).flatMap(json[MonitorDto])
      _ <- checkNow(m.id)
      _ <- call(
        t,
        Method.POST,
        "/api/pages",
        Some(StatusPageReq("acme", "ACME の稼働状況", items = List(PageItem(m.id, Some("Web サイト")))).asJson)
      )
        .flatMap(json[StatusPageDto])
      dup <- call(t, Method.POST, "/api/pages", Some(StatusPageReq("acme", "同じ名前").asJson))
      _ = assertEquals(dup.status, Status.Conflict)
      bad <- call(t, Method.POST, "/api/pages", Some(StatusPageReq("Bad Slug!", "x").asJson))
      _ = assertEquals(bad.status, Status.BadRequest)
      r <- e.app.run(Request[IO](Method.GET, uri"/api/public/status/acme"))
      body <- r.bodyText.compile.string
      _ = assertEquals(r.status, Status.Ok)
      _ = assert(!body.contains("127.0.0.1") && !body.contains("秘密の名前"), body)
      ps <- IO.fromEither(io.circe.parser.decode[PublicStatusDto](body))
      _ = assertEquals(ps.items.map(i => (i.name, i.status)), List(("Web サイト", MonStatus.Up)))
      _ = assertEquals(ps.overall, MonStatus.Up)
      _ = assertEquals(ps.items.head.days.size, 90)
      _ <- call(
        t,
        Method.POST,
        "/api/pages",
        Some(StatusPageReq("internal", "社内", public = false).asJson)
      ).flatMap(json[StatusPageDto])
      g <- e.app.run(Request[IO](Method.GET, uri"/api/public/status/internal"))
      _ = assertEquals(g.status, Status.NotFound)
      li <- call(t, Method.GET, "/api/public/status/internal")
      _ = assertEquals(li.status, Status.Ok)
    } yield ()
  }

  test("API トークン: 参照のみは変更できない. MCP でツールを呼べる") {
    for {
      t <- login("AI 利用者", "ai@example.com")
      ro <- call(
        t,
        Method.POST,
        "/api/tokens",
        Some(ApiTokenReq("参照", TokenScope.Read, None).asJson)
      ).flatMap(json[ApiTokenCreated])
      rw <- call(
        t,
        Method.POST,
        "/api/tokens",
        Some(ApiTokenReq("読み書き", TokenScope.Write, Some(30)).asJson)
      ).flatMap(json[ApiTokenCreated])
      l <- bearer(ro.token, Method.GET, "/api/monitors")
      _ = assertEquals(l.status, Status.Ok)
      w <- bearer(ro.token, Method.POST, "/api/monitors", Some(httpMonitor("x", "https://example.com").asJson))
      _ = assertEquals(w.status, Status.Forbidden)
      rpc = (
        tok: String,
        method: String,
        params: Json
      ) =>
        bearer(
          tok,
          Method.POST,
          "/mcp",
          Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "method" -> method.asJson, "params" -> params))
        )
          .flatMap(json[Json])
      tl <- rpc(ro.token, "tools/list", Json.obj())
      names = tl.hcursor.downField(
        "result"
      ).downField("tools").as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[String]("name").toOption)
      _ = assert(names.contains("get_status") && !names.contains("create_monitor"), names)
      cr <- rpc(
        rw.token,
        "tools/call",
        Json.obj(
          "name" -> "create_monitor".asJson,
          "arguments" -> Json.obj(
            "name" -> "AI が作った監視".asJson,
            "kind" -> "tcp".asJson,
            "host" -> "127.0.0.1".asJson,
            "port" -> 9.asJson,
            "intervalSec" -> 120.asJson
          )
        )
      )
      _ = assertEquals(cr.hcursor.downField("result").get[Boolean]("isError").toOption, Some(false), cr.noSpaces)
      up <- rpc(
        rw.token,
        "tools/call",
        Json.obj(
          "name" -> "update_monitor".asJson,
          "arguments" -> Json.obj("monitor" -> "AI が作った監視".asJson, "port" -> 10.asJson)
        )
      )
      _ = assertEquals(up.hcursor.downField("result").get[Boolean]("isError").toOption, Some(false), up.noSpaces)
      ms <- call(t, Method.GET, "/api/monitors").flatMap(json[List[MonitorDto]])
      ai = ms.find(_.name == "AI が作った監視").get
      _ = assertEquals((ai.config.port, ai.intervalSec), (10, 120))
      st <- rpc(ro.token, "tools/call", Json.obj("name" -> "get_status".asJson))
      text = st.hcursor.downField("result").downField("content").downArray.get[String]("text").getOrElse("")
      _ = assert(text.contains("正常") && text.contains("ノード"), text)
    } yield ()
  }

  test("複数の拠点: 1つの拠点だけが届かなければ skotare 側（拠点）の異常とし、監視先の停止は過半数の拠点で判断する") {
    val e = fixture()
    val now = java.time.Instant.now()
    val ok = skotare.core.ProbeResult(true, Some(5), "応答コード 200")
    def ng(
      m: String
    ) = skotare.core.ProbeResult(false, None, m)
    def rec(
      id: UUID,
      region: String,
      r: skotare.core.ProbeResult
    ) =
      e.monitors.record(id, region, r, s"node-$region", java.time.Instant.now(), apiCfg.publicUrl)
    def health(
      region: String
    ) = e.nodes.regions.map(_.find(_.name == region).map(_.health))
    def status(
      id: UUID
    ) = e.monitors.find(id).map(_.get.status)
    def outbox(
      event: String
    ) = sql"select count(*) from notification_outbox where payload->>'event' = $event".query[Int].unique.transact(e.xa)
    for {
      t <- login("拠点 担当", "region@example.com")
      _ <- List("tokyo", "osaka", "nagoya").traverse_(r => e.nodes.beat(s"node-$r", r, "test", now))
      ms <- (1 to 4).toList.traverse(i =>
        call(
          t,
          Method.POST,
          "/api/monitors",
          Some(MonitorReq(s"拠点-$i", Kind.Tcp, MonitorConfig(host = "127.0.0.1", port = 1), retries = 0).asJson)
        ).flatMap(json[MonitorDto]).map(_.id)
      )
      ids = ms
      // どの拠点からも正常
      _ <- ids.traverse_(id => List("tokyo", "osaka", "nagoya").traverse_(r => rec(id, r, ok)))
      _ <- ids.traverse(status).map(st => assertEquals(st.distinct, List(MonStatus.Up)))
      // 大阪からだけ届かない → 大阪は停止と見るが、過半数（2/3）に届かないので監視は正常のまま
      _ <- ids.traverse_(id => rec(id, "osaka", ng("接続できません（タイムアウト）")))
      _ <- ids.traverse(status).map(st => assertEquals(st.distinct, List(MonStatus.Up)))
      // 拠点の判定: 大阪だけが、ほかの拠点では正常な監視先に届かない → 大阪を疑う（skotare 側の異常）
      _ <- e.worker.judgeTick
      _ <- health("osaka").map(h => assertEquals(h, Some(RegionHealth.Suspect)))
      _ <- health("tokyo").map(h => assertEquals(h, Some(RegionHealth.Ok)))
      s1 <- call(t, Method.GET, "/api/summary").flatMap(json[SummaryDto])
      _ = assert(s1.regions.find(_.name == "osaka").exists(_.reason.contains("この拠点だけ")), s1.regions)
      m1 <- e.monitors.find(ids.head).map(_.get)
      _ = assertEquals(
        m1.regions.find(_.region == "osaka").map(r => (r.status, r.counted)),
        Some((MonStatus.Down, false))
      )
      _ <- outbox("region.suspect").map(n => assert(n > 0, "拠点の異常を知らせる"))
      _ <-
        sql"select count(*) from notification_outbox where payload->>'event' = 'down' and payload->'monitor'->>'name' like '拠点-%'"
          .query[Int].unique.transact(e.xa).map(n => assertEquals(n, 0, "監視先の停止とは知らせない"))
      // 監視先が本当に止まった: 健全な東京・名古屋の両方から届かない → 停止（疑わしい大阪の票は数えない）
      _ <- rec(ids.head, "tokyo", ng("接続を拒否されました"))
      _ <- status(ids.head).map(s => assertEquals(s, MonStatus.Up, "東京だけでは過半数に届かない"))
      t1 <- rec(ids.head, "nagoya", ng("接続を拒否されました"))
      _ = assertEquals(t1.map(_.to), Some(MonStatus.Down))
      _ = assert(
        t1.exists(x => x.message.contains("tokyo:") && x.message.contains("nagoya:") && !x.message.contains("osaka")),
        t1
      )
      // 大阪が直った（ほかの監視先にふたたび届く）→ 大阪を健全に戻す
      _ <- ids.tail.traverse_(id => rec(id, "osaka", ok))
      _ <- e.worker.judgeTick
      _ <- health("osaka").map(h => assertEquals(h, Some(RegionHealth.Ok)))
      _ <- outbox("region.ok").map(n => assert(n > 0))
      _ <- status(ids.head).map(s => assertEquals(s, MonStatus.Down))
      // 拠点が止まった（心拍が途絶えた）→ 停止にし、その拠点の状態を未確認に戻す（戻ったときに古い票で判定しない）
      _ <-
        sql"update node set seen_at = now() - interval '10 minutes' where region = 'nagoya'".update.run.transact(e.xa)
      _ <- e.worker.judgeTick
      _ <- health("nagoya").map(h => assertEquals(h, Some(RegionHealth.Offline)))
      _ <- outbox("region.offline").map(n => assert(n > 0))
      _ <- sql"select count(*) from monitor_region where region = 'nagoya' and status <> 'pending'".query[
        Int
      ].unique.transact(e.xa)
        .map(n => assertEquals(n, 0))
      _ <- status(ids.head).map(s => assertEquals(s, MonStatus.Down, "残りの拠点（東京・大阪）がどちらも停止と見ている"))
      // 日ごとの集計: 判定の決まった確認だけを数える（この監視はずっと正常）
      d <-
        sql"select coalesce(sum(total_n), 0)::int, coalesce(sum(up_n), 0)::int from check_daily where monitor_id = ${ids(1).toString}::uuid"
          .query[(Int, Int)].unique.transact(e.xa)
      _ = assert(d._1 > 0 && d._1 == d._2, d)
      m2 <- e.monitors.find(ids(1)).map(_.get)
      _ = assertEquals(m2.uptime30d, Some(100.0))
      // 停止中に設定を変えると、障害を閉じて closed を知らせる（復旧とは知らせない）
      m0 <- e.monitors.find(ids.head).map(_.get)
      r0 <- call(
        t,
        Method.PUT,
        s"/api/monitors/${ids.head}",
        Some(m0.toReq.copy(config = m0.config.copy(port = 2)).asJson)
      ).flatMap(json[MonitorDto])
      _ = assertEquals(r0.status, MonStatus.Pending)
      _ = assertEquals(r0.lastMessage, None)
      cl <-
        sql"select payload::text from notification_outbox where payload->>'event' = 'closed' and payload->'monitor'->>'id' = ${ids.head.toString}"
          .query[String].to[List].transact(e.xa).map(_.flatMap(io.circe.parser.parse(_).toOption))
      _ = assert(cl.nonEmpty, "通知先ごとに1行")
      _ = assert(Messages.subject(cl.head).contains("障害を閉じました"), Messages.subject(cl.head))
      // 基準点: 東京から基準点に届かなければ、東京を疑う
      c <- call(
        t,
        Method.POST,
        "/api/monitors",
        Some(MonitorReq(
          "基準点",
          Kind.Tcp,
          MonitorConfig(host = "127.0.0.1", port = 1),
          retries = 0,
          canary = true
        ).asJson)
      ).flatMap(json[MonitorDto])
      _ = assert(c.canary)
      _ <- rec(c.id, "tokyo", ng("名前を引けません"))
      _ <- e.worker.judgeTick
      _ <- e.nodes.regions.map(rs =>
        assert(rs.find(_.name == "tokyo").exists(r => r.health == RegionHealth.Suspect && r.reason.contains("基準点")), rs)
      )
      // 拠点の通知の文面
      _ = assert(Messages.subject(
        io.circe.parser.parse("""{"event":"region.suspect","region":{"name":"tokyo"}}""").toOption.get
      ).contains("拠点の異常の疑い: tokyo"))
      // 後片づけ（ほかのテストに拠点を残さない）
      _ <- sql"delete from monitor where name like '拠点-%' or name = '基準点'".update.run.transact(e.xa)
      _ <- sql"delete from region where name in ('tokyo', 'osaka', 'nagoya')".update.run.transact(e.xa)
      _ <- sql"delete from node where region in ('tokyo', 'osaka', 'nagoya')".update.run.transact(e.xa)
    } yield ()
  }

  // ------------------------------------------------------------------ OAuth の提供側（skakapo が利用者の権限で使う）

  private val verifier = "v" * 50
  private val challenge = sbirds.auth.Crypto.pkceChallenge(verifier)

  private def authorizeUri(
    client: String = "skakapo",
    redirect: String = agentCb,
    scope: String = "read write"
  ): Uri =
    uri"/oauth/authorize".withQueryParams(Map(
      "response_type" -> "code",
      "client_id" -> client,
      "redirect_uri" -> redirect,
      "scope" -> scope,
      "state" -> "st-1",
      "code_challenge" -> challenge,
      "code_challenge_method" -> "S256"
    ))

  private def withSession(
    session: String,
    r: Request[IO],
    extra: (String, String)*
  ): Request[IO] =
    r.putHeaders(Cookie(cats.data.NonEmptyList(
      RequestCookie("skotare_session", session),
      extra.toList.map(
        (
          k,
          v
        ) => RequestCookie(k, v)
      )
    )))

  private def token(
    form: (String, String)*
  )(
    client: String = "skakapo",
    secret: String = "skakapo-test-secret-0123456789"
  ): IO[Response[IO]] =
    fixture().app.run(Request[IO](Method.POST, uri"/oauth/token").withEntity(UrlForm(form*))
      .putHeaders(Authorization(BasicCredentials(client, secret))))

  /** ログインした利用者が許可の画面で許可し、認可コードを受け取る. */
  private def authorize(
    session: String,
    scope: String
  ): IO[String] = {
    val e = fixture()
    for {
      pg <- e.app.run(withSession(session, Request[IO](Method.GET, authorizeUri(scope = scope))))
      _ = assertEquals(pg.status, Status.Ok)
      html <- pg.bodyText.compile.string
      _ = assert(html.contains("skakapo") && html.contains("<code>read</code>"), html)
      _ = assertEquals(html.contains("<code>write</code>"), scope.contains("write"))
      csrf = pg.cookies.find(_.name == "skotare_oauth_csrf").map(_.content).getOrElse(fail("no csrf cookie"))
      hidden = hiddenInputs(html)
      // CSRF の値が合わなければ断る
      bad <- e.app.run(withSession(
        session,
        Request[IO](Method.POST, uri"/oauth/authorize").withEntity(UrlForm((hidden :+ ("decision" -> "allow"))*))
      ))
      _ = assertEquals(bad.status, Status.BadRequest)
      ok <- e.app.run(withSession(
        session,
        Request[IO](Method.POST, uri"/oauth/authorize").withEntity(UrlForm((hidden :+ ("decision" -> "allow"))*)),
        "skotare_oauth_csrf" -> csrf
      ))
      _ = assertEquals(ok.status, Status.Found)
      cb = location(ok)
      _ = assert(cb.renderString.startsWith(agentCb + "?"), cb.renderString)
      _ = assertEquals(cb.params.get("state"), Some("st-1"))
    } yield cb.params.getOrElse("code", fail(s"no code: $cb"))
  }

  /** 許可の画面のフォームの hidden の値（ボタンの decision は除く）. */
  private def hiddenInputs(
    html: String
  ): List[(String, String)] =
    "type=\"hidden\" name=\"([a-z_]+)\" value=\"([^\"]*)\"".r.findAllMatchIn(html).map(m =>
      m.group(1) -> m.group(2).replace("&amp;", "&")
    ).toList

  private def exchange(
    code: String
  ): IO[Json] =
    token(
      "grant_type" -> "authorization_code",
      "code" -> code,
      "redirect_uri" -> agentCb,
      "code_verifier" -> verifier
    )().flatMap(json[Json])

  private def mcp(
    tok: String,
    method: String,
    params: Json = Json.obj()
  ): IO[Response[IO]] =
    bearer(
      tok,
      Method.POST,
      "/mcp",
      Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "method" -> method.asJson, "params" -> params))
    )

  test("OAuth: 未ログインならログインへ、知らないクライアント・違う戻り先は 400、許していないスコープは invalid_scope") {
    val e = fixture()
    for {
      r1 <- e.app.run(Request[IO](Method.GET, authorizeUri()))
      _ = assertEquals(r1.status, Status.Found)
      lo = location(r1)
      _ = assertEquals(lo.path.renderString, "/auth/login")
      _ = assert(lo.params.get("returnTo").exists(_.startsWith("/oauth/authorize?")), lo.renderString)
      // ログインの戻り先として受け付ける（ログインの後にこの画面へ戻る）
      _ = assertEquals(SafePath.of(lo.params.get("returnTo")), lo.params("returnTo"))
      r2 <- e.app.run(Request[IO](Method.GET, authorizeUri(client = "unknown")))
      _ = assertEquals(r2.status, Status.BadRequest)
      r3 <- e.app.run(Request[IO](Method.GET, authorizeUri(redirect = "https://evil.example.com/cb")))
      _ = assertEquals(r3.status, Status.BadRequest)
      _ = assert(r3.headers.get[Location].isEmpty)
      r4 <- e.app.run(Request[IO](
        Method.GET,
        authorizeUri(client = "viewer", redirect = "https://viewer.example.com/cb", scope = "read write")
      ))
      _ = assertEquals(r4.status, Status.Found)
      _ = assertEquals(location(r4).params.get("error"), Some("invalid_scope"))
      r5 <- e.app.run(Request[IO](Method.GET, authorizeUri(scope = "admin")))
      _ = assertEquals(location(r5).params.get("error"), Some("invalid_scope"))
      // PKCE は S256 だけ
      r6 <- e.app.run(Request[IO](Method.GET, authorizeUri().withQueryParam("code_challenge_method", "plain")))
      _ = assertEquals(location(r6).params.get("error"), Some("invalid_request"))
      // クライアントの認証が違えば 401
      t1 <- token("grant_type" -> "authorization_code", "code" -> "x")(secret = "wrong-secret-0123456789")
      _ = assertEquals(t1.status, Status.Unauthorized)
    } yield ()
  }

  test("OAuth: 許可 → コード → トークンで MCP を使える. refresh で替えると前のトークンは使えない. 設定の画面で取り消せる") {
    for {
      t <- login("連携する人", "agent-user@example.com")
      code <- authorize(t, "read write")
      tk <- exchange(code)
      c = tk.hcursor
      access = c.get[String]("access_token").getOrElse(fail(tk.noSpaces))
      refresh = c.get[String]("refresh_token").getOrElse(fail(tk.noSpaces))
      _ = assertEquals(c.get[String]("token_type").toOption, Some("Bearer"))
      _ = assertEquals(c.get[String]("scope").toOption, Some("read write"))
      _ = assertEquals(c.get[Long]("expires_in").toOption, Some(3600L))
      // 同じコードは2度使えない
      again <- token(
        "grant_type" -> "authorization_code",
        "code" -> code,
        "redirect_uri" -> agentCb,
        "code_verifier" -> verifier
      )()
      _ = assertEquals(again.status, Status.BadRequest)
      tl <- mcp(access, "tools/list").flatMap(json[Json])
      names = tl.hcursor.downField(
        "result"
      ).downField("tools").as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[String]("name").toOption)
      _ = assert(names.contains("get_status") && names.contains("create_monitor"), names)
      me <- bearer(access, Method.GET, "/api/me").flatMap(json[MeDto])
      _ = assertEquals(me.name, "連携する人")
      cr <- bearer(access, Method.POST, "/api/monitors", Some(httpMonitor("連携で作った監視", "https://example.com").asJson))
      _ = assertEquals(cr.status, Status.Created)
      // 連携のトークンは API トークンの一覧ではなく、連携しているサービスに出る
      toks <- call(t, Method.GET, "/api/tokens").flatMap(json[List[ApiTokenDto]])
      _ = assert(!toks.exists(_.name.contains("skakapo")), toks)
      gs <- call(t, Method.GET, "/api/oauth/grants").flatMap(json[List[OAuthGrantDto]])
      _ = assertEquals(gs.map(g => (g.clientId, g.name, g.scope)), List(("skakapo", "skakapo", "write")))
      _ = assert(gs.head.lastUsedAt.isDefined)
      // 連携の管理は画面（Cookie）からだけ
      gb <- bearer(access, Method.GET, "/api/oauth/grants")
      _ = assertEquals(gb.status, Status.Forbidden)
      // refresh で替える（scope を付ければ狭められ、広げられない）
      r1 <- token("grant_type" -> "refresh_token", "refresh_token" -> refresh)().flatMap(json[Json])
      access2 = r1.hcursor.get[String]("access_token").getOrElse(fail(r1.noSpaces))
      refresh2 = r1.hcursor.get[String]("refresh_token").getOrElse(fail(r1.noSpaces))
      _ = assertEquals(r1.hcursor.get[String]("scope").toOption, Some("read write"))
      old <- bearer(access, Method.GET, "/api/me")
      _ = assertEquals(old.status, Status.Unauthorized)
      reuse <- token("grant_type" -> "refresh_token", "refresh_token" -> refresh)()
      _ = assertEquals(reuse.status, Status.BadRequest)
      other <- token(
        "grant_type" -> "refresh_token",
        "refresh_token" -> refresh2
      )(client = "viewer", secret = "viewer-test-secret-0123456789")
      _ = assertEquals(other.status, Status.BadRequest)
      now <- mcp(access2, "tools/list")
      _ = assertEquals(now.status, Status.Ok)
      r2 <- token("grant_type" -> "refresh_token", "refresh_token" -> refresh2, "scope" -> "read")().flatMap(json[Json])
      _ = assertEquals(r2.hcursor.get[String]("scope").toOption, Some("read"))
      access3 = r2.hcursor.get[String]("access_token").getOrElse("")
      refresh3 = r2.hcursor.get[String]("refresh_token").getOrElse("")
      wide <- token("grant_type" -> "refresh_token", "refresh_token" -> refresh3, "scope" -> "read write")()
      _ = assertEquals(wide.status, Status.BadRequest)
      // 広げる要求で断っても、refresh token はそのまま使える
      ro <- bearer(access3, Method.POST, "/api/monitors", Some(httpMonitor("参照のみ", "https://example.com").asJson))
      _ = assertEquals(ro.status, Status.Forbidden)
      gs2 <- call(t, Method.GET, "/api/oauth/grants").flatMap(json[List[OAuthGrantDto]])
      _ = assertEquals(gs2.map(_.scope), List("read"))
      _ = assertEquals(gs2.head.grantedAt, gs.head.grantedAt)
      // 取り消すと、アクセストークンも refresh token も使えない
      del <- call(t, Method.DELETE, "/api/oauth/grants/skakapo")
      _ = assertEquals(del.status, Status.NoContent)
      gone <- mcp(access3, "tools/list")
      _ = assertEquals(gone.status, Status.Unauthorized)
      rf <- token("grant_type" -> "refresh_token", "refresh_token" -> refresh3)()
      _ = assertEquals(rf.status, Status.BadRequest)
      gs3 <- call(t, Method.GET, "/api/oauth/grants").flatMap(json[List[OAuthGrantDto]])
      _ = assertEquals(gs3, Nil)
    } yield ()
  }

  test("OAuth: read だけのトークンでは書き込みが拒まれる. 拒否すれば access_denied") {
    for {
      t <- login("参照だけ許す人", "read-only@example.com")
      code <- authorize(t, "read")
      tk <- exchange(code)
      _ = assertEquals(tk.hcursor.get[String]("scope").toOption, Some("read"))
      access = tk.hcursor.get[String]("access_token").getOrElse(fail(tk.noSpaces))
      l <- bearer(access, Method.GET, "/api/monitors")
      _ = assertEquals(l.status, Status.Ok)
      w <- bearer(access, Method.POST, "/api/monitors", Some(httpMonitor("書けない", "https://example.com").asJson))
      _ = assertEquals(w.status, Status.Forbidden)
      tl <- mcp(access, "tools/list").flatMap(json[Json])
      names = tl.hcursor.downField(
        "result"
      ).downField("tools").as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[String]("name").toOption)
      _ = assert(names.contains("get_status") && !names.contains("create_monitor"), names)
      cr <- mcp(
        access,
        "tools/call",
        Json.obj(
          "name" -> "create_monitor".asJson,
          "arguments" ->
            Json.obj("name" -> "書けない".asJson, "kind" -> "tcp".asJson, "host" -> "127.0.0.1".asJson, "port" -> 9.asJson)
        )
      ).flatMap(json[Json])
      _ = assertEquals(cr.hcursor.downField("result").get[Boolean]("isError").toOption, Some(true), cr.noSpaces)
      // 拒否すれば access_denied で戻す
      pg <- fixture().app.run(withSession(t, Request[IO](Method.GET, authorizeUri(scope = "read"))))
      csrf = pg.cookies.find(_.name == "skotare_oauth_csrf").map(_.content).getOrElse("")
      html <- pg.bodyText.compile.string
      hidden = hiddenInputs(html)
      dn <- fixture().app.run(withSession(
        t,
        Request[IO](Method.POST, uri"/oauth/authorize").withEntity(UrlForm((hidden :+ ("decision" -> "deny"))*)),
        "skotare_oauth_csrf" -> csrf
      ))
      _ = assertEquals(location(dn).params.get("error"), Some("access_denied"))
    } yield ()
  }
}
