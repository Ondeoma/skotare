package skotare.app

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import doobie.implicits.*
import fs2.io.file.Path
import org.http4s.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.io.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.ErrorHandling
import org.http4s.server.staticcontent.*
import skotare.api.*
import skotare.auth.{OidcClient, OidcConfig, SecretBox}
import skotare.db.*
import skotare.observe.{HttpClientTelemetry, HttpTelemetry, Metrics, RumRoutes, Telemetry}
import skotare.push.{PushService, PushStore}
import skotare.shared.ErrorDto

import scala.concurrent.duration.*

final case class AppConfig(port: Int, db: DbConfig, oidc: Option[OidcConfig], api: ApiConfig, staticDir: Option[String],
    appSecret: Option[String], node: String, concurrency: Int, region: String, bindHost: com.comcast.ip4s.Host)

/** 設定は環境変数から読む. */
object Config:
  private def opt(name: String): Option[String] = sys.env.get(name).map(_.trim).filter(_.nonEmpty)
  private def int(name: String): Option[Int]    = opt(name).flatMap(_.toIntOption)
  private def enabled(name: String): Boolean    = opt(name).exists(_.equalsIgnoreCase("true"))
  private def required(name: String): Either[String, String] = opt(name).toRight(s"環境変数 $name を設定してください")
  private def list(name: String): List[String] = opt(name).toList.flatMap(_.split(Array(',', ' ')).map(_.trim).filter(_.nonEmpty))

  private def local(url: String): Boolean =
    scala.util.Try(java.net.URI.create(url).getHost).toOption.flatMap(Option(_)).map(_.toLowerCase)
      .exists(h => Set("localhost", "127.0.0.1", "[::1]").contains(h) || h.endsWith(".localhost"))

  /** ノードの名前（既定はホスト名. コンテナではコンテナの ID になる）. */
  private def hostName: String = scala.util.Try(java.net.InetAddress.getLocalHost.getHostName).getOrElse("node")

  /**
   * OAUTH_CLIENTS: skotare を利用者の権限で使うサービス（OAuth のクライアント）.
   * [{"id":"skakapo","secret":"…（16文字以上）","redirectUri":"https://…/api/connections/callback","name":"skakapo","scopes":["read","write"]}]
   * （scopes は省くと read だけ. redirectUri は https://（手元なら http://localhost も可））
   */
  private def oauthClients: Either[String, List[OAuthClient]] =
    opt("OAUTH_CLIENTS").fold(Right(Nil)) { s =>
      io.circe.parser.parse(s).flatMap(_.as[List[io.circe.Json]]).left.map(_ => "OAUTH_CLIENTS は JSON の配列で指定してください").flatMap(_.traverse { j =>
        val c = j.hcursor
        (c.get[String]("id"), c.get[String]("secret"), c.get[String]("redirectUri")).tupled.toOption
          .filter((id, sec, uri) => id.nonEmpty && sec.length >= 16 && (uri.startsWith("https://") || uri.startsWith("http://localhost")))
          .map((id, sec, uri) => OAuthClient(id, sec, uri, c.get[String]("name").getOrElse(id),
            c.get[List[String]]("scopes").toOption.map(_.filter(Set("read", "write"))).filter(_.nonEmpty).getOrElse(List("read"))))
          .toRight("OAUTH_CLIENTS の各要素には id・secret（16文字以上）・redirectUri（https://）を指定してください")
      }).flatMap(cs => Either.cond(cs.map(_.id).distinct.size == cs.size, cs, "OAUTH_CLIENTS の id が重複しています"))
    }

  def load: IO[AppConfig] = IO.fromEither(
    (for
      dbUrl  <- required("DATABASE_URL")
      dbUser <- required("DATABASE_USER")
      dbPass <- required("DATABASE_PASSWORD")
      pub    <- required("PUBLIC_URL").map(_.stripSuffix("/"))
      devLogin = enabled("DEV_LOGIN")
      oidc <- (opt("OIDC_ISSUER"), opt("OIDC_CLIENT_ID"), opt("OIDC_CLIENT_SECRET")) match
                // グループを絞らないと IdP のだれでもログインでき、監視を通して内部のネットワークを調べられる. 明示したときだけ許す
                case (Some(_), Some(_), Some(_)) if list("OIDC_ALLOWED_GROUPS").isEmpty && !enabled("OIDC_ALLOW_ANY_USER") =>
                  Left("OIDC_ALLOWED_GROUPS でログインできるグループを絞ってください（IdP のだれでもログインしてよいときは OIDC_ALLOW_ANY_USER=true）")
                case (Some(i), Some(c), Some(s)) =>
                  Right(Some(OidcConfig(i, c, s, s"$pub/auth/callback", opt("OIDC_GROUPS_CLAIM").getOrElse("groups"),
                    Some(list("OIDC_SCOPES")).filter(_.nonEmpty).getOrElse(List("openid", "profile", "email")), list("OIDC_ALLOWED_GROUPS").toSet)))
                // 開発用のログインは名前だけでだれにでもなれるので、手元（localhost）で開くときだけ
                case (None, None, None) if devLogin && !local(pub) => Left("DEV_LOGIN は PUBLIC_URL が localhost のときだけ使えます")
                case (None, None, None) if devLogin => Right(None)
                case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      proxies <- list("TRUSTED_PROXIES").traverse(s => RateLimitConfig.proxy(s).toRight(s"TRUSTED_PROXIES の「$s」は IP か CIDR で書いてください"))
      secret <- opt("APP_SECRET") match
                  case Some(s) if s.length < 16 => Left("APP_SECRET は16文字以上にしてください")
                  case s                        => Right(s)
      conc <- int("CHECK_CONCURRENCY").getOrElse(16) match
                case n if n >= 1 && n <= 256 => Right(n)
                case _                       => Left("CHECK_CONCURRENCY は 1〜256 にしてください")
      // 拠点の名前（同じ拠点のノードは同じ名前にする. 例: tokyo・osaka・東京）
      // 待ち受けるアドレス（既定 0.0.0.0. 開発用のログインのときは 127.0.0.1）
      bind   <- opt("BIND_HOST").fold[Either[String, com.comcast.ip4s.Host]](Right(if devLogin then ipv4"127.0.0.1" else ipv4"0.0.0.0"))(h =>
                  com.comcast.ip4s.Host.fromString(h).toRight("BIND_HOST は IP アドレスかホスト名で書いてください"))
      clients <- oauthClients
      region <- opt("REGION").getOrElse("default") match
                  case r if r.length <= 40 && !r.exists(c => c.isControl || c == '/') => Right(r)
                  case _ => Left("REGION は40文字までで、制御文字と / を含めないでください")
    yield AppConfig(
      port = int("HTTP_PORT").getOrElse(8080),
      // 接続の数は、同時に確かめる数より多くする（確認の記録と API・裏の処理で取り合って待たないように）
      db = DbConfig(dbUrl, dbUser, dbPass, int("DATABASE_POOL_SIZE").getOrElse(conc + 6)),
      oidc = oidc,
      api = ApiConfig(cookieSecure = pub.startsWith("https://"), publicUrl = pub, sessionTtl = int("SESSION_DAYS").getOrElse(14).days,
        devLogin = devLogin, oauthClients = clients,
        rateLimit = RateLimitConfig(int("RATE_LIMIT_AUTH").getOrElse(20), int("RATE_LIMIT_PUBLIC").getOrElse(120), proxies)),
      staticDir = opt("STATIC_DIR"),
      appSecret = secret,
      node = opt("NODE_NAME").getOrElse(hostName).take(100),
      concurrency = conc,
      region = region,
      bindHost = bind
    )).left.map(new IllegalArgumentException(_))
  )

object Main extends IOApp.Simple:

  // ログにトレースの ID・操作したユーザーを載せるため、IOLocal の文脈を Java のライブラリから見えるようにする
  // （IORuntime を作る前に設定する必要がある. オブジェクトの初期化は main より先に動く）
  Telemetry.prepare()

  private val version = Option(getClass.getPackage.getImplementationVersion).orElse(sys.env.get("APP_VERSION")).getOrElse("dev")

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.slf4j.Slf4jFactory.create[IO]

  def run: IO[Unit] =
    for
      cfg <- Config.load
      n   <- migrateWithRetry(cfg)
      _   <- skotare.observe.Log.println(s"[skotare] database migrated ($n migration(s) applied)")
      _ <- (for
        _    <- Telemetry.resource("skotare", version, runtime)
        xa   <- Database.transactor(cfg.db)
        _    <- gauges(xa)
        // 外への呼び出し（IdP・通知先・プッシュのサービス）もトレースに出す（traceparent を付けて送る）
        http <- EmberClientBuilder.default[IO].withTimeout(30.seconds).build.map(HttpClientTelemetry(_))
        oidc <- Resource.eval(cfg.oidc.traverse(OidcClient.create(_, http)))
        users     = new UserStore(xa)
        sess      = new SessionAuth(users, cfg.api)
        box       = cfg.appSecret.map(new SecretBox(_))
        monitors  = new MonitorStore(xa)
        notifiers = new NotifierStore(xa)
        pages     = new PageStore(xa)
        nodes     = new NodeStore(xa)
        senders   = new Senders(http, box, cfg.api.publicUrl)
        _ <- Resource.eval(IO.whenA(oidc.isEmpty)(skotare.observe.Log.println("[skotare] WARNING: OIDC が未設定です。開発用のログイン（DEV_LOGIN）を使います。本番では使わないでください")))
        _ <- Resource.eval(IO.whenA(box.isEmpty)(skotare.observe.Log.println("[skotare] WARNING: APP_SECRET が未設定のため、通知先（stui・skereru・Webhook）を使えません")))
        // OIDC の設定の誤り（issuer の URL など）は起動時に知らせる（IdP が止まっていても起動はする）
        _ <- Resource.eval(oidc.traverse_(_.discover.attempt.flatMap {
               case Right(m) => skotare.observe.Log.println(s"[skotare] OIDC: ${m.issuer}")
               case Left(e)  => skotare.observe.Log.println(s"[skotare] WARNING: OIDC の discovery に失敗しました（${e.getMessage}）。ログインのときにもう一度試します")
             }))
        // プッシュ通知（PWA）: 監視の停止・復旧
        pushStore = new PushStore(xa)
        push     <- Resource.eval(PushService.create(pushStore, http, cfg.api.publicUrl, "skotare"))
        announcer = new Announcer(Some(pushStore -> push), cfg.api.publicUrl)
        worker    = new Worker(monitors, notifiers, nodes, senders, announcer, WorkerConfig(cfg.node, cfg.api.publicUrl, version, cfg.concurrency, cfg.region))
        _ <- purgeLoop(users).background
        _ <- worker.run.background
        web <- Resource.eval(cfg.staticDir.traverse(spa))
        routes = new MonitorRoutes(monitors, notifiers, pages, nodes, senders, box, sess, announcer)
        all = ApiRoutes(oidc, users, sess, cfg.api, routes, Some(pushStore -> push)) <+>
                healthRoutes(xa) <+> RumRoutes.routes <+> web.getOrElse(HttpRoutes.empty[IO])
        // セキュリティのヘッダーは、見つからない 404・内部エラーの 500 にも付ける
        httpApp <- HttpTelemetry(SecurityHeaders.app(ErrorHandling.Custom.recoverWith(all.orNotFound)(handler), cfg.api.cookieSecure),
                     List("/api", "/auth", "/mcp", "/oauth"), RateLimit.clientIp(_, cfg.api.rateLimit.trustedProxies))
        _ <- EmberServerBuilder.default[IO]
               .withHost(cfg.bindHost)
               .withPort(Port.fromInt(cfg.port).getOrElse(port"8080"))
               .withShutdownTimeout(5.seconds)
               .withHttpApp(httpApp)
               .build
        _ <- Resource.eval(skotare.observe.Log.println(s"[skotare] listening on :${cfg.port} (${cfg.api.publicUrl}, node ${cfg.node})"))
      yield ()).useForever
    yield ()

  /** DB が使えるまで待ってからマイグレーションする. */
  private def migrateWithRetry(cfg: AppConfig, attempt: Int = 1): IO[Int] =
    Database.migrate(cfg.db).handleErrorWith { e =>
      if attempt >= 60 then IO.raiseError(e)
      else skotare.observe.Log.println(s"[skotare] database not ready (${e.getMessage.take(120)}); retry $attempt/60") *> IO.sleep(2.seconds) *> migrateWithRetry(cfg, attempt + 1)
    }

  private def healthRoutes(xa: doobie.Transactor[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "healthz" =>
      sql"select 1".query[Int].unique.transact(xa).timeout(3.seconds).attempt.map {
        case Right(_) => TextResponse(Status.Ok, "ok")
        case Left(_)  => TextResponse(Status.ServiceUnavailable, "database unavailable")
      }
  }

  private val handler: PartialFunction[Throwable, IO[Response[IO]]] = { case e =>
    IO(org.slf4j.LoggerFactory.getLogger("skotare").error("unhandled error", e)) *> InternalServerError(ErrorDto("サーバーでエラーが発生しました"))
  }

  /** DB から定期的に読むメトリクス（skorimako のダッシュボード・アラート用）. */
  private def gauges(xa: doobie.Transactor[IO]): Resource[IO, Unit] =
    def q(sqlText: doobie.Fragment): IO[Long] = sqlText.query[Long].unique.transact(xa)
    List(
      Metrics.gauge("skotare.users.total", "有効なユーザーの数")(q(sql"select count(*) from app_user where not disabled")),
      Metrics.gauge("skotare.users.active_24h", "24時間以内にログインしたユーザーの数")(q(sql"select count(*) from app_user where last_login_at > now() - interval '24 hours'")),
      Metrics.gauge("skotare.monitors.total", "有効な監視の数")(q(sql"select count(*) from monitor where enabled")),
      Metrics.gauge("skotare.monitors.down", "停止している監視の数", 30.seconds)(q(sql"select count(*) from monitor where status = 'down'")),
      Metrics.gauge("skotare.incidents.open", "続いている障害の数", 30.seconds)(q(sql"select count(*) from incident where resolved_at is null")),
      Metrics.gauge("skotare.checks.overdue", "動いている拠点で、確かめる時刻を大きく過ぎた（判定に数えない）監視の数（ノードが足りない）", 30.seconds)(
        q(fr"select count(*) from monitor_region mr join monitor m on m.id = mr.monitor_id join region r on r.name = mr.region" ++
          fr"where m.enabled and r.health <> 'offline' and" ++ Sql.stale)),
      Metrics.gauge("skotare.regions.unhealthy", "異常の疑い・停止の拠点の数", 30.seconds)(q(sql"select count(*) from region where health <> 'ok'")),
      Metrics.gauge("skotare.notifications.pending", "送れていない通知の数", 30.seconds)(q(sql"select count(*) from notification_outbox where sent_at is null")),
      Metrics.gauge("skotare.nodes.alive", "動いているノードの数", 30.seconds)(q(fr"select count(*) from node where seen_at >" ++ Sql.AliveWindow)),
      Metrics.gauge("skotare.push.subscriptions", "プッシュ通知を受け取る端末の数")(q(sql"select count(*) from push_subscription"))
    ).sequence_

  /** 期限の切れたセッション・ログイン途中の状態を消す（1時間ごと）. */
  private def purgeLoop(users: UserStore): IO[Unit] =
    (IO.sleep(1.minute) *> (IO.realTimeInstant.flatMap(users.purgeExpired)
      .handleErrorWith(e => skotare.observe.Log.println(s"[skotare] purge failed: $e")) *> IO.sleep(1.hour)).foreverM).void

  // IdP が http（開発環境など）のときは、そのアイコンの画像も許す（https の画像はもとから許している）
  private val idpImg: String = sys.env.get("OIDC_ISSUER").flatMap(u => scala.util.Try(java.net.URI.create(u)).toOption)
    .filter(u => u.getScheme == "http" && u.getAuthority != null).fold("")(u => s" http://${u.getAuthority}")

  /**
   * 画面の CSP. スクリプトは自分のサイトのものと、index.html に直接書いたもの（テーマの読み込み. ハッシュで許す）だけ.
   * skorimako のブラウザの観測（RUM）が設定されていれば、その rum.js の読み込みと送信も許す（rum はそのオリジン）.
   * 画像はアイコン（IdP の画像は外部の https）のため広めにする. Service Worker・manifest は自分のサイトなので default-src で足りる.
   */
  def spaCsp(indexHtml: String, rum: Option[String] = RumRoutes.origin): String =
    val inline = "(?s)<script>(.*?)</script>".r.findAllMatchIn(indexHtml).map(m =>
      s"'sha256-${java.util.Base64.getEncoder.encodeToString(java.security.MessageDigest.getInstance("SHA-256").digest(m.group(1).getBytes("UTF-8")))}'")
    (List("default-src 'self'", ("script-src 'self'" :: inline.toList ++ rum.toList).mkString(" "), "style-src 'self' 'unsafe-inline'",
      s"img-src 'self' data: blob: https:$idpImg", ("connect-src 'self'" :: rum.toList).mkString(" "), "object-src 'none'", "base-uri 'self'",
      "frame-ancestors 'none'", "form-action 'self'")).mkString("; ")

  /** ビルド済みの画面の配信. 存在しないパスは index.html（SPA）. */
  private def spa(dir: String): IO[HttpRoutes[IO]] =
    IO.blocking(java.nio.file.Files.readString(java.nio.file.Path.of(dir, "index.html"))).map(spaRoutes(dir, _))

  private def spaRoutes(dir: String, indexHtml: String): HttpRoutes[IO] =
    val csp   = spaCsp(indexHtml)
    val files = fileService[IO](FileService.Config(dir))
    val index = HttpRoutes.of[IO] {
      case req @ GET -> path if !path.segments.headOption.map(_.decoded()).exists(Set("api", "auth", "mcp", "oauth", "healthz")) =>
        StaticFile.fromPath(Path(dir) / "index.html", Some(req)).getOrElseF(NotFound())
    }
    HttpRoutes[IO] { req =>
      val cc = if req.pathInfo.renderString.startsWith("/assets/") then "public, max-age=31536000, immutable" else "no-cache"
      (files <+> index).run(req).map(_.putHeaders("Cache-Control" -> cc, "Content-Security-Policy" -> csp))
    }
