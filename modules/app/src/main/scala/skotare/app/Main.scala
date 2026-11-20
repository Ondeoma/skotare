package skotare.app

import sbirds.db.*
import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import doobie.implicits.*
import org.http4s.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import skotare.api.*
import sbirds.auth.{OidcClient, OidcConfig}
import sbirds.links.IntegrationTokenStore
import skotare.auth.SecretBox
import skotare.db.*
import sbirds.observe.{HttpClientTelemetry, HttpTelemetry, Log, Metrics, RumRoutes, Telemetry}
import sbirds.push.{PushService, PushStore}
import skotare.core.ErrorCode
import sbirds.http.{Errors, Health, SecurityHeaders, Spa}

import scala.concurrent.duration.*

final case class AppConfig(port: Int,
                           db: DbConfig,
                           oidc: Option[OidcConfig],
                           api: ApiConfig,
                           staticDir: Option[String],
                           appSecret: Option[String],
                           node: String,
                           concurrency: Int,
                           region: String,
                           probePolicy: sbirds.net.NetGuard.Policy,
                           notifyPolicy: sbirds.net.NetGuard.Policy,
                           bindHost: com.comcast.ip4s.Host)

object Main extends IOApp.Simple {

  // ログにトレースの ID・操作したユーザーを載せるため、IOLocal の文脈を Java のライブラリから見えるようにする
  // （IORuntime を作る前に設定する必要がある. オブジェクトの初期化は main より先に動く）
  Telemetry.prepare()

  private val version =
    Option(getClass.getPackage.getImplementationVersion).orElse(sys.env.get("APP_VERSION")).getOrElse("dev")

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.slf4j.Slf4jFactory.create[IO]

  def run: IO[Unit] =
    for {
      cfg <- Config.load
      n <- migrateWithRetry(cfg)
      _ <- Log.println(s"[skotare] database migrated ($n migration(s) applied)")
      _ <- (for {
        _ <- Telemetry.resource("skotare", version, runtime)
        xa <- Database.transactor(cfg.db)
        _ <- gauges(xa)
        // 外への呼び出し（IdP・通知先・プッシュのサービス）もトレースに出す（traceparent を付けて送る）
        http <- EmberClientBuilder.default[IO].withTimeout(30.seconds).build.map(HttpClientTelemetry(_))
        oidc <- Resource.eval(cfg.oidc.traverse(OidcClient.create(_, http)))
        users = new UserStore(xa)
        sess = new SessionAuth(users, cfg.api)
        box = cfg.appSecret.map(SecretBox(_))
        monitors = new MonitorStore(xa)
        notifiers = new NotifierStore(xa)
        pages = new PageStore(xa)
        nodes = new NodeStore(xa)
        stui = new StuiLink(cfg.api.stui, new IntegrationTokenStore(xa), http, box, cfg.api.publicUrl, sess)
        // 利用者が決めた URL（Webhook・skereru の URL）へ送るクライアント. 内部のアドレスは NOTIFY_ALLOW_INTERNAL で許したものだけ
        // （確かめた IP にそのままつなぐ. リダイレクトは追わない）
        hookHttp <- sbirds.net.NetGuard.client(cfg.notifyPolicy, timeout = 30.seconds).map(HttpClientTelemetry(_))
        senders = new Senders(hookHttp, box, cfg.api.publicUrl, Some(stui))
        _ <- Resource.eval(IO.whenA(
          oidc.isEmpty
        )(Log.println("[skotare] WARNING: OIDC が未設定です。開発用のログイン（DEV_LOGIN）を使います。本番では使わないでください")))
        _ <- Resource.eval(IO.whenA(
          box.isEmpty
        )(Log.println("[skotare] WARNING: APP_SECRET が未設定のため、通知先（stui・skereru・Webhook）を使えません")))
        // OIDC の設定の誤り（issuer の URL など）は起動時に知らせる（IdP が止まっていても起動はする）
        _ <- Resource.eval(oidc.traverse_(_.discover.attempt.flatMap {
          case Right(m) => Log.println(s"[skotare] OIDC: ${m.issuer}")
          case Left(e) => Log.println(
              s"[skotare] WARNING: OIDC の discovery に失敗しました（${e.getMessage}）。ログインのときにもう一度試します"
            )
        }))
        // プッシュ通知（PWA）: 監視の停止・復旧
        pushStore = new PushStore(xa, keepOwner = true)
        push <- Resource.eval(PushService.create(pushStore, http, cfg.api.publicUrl, "skotare"))
        announcer = new Announcer(Some(users -> push), cfg.api.publicUrl)
        worker = new Worker(
          monitors,
          notifiers,
          nodes,
          senders,
          announcer,
          WorkerConfig(cfg.node, cfg.api.publicUrl, version, cfg.concurrency, cfg.region, cfg.probePolicy)
        )
        _ <- purgeLoop(users).background
        _ <- worker.run.background
        web <- Resource.eval(cfg.staticDir.traverse(spa))
        routes = new MonitorRoutes(
          monitors,
          notifiers,
          pages,
          nodes,
          senders,
          box,
          sess,
          announcer,
          Some(stui),
          cfg.probePolicy
        )
        all = ApiRoutes(oidc, users, sess, cfg.api, routes, Some(pushStore -> push), stui.routes) <+>
          Health.routes(Database.ping(xa)) <+> RumRoutes.routes <+> web.getOrElse(HttpRoutes.empty[IO])
        // セキュリティのヘッダーは、見つからない 404・内部エラーの 500 にも付ける
        httpApp <- HttpTelemetry(
          SecurityHeaders.app(SecurityHeaders.Policy(hsts = SecurityHeaders.hsts(cfg.api.cookieSecure)))(
            Errors.handle(all.orNotFound, ErrorCode.Unexpected)
          ),
          List("/api", "/auth", "/mcp", "/oauth"),
          sbirds.http.RateLimit.clientIp(_, cfg.api.rateLimit.trustedProxies)
        )
        _ <- EmberServerBuilder.default[IO]
          .withHost(cfg.bindHost)
          .withPort(Port.fromInt(cfg.port).getOrElse(port"8080"))
          .withShutdownTimeout(5.seconds)
          .withHttpApp(httpApp)
          .build
        _ <- Resource.eval(
          Log.println(s"[skotare] listening on :${cfg.port} (${cfg.api.publicUrl}, node ${cfg.node})")
        )
      } yield ()).useForever
    } yield ()

  /** DB が使えるまで待ってからマイグレーションする. */
  private def migrateWithRetry(cfg: AppConfig, attempt: Int = 1): IO[Int] =
    Database.migrate(cfg.db).handleErrorWith { e =>
      if attempt >= 60 then IO.raiseError(e)
      else
        Log.report(ErrorCode.DatabaseNotReady, s"${e.getMessage.take(120)}; retry $attempt/60") *>
          IO.sleep(2.seconds) *> migrateWithRetry(cfg, attempt + 1)
    }

  /** DB から定期的に読むメトリクス（skorimako のダッシュボード・アラート用）. */
  private def gauges(xa: doobie.Transactor[IO]): Resource[IO, Unit] = {
    def q(sqlText: doobie.Fragment): IO[Long] = sqlText.query[Long].unique.transact(xa)
    List(
      Metrics.gauge("skotare.users.total", "有効なユーザーの数")(q(sql"select count(*) from app_user where not disabled")),
      Metrics.gauge(
        "skotare.users.active_24h",
        "24時間以内にログインしたユーザーの数"
      )(q(sql"select count(*) from app_user where last_login_at > now() - interval '24 hours'")),
      Metrics.gauge("skotare.monitors.total", "有効な監視の数")(q(sql"select count(*) from monitor where enabled")),
      Metrics.gauge(
        "skotare.monitors.down",
        "停止している監視の数",
        30.seconds
      )(q(sql"select count(*) from monitor where status = 'down'")),
      Metrics.gauge(
        "skotare.incidents.open",
        "続いている障害の数",
        30.seconds
      )(q(sql"select count(*) from incident where resolved_at is null")),
      Metrics.gauge("skotare.checks.overdue", "動いている拠点で、確かめる時刻を大きく過ぎた（判定に数えない）監視の数（ノードが足りない）", 30.seconds)(
        q(fr"select count(*) from monitor_region mr join monitor m on m.id = mr.monitor_id join region r on r.name = mr.region" ++
          fr"where m.enabled and r.health <> 'offline' and" ++ NodeSql.stale)
      ),
      Metrics.gauge(
        "skotare.regions.unhealthy",
        "異常の疑い・停止の拠点の数",
        30.seconds
      )(q(sql"select count(*) from region where health <> 'ok'")),
      Metrics.gauge(
        "skotare.notifications.pending",
        "送れていない通知の数",
        30.seconds
      )(q(sql"select count(*) from notification_outbox where sent_at is null")),
      Metrics.gauge("skotare.nodes.alive", "動いているノードの数", 30.seconds)(q(fr"select count(*) from node where seen_at >" ++
        NodeSql.AliveWindow)),
      Metrics.gauge("skotare.push.subscriptions", "プッシュ通知を受け取る端末の数")(q(sql"select count(*) from push_subscription"))
    ).sequence_
  }

  /** 期限の切れたセッション・ログイン途中の状態を消す（1時間ごと）. */
  private def purgeLoop(users: UserStore): IO[Unit] =
    Periodic.run(1.minute, 1.hour, ErrorCode.SessionPurgeFailed)(IO.realTimeInstant.flatMap(users.purgeExpired)).void

  // IdP が http（開発環境など）のときは、そのアイコンの画像も許す（https の画像はもとから許している）
  private val idpImg: String = sys.env.get("OIDC_ISSUER").flatMap(u => scala.util.Try(java.net.URI.create(u)).toOption)
    .filter(u => u.getScheme == "http" && u.getAuthority != null).fold("")(u => s" http://${u.getAuthority}")

  /**
   * 画面の CSP. スクリプトは自分のサイトのものと、index.html に直接書いたもの（テーマの読み込み. ハッシュで許す）だけ.
   * skorimako のブラウザの観測（RUM）が設定されていれば、その rum.js の読み込みと送信も許す（rum はそのオリジン）.
   * 画像はアイコン（IdP の画像は外部の https）のため広めにする. Service Worker・manifest は自分のサイトなので default-src で足りる.
   */
  def spaCsp(indexHtml: String, rum: Option[String] = RumRoutes.origin): String = {
    val inline = Spa.inlineScriptHashes(indexHtml)
    (List(
      "default-src 'self'",
      ("script-src 'self'" :: inline ++ rum.toList).mkString(" "),
      "style-src 'self' 'unsafe-inline'",
      s"img-src 'self' data: blob: https:$idpImg",
      ("connect-src 'self'" :: rum.toList).mkString(" "),
      "object-src 'none'",
      "base-uri 'self'",
      "frame-ancestors 'none'",
      "form-action 'self'"
    )).mkString("; ")
  }

  /** ビルド済みの画面の配信（画面の CSP を付ける）. 存在しないパスは index.html（SPA）. */
  private def spa(dir: String): IO[HttpRoutes[IO]] =
    Spa.indexHtml(dir).map { indexHtml =>
      val csp = spaCsp(indexHtml)
      Spa.routes(
        dir,
        Spa.firstSegment("api", "auth", "mcp", "oauth", "healthz"),
        notFound = Errors.respond(ErrorCode.IndexHtmlMissing),
        finish = (_, r) => r.putHeaders("Content-Security-Policy" -> csp)
      )
    }
}
