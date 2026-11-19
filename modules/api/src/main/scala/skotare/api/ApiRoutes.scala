package skotare.api

import cats.data.OptionT
import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.server.middleware.EntityLimiter
import sbirds.http.Errors
import skotare.core.ErrorCode
import skotare.db.*

/** アプリのルートをまとめる（REST API・OAuth の認可・MCP・プッシュ通知の購読）. MCP は REST API をトークンで内部から呼ぶ. */
object ApiRoutes {
  def apply(oidc: Option[sbirds.auth.OidcClient],
            users: UserStore,
            sess: SessionAuth,
            cfg: ApiConfig,
            monitorRoutes: MonitorRoutes,
            push: Option[(sbirds.push.PushStore, sbirds.push.PushService)] = None,
            extra: HttpRoutes[IO] = HttpRoutes.empty[IO]): HttpRoutes[IO] = {
    val rest = new AuthRoutes(oidc, users, sess, cfg).routes <+> new OAuthRoutes(users, sess, cfg).routes <+>
      monitorRoutes.routes <+> extra <+>
      push.fold(HttpRoutes.empty[IO])((store, service) => new PushRoutes(store, service, sess).routes)
    // 本文は MaxBody まで（大きな本文でメモリを使い切らせない. アイコンの 1MB より大きくしておく）.
    // Content-Length が超えていれば、EntityLimiter はルートに入る前に例外にするので、ここで 413 にする
    val limited =
      EntityLimiter.httpRoutes(rest <+> new McpRoutes(rest, sess, cfg.publicUrl, cfg.version).routes, MaxBody)
    val all = HttpRoutes[IO](req =>
      limited(req).recoverWith { case _: EntityLimiter.EntityTooLarge => OptionT.liftF(tooLarge) }
    )
    // セキュリティのヘッダーは、起動するところ（Main）でアプリ全体に付ける
    RateLimit(all, cfg.rateLimit)
  }

  val MaxBody: Long = 2L * 1024 * 1024

  private def tooLarge: IO[Response[IO]] =
    Errors.respond(ErrorCode.RequestTooLarge)
}
