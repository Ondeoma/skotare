package skotare.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import sbirds.http.BodyLimit
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
    // 本文は MaxBody まで（大きな本文でメモリを使い切らせない. アイコンの 1MB より大きくしておく）. 超えていれば 413
    val all = BodyLimit(MaxBody, ErrorCode.RequestTooLarge)(
      rest <+> new McpRoutes(rest, sess, cfg.publicUrl, cfg.version).routes
    )
    // セキュリティのヘッダーは、起動するところ（Main）でアプリ全体に付ける
    // IP ごとの回数の制限（数え方は全サービス共通. sbirds-lib の RateLimit.standard）
    sbirds.http.RateLimit.standard(all, cfg.rateLimit, ErrorCode.TooManyRequests)()
  }

  val MaxBody: Long = 2L * 1024 * 1024
}
