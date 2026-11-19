package skotare.api

import cats.effect.*
import org.http4s.*
import skotare.db.*
import skotare.shared.*

import java.util.UUID
import scala.concurrent.duration.*

final case class ApiConfig(cookieSecure: Boolean,
                           publicUrl: String,
                           sessionTtl: FiniteDuration = 14.days,
                           cookieName: String = "skotare_session",
                           devLogin: Boolean = false,
                           rateLimit: RateLimitConfig = RateLimitConfig(),
                           version: String = "0.1.0",
                           oauthClients: List[OAuthClient] = Nil,
                           stui: Option[LinkService] = None)

/**
 * skotare が利用者の権限で使う兄弟のサービス（OAuth のクライアント側. 今は stui だけ）.
 * url は利用者のブラウザが開く URL、internalUrl はサーバーから呼ぶ URL（なければ url）.
 */
final case class LinkService(provider: String,
                             url: String,
                             clientId: String,
                             clientSecret: String,
                             internalUrl: Option[String] = None) {
  def apiUrl: String = internalUrl.getOrElse(url)
}

/** skotare を利用者の権限で使う外部のサービス（OAuth のクライアント. 環境変数 OAUTH_CLIENTS）. scopes はこのクライアントに許すスコープ. */
final case class OAuthClient(id: String, secret: String, redirectUri: String, name: String, scopes: List[String])

/** HTML に埋め込む文字のエスケープ. */
object Html {
  def escape(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;").replace("'", "&#39;")
}

/** 誰の要求か. token は API トークンで呼ばれたときの (トークンID, スコープ). */
final case class Principal(user: User, token: Option[(UUID, String)]) {
  def canWrite: Boolean = token.forall(_._2 == TokenScope.Write)
}

/** 文字列の応答（JSON 以外. circe の EntityEncoder に取られないよう明示する）. */
object TextResponse {
  def apply(status: Status, body: String, media: MediaType = MediaType.text.plain): Response[IO] =
    Response[IO](status).withEntity(body)(using EntityEncoder.stringEncoder).withContentType(headers.`Content-Type`(
      media,
      Charset.`UTF-8`
    ))
}

/** 入力の確認. */
object Check {

  /** 名前・題名（前後の空白を除いて1〜100文字）. */
  def name(v: String, what: String = "名前"): Either[String, String] =
    Either.cond(v.trim.nonEmpty && v.trim.length <= 100, v.trim, s"${what}を100文字以内で入れてください")

  def text(v: String, what: String, max: Int): Either[String, String] =
    Either.cond(v.length <= max, v, s"$what は $max 文字以内にしてください")
}
