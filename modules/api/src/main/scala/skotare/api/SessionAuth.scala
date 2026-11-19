package skotare.api

import cats.effect.*
import org.http4s.*
import org.http4s.headers.Authorization
import sbirds.auth.Crypto
import skotare.auth.ApiTokens
import skotare.db.*

import java.time.Instant

/** 認証: Cookie のセッション、または `Authorization: Bearer <API トークン>`（どちらも DB にはハッシュだけ保存する）. */
final class SessionAuth(users: UserStore, cfg: ApiConfig) {

  /** Bearer の値（API トークンの形のものだけ）. */
  def bearer(req: Request[IO]): Option[String] =
    req.headers.get[Authorization].collect { case Authorization(Credentials.Token(AuthScheme.Bearer, t)) => t.trim }

  def fromRequest(req: Request[IO]): IO[Option[Principal]] =
    bearer(req) match {
      case Some(t) if ApiTokens.looksLike(t) =>
        users.findToken(Crypto.sha256(t), Instant.now()).map(_.map((u, id, scope) => Principal(u, Some(id -> scope))))
      case Some(_) => IO.pure(None)
      case None =>
        req.cookies.find(_.name == cfg.cookieName).map(_.content).filter(_.nonEmpty) match {
          case None => IO.pure(None)
          case Some(token) => users.findSession(Crypto.sha256(token), Instant.now()).map(_.map(Principal(_, None)))
        }
    }

  def issue(user: User, idToken: Option[String]): IO[ResponseCookie] = {
    val token = Crypto.randomToken()
    users.createSession(Crypto.sha256(token), user.id, idToken, Instant.now().plusSeconds(cfg.sessionTtl.toSeconds))
      .as(cookie(token, cfg.sessionTtl.toSeconds))
  }

  /** sbirds のアプリの表示名（ログインしたときの ID トークンにある. API トークンや開発用のログインでは空）. */
  def appNames(req: Request[IO]): IO[Map[String, String]] =
    req.cookies.find(_.name == cfg.cookieName) match {
      case Some(c) =>
        users.sessionIdToken(Crypto.sha256(c.content)).map(_.fold(Map.empty)(sbirds.auth.AppNames.fromIdToken))
      case None => IO.pure(Map.empty)
    }

  /** セッションを消す. IdP のログアウトに渡す ID トークンと、Cookie を消す指示を返す. */
  def revoke(req: Request[IO]): IO[(Option[String], ResponseCookie)] =
    req.cookies.find(_.name == cfg.cookieName) match {
      case Some(c) => users.deleteSession(Crypto.sha256(c.content)).map(_ -> cookie("", 0))
      case None => IO.pure(None -> cookie("", 0))
    }

  private def cookie(value: String, maxAge: Long): ResponseCookie =
    SessionAuth.cookie(cfg, cfg.cookieName, value, maxAge, "/")
}

object SessionAuth {

  /** HttpOnly・SameSite=Lax の Cookie（https なら Secure）. */
  def cookie(cfg: ApiConfig, name: String, value: String, maxAge: Long, path: String): ResponseCookie =
    ResponseCookie(
      name,
      value,
      maxAge = Some(maxAge),
      path = Some(path),
      httpOnly = true,
      secure = cfg.cookieSecure,
      sameSite = Some(SameSite.Lax)
    )
}
