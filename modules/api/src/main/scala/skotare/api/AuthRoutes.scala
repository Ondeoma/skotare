package skotare.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Location
import sbirds.auth.{Crypto, LoginDenied, OidcClient}
import skotare.auth.ApiTokens
import skotare.db.*
import skotare.shared.*
import skotare.core.ErrorCode
import sbirds.http.Errors
import sbirds.shared.AppError

import java.time.Instant
import java.util.UUID

/** ログイン（OIDC・開発用）・ログアウト・自分の情報とアイコン・ユーザーの検索. */
final class AuthRoutes(oidc: Option[OidcClient], users: UserStore, val sessions: SessionAuth, cfg: ApiConfig)
  extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "auth" / "login" => getAuthLogin(req)
    case req @ GET -> Root / "auth" / "callback" => getAuthCallback(req)
    case req @ POST -> Root / "auth" / "logout" if !fromScreen(req) => rejectLogoutNotFromScreen(req)
    case req @ POST -> Root / "auth" / "logout" => postAuthLogout(req)
    case req @ POST -> Root / "auth" / "dev-login" if cfg.devLogin && oidc.isEmpty => postAuthDevLogin(req)
    case GET -> Root / "api" / "client-config" => getClientConfig()
    case req @ GET -> Root / "api" / "me" => getMe(req)
    case req @ PATCH -> Root / "api" / "me" => patchMe(req)
    case req @ PUT -> Root / "api" / "me" / "avatar" => putMeAvatar(req)
    case req @ DELETE -> Root / "api" / "me" / "avatar" => deleteMeAvatar(req)
    case req @ GET -> Root / "api" / "users" / UuidVar(id) / "avatar" => getUserAvatar(req, id)
    case req @ GET -> Root / "api" / "tokens" => getTokens(req)
    case req @ POST -> Root / "api" / "tokens" => postTokens(req)
    case req @ DELETE -> Root / "api" / "tokens" / UuidVar(id) => deleteToken(req, id)
  }

  private def getAuthLogin(req: Request[IO]): IO[Response[IO]] = {
    val back = SafePath.of(param(req, "returnTo"))
    oidc match {
      case Some(o) =>
        val state = Crypto.randomToken(); val verifier = Crypto.randomToken(48); val nonce = Crypto.randomToken()
        users.saveLoginState(state, verifier, nonce, back) *>
          o.authorizationUri(state, verifier, nonce).flatMap(u =>
            redirect(u.renderString, Some(loginCookie(state, 600)))
          )
      case None if cfg.devLogin => redirect(s"/login?returnTo=${java.net.URLEncoder.encode(back, "UTF-8")}")
      case None => err(ErrorCode.OidcNotConfigured)
    }
  }

  private def getAuthCallback(req: Request[IO]): IO[Response[IO]] =
    (oidc, param(req, "code"), param(req, "state"), param(req, "error")) match {
      case (_, _, _, Some(e)) =>
        // IdP からの説明は URL で差し替えられるので画面には出さず、ログにだけ残す
        loginFailed(
          req,
          ErrorCode.LoginRejectedByIdp,
          s"${e.take(100)} ${param(req, "error_description").getOrElse("").take(300)}"
        )
      case (Some(_), Some(_), Some(state), None) if !sameBrowser(req, state) =>
        loginFailed(req, ErrorCode.LoginBrowserMismatch)
      case (Some(o), Some(code), Some(state), None) =>
        users.takeLoginState(state).flatMap {
          case None => loginFailed(req, ErrorCode.LoginExpired)
          case Some((verifier, nonce, back)) =>
            o.authenticate(code, verifier, nonce, Instant.now()).attempt.flatMap {
              // 理由の詳細（検証に失敗した項目など）はログにだけ出す
              case Left(e: LoginDenied) => loginFailed(req, ErrorCode.LoginDenied, e.getMessage, e.getMessage)
              case Left(e) =>
                loginFailed(req, ErrorCode.LoginFailed, e.getMessage)
              case Right(res) =>
                val id = res.identity
                users.upsert(
                  id.sub,
                  id.name,
                  id.email,
                  id.picture.filter(p => p.startsWith("https://") || p.startsWith("http://"))
                ).flatMap { u =>
                  if u.disabled then loginFailed(req, ErrorCode.UserDisabled)
                  else
                    loggedIn(u, Some(res.idToken)).flatMap(c =>
                      redirect(back, Some(c)).map(_.addCookie(loginCookie("", 0)))
                    )
                }
            }
        }
      case (None, _, _, _) => err(ErrorCode.OidcNotConfigured)
      case _ => err(ErrorCode.LoginCodeStateRequired)
    }

  private def rejectLogoutNotFromScreen(req: Request[IO]): IO[Response[IO]] =
    err(ErrorCode.LogoutNotFromScreen)

  private def postAuthLogout(req: Request[IO]): IO[Response[IO]] =
    sessions.revoke(req).flatMap {
      (idToken, c) =>
        oidc.traverse(_.logoutUri(idToken, cfg.publicUrl + "/")).map(_.flatten).flatMap { idp =>
          Ok(Json.obj("redirect" -> idp.fold("/login?loggedOut=1")(_.renderString).asJson)).map(_.addCookie(c))
        }
    }

  private def postAuthDevLogin(req: Request[IO]): IO[Response[IO]] =
    body[Json](req) { j =>
      val c = j.hcursor
      val name = c.get[String]("name").getOrElse("").trim
      if name.isEmpty then err(ErrorCode.NameRequired)
      else
        for {
          u <- users.upsert(
            s"dev:$name",
            name,
            c.get[String]("email").toOption.map(_.trim).filter(_.contains("@")),
            None
          )
          ck <- loggedIn(u, None)
          r <- Ok(Json.obj("redirect" -> SafePath.of(c.get[String]("returnTo").toOption).asJson)).map(_.addCookie(ck))
        } yield r
    }

  private def getClientConfig(): IO[Response[IO]] =
    Ok(ClientConfigDto(cfg.devLogin && oidc.isEmpty, oidc.isDefined, cfg.version, cfg.stui.isDefined))

  private def getMe(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u => sessions.appNames(req).flatMap(n => Ok(u.me.copy(appNames = n))))

  private def patchMe(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u =>
      body[UpdateMeReq](req) { r =>
        val tz = r.timezone.map(_.trim).filter(_.nonEmpty)
        if tz.exists(z => scala.util.Try(java.time.ZoneId.of(z)).isFailure) then
          err(ErrorCode.TimezoneInvalid)
        else if r.name.exists(_.trim.length > 100) then err(ErrorCode.NameTooLong)
        else users.updateMe(u.id, r.name, tz) *> me(u)
      }
    )

  private def putMeAvatar(req: Request[IO]): IO[Response[IO]] =
    authed(req) { u =>
      req.body.take(AvatarImage.MaxBytes + 1L).compile.to(Array).flatMap { data =>
        if data.length > AvatarImage.MaxBytes then err(ErrorCode.AvatarTooLarge)
        else
          AvatarImage.detect(data).fold(err(ErrorCode.AvatarTypeUnsupported)) { ct =>
            users.saveAvatar(u.id, ct, data) *> me(u)
          }
      }
    }

  private def deleteMeAvatar(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u => users.deleteAvatar(u.id) *> me(u))

  private def getUserAvatar(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req) { _ =>
      users.avatar(id).flatMap {
        case None => err(ErrorCode.AvatarNotFound)
        case Some((ct, data)) =>
          // JSON の変換が優先されないよう、バイト列のまま返す
          IO.pure(Response[IO](Status.Ok).withBodyStream(fs2.Stream.emits(data)).putHeaders(
            headers.`Content-Type`(MediaType.unsafeParse(ct)),
            "Cache-Control" -> "private, max-age=31536000, immutable",
            "X-Content-Type-Options" -> "nosniff",
            "Content-Security-Policy" -> "default-src 'none'"
          ))
      }
    }

  private def getTokens(req: Request[IO]): IO[Response[IO]] =
    interactive(req)(u => users.tokens(u.id).flatMap(Ok(_)))

  private def postTokens(req: Request[IO]): IO[Response[IO]] =
    interactive(req)(u =>
      body[ApiTokenReq](req) { r =>
        val name = r.name.trim
        if name.isEmpty || name.length > 100 then err(ErrorCode.ApiTokenNameInvalid)
        else if !TokenScope.all.contains(r.scope) then err(ErrorCode.ApiTokenScopeInvalid)
        else if r.expiresInDays.exists(d => d < 1 || d > 3650) then err(ErrorCode.ApiTokenExpiryInvalid)
        else {
          val (token, prefix) = ApiTokens.generate()
          users.createToken(
            u.id,
            name,
            prefix,
            Crypto.sha256(token),
            r.scope,
            r.expiresInDays.map(d => Instant.now().plusSeconds(d * 86400L))
          )
            .flatMap(t => Created(ApiTokenCreated(t, token)))
        }
      }
    )

  private def deleteToken(req: Request[IO], id: UUID): IO[Response[IO]] =
    interactive(req)(u => users.revokeToken(u.id, id).ifM(NoContent(), err(ErrorCode.ApiTokenNotFound)))

  private def redirect(to: String, cookie: Option[ResponseCookie] = None): IO[Response[IO]] = {
    // SafePath は % を通すので、URL として読めないものは / に戻す
    val r = Response[IO](Status.Found).putHeaders(
      Location(Uri.fromString(to).getOrElse(Uri(path = Uri.Path.Root))),
      "Cache-Control" -> "no-store"
    )
    IO.pure(cookie.fold(r)(r.addCookie))
  }

  /**
   * ログインを始めたブラウザの印（state の SHA-256）. IdP から戻ったとき、同じブラウザかを確かめる
   * （攻撃者が途中で止めたコールバックの URL を踏ませて、攻撃者のアカウントでログインさせる「ログイン CSRF」を防ぐ）.
   */
  private val LoginCookie = "skotare_login"
  private def loginCookie(state: String, maxAge: Long) =
    SessionAuth.cookie(
      cfg,
      LoginCookie,
      if state.isEmpty then "" else Crypto.hex(Crypto.sha256(state)),
      maxAge,
      "/auth"
    )
  private def sameBrowser(req: Request[IO], state: String): Boolean =
    req.cookies.find(_.name == LoginCookie).exists(c =>
      java.security.MessageDigest.isEqual(c.content.getBytes, Crypto.hex(Crypto.sha256(state)).getBytes)
    )

  /** 変更した後の自分の情報. */
  private def me(u: User): IO[Response[IO]] = users.find(u.id).flatMap(x => Ok(x.getOrElse(u).me))

  /** セッションを発行する. */
  private def loggedIn(u: User, idToken: Option[String]): IO[ResponseCookie] =
    sessions.issue(u, idToken) <*
      sbirds.observe.Metrics.count("skotare.logins", "via" -> idToken.fold("dev")(_ => "oidc"))

  private def page(title: String, msg: String, status: Status): IO[Response[IO]] = {
    val esc = (s: String) => Html.escape(s)
    IO.pure(TextResponse(
      status,
      s"""<!doctype html><html lang="ja"><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>${esc(
          title
        )}</title>
         |<body style="font-family:system-ui,sans-serif;max-width:480px;margin:12vh auto;padding:0 1rem">
         |<h1 style="font-size:1.3rem">${esc(title)}</h1><p>${esc(
          msg
        )}</p><p><a href="/auth/login">もう一度ログインする</a></p></body></html>""".stripMargin,
      MediaType.text.html
    ))
  }

  /** ログインできなかったときの画面（コード付きでログに出す. detail はログにだけ出し、画面には shown（省くとコードの文言）を出す）. */
  private def loginFailed(req: Request[IO],
                          code: ErrorCode,
                          detail: String | Null = null,
                          shown: String | Null = null): IO[Response[IO]] =
    Errors.log(AppError(code, Option(detail)), Some(req)) *>
      page(
        "ログインできませんでした",
        Option(shown).getOrElse(code.message),
        Status.fromInt(code.status).getOrElse(Status.BadRequest)
      )

}
