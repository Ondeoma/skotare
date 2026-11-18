package skotare.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Location
import skotare.auth.{ApiTokens, Crypto, LoginDenied, OidcClient}
import skotare.db.*
import skotare.shared.*

import java.time.Instant

/** ログイン（OIDC・開発用）・ログアウト・自分の情報とアイコン・ユーザーの検索. */
final class AuthRoutes(oidc: Option[OidcClient], users: UserStore, val sessions: SessionAuth, cfg: ApiConfig) extends RouteSupport:

  private def redirect(to: String, cookie: Option[ResponseCookie] = None): IO[Response[IO]] =
    // SafePath は % を通すので、URL として読めないものは / に戻す
    val r = Response[IO](Status.Found).putHeaders(Location(Uri.fromString(to).getOrElse(Uri(path = Uri.Path.Root))), "Cache-Control" -> "no-store")
    IO.pure(cookie.fold(r)(r.addCookie))

  /**
   * ログインを始めたブラウザの印（state の SHA-256）. IdP から戻ったとき、同じブラウザかを確かめる
   * （攻撃者が途中で止めたコールバックの URL を踏ませて、攻撃者のアカウントでログインさせる「ログイン CSRF」を防ぐ）.
   */
  private val LoginCookie = "skotare_login"
  private def loginCookie(state: String, maxAge: Long) =
    SessionAuth.cookie(cfg, LoginCookie, if state.isEmpty then "" else Crypto.hex(Crypto.sha256(state)), maxAge, "/auth")
  private def sameBrowser(req: Request[IO], state: String): Boolean =
    req.cookies.find(_.name == LoginCookie).exists(c => java.security.MessageDigest.isEqual(c.content.getBytes, Crypto.hex(Crypto.sha256(state)).getBytes))

  /** 変更した後の自分の情報. */
  private def me(u: User): IO[Response[IO]] = users.find(u.id).flatMap(x => Ok(x.getOrElse(u).me))

  /** セッションを発行する. */
  private def loggedIn(u: User, idToken: Option[String]): IO[ResponseCookie] =
    sessions.issue(u, idToken) <* skotare.observe.Metrics.count("skotare.logins", "via" -> idToken.fold("dev")(_ => "oidc"))

  private def page(title: String, msg: String, status: Status): IO[Response[IO]] =
    val esc = (s: String) => s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")
    IO.pure(TextResponse(status,
      s"""<!doctype html><html lang="ja"><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>${esc(title)}</title>
         |<body style="font-family:system-ui,sans-serif;max-width:480px;margin:12vh auto;padding:0 1rem">
         |<h1 style="font-size:1.3rem">${esc(title)}</h1><p>${esc(msg)}</p><p><a href="/auth/login">もう一度ログインする</a></p></body></html>""".stripMargin,
      MediaType.text.html))

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "auth" / "login" =>
      val back = SafePath.of(param(req, "returnTo"))
      oidc match
        case Some(o) =>
          val state = Crypto.randomToken(); val verifier = Crypto.randomToken(48); val nonce = Crypto.randomToken()
          users.saveLoginState(state, verifier, nonce, back) *>
            o.authorizationUri(state, verifier, nonce).flatMap(u => redirect(u.renderString, Some(loginCookie(state, 600))))
        case None if cfg.devLogin => redirect(s"/login?returnTo=${java.net.URLEncoder.encode(back, "UTF-8")}")
        case None                 => err(Status.ServiceUnavailable, "OIDC が設定されていません")

    // IdP から戻ってきたところ（認可コードを ID トークンに交換して、署名・iss・aud・exp・nonce を確かめる）
    case req @ GET -> Root / "auth" / "callback" =>
      (oidc, param(req, "code"), param(req, "state"), param(req, "error")) match
        case (_, _, _, Some(e)) =>
          // IdP からの説明は URL で差し替えられるので画面には出さず、ログにだけ残す
          skotare.observe.Log.println(s"[skotare] login error from IdP: ${e.take(100)} ${param(req, "error_description").getOrElse("").take(300)}") *>
            page("ログインできませんでした", "IdP がログインを断りました。時間をおいてもう一度試すか、管理者に問い合わせてください。", Status.Unauthorized)
        case (Some(_), Some(_), Some(state), None) if !sameBrowser(req, state) =>
          page("ログインできませんでした", "ログインを始めたブラウザと違います。もう一度ログインしてください。", Status.BadRequest)
        case (Some(o), Some(code), Some(state), None) =>
          users.takeLoginState(state).flatMap {
            case None => page("ログインできませんでした", "ログインの有効期限が切れました。もう一度ログインしてください。", Status.BadRequest)
            case Some((verifier, nonce, back)) =>
              o.authenticate(code, verifier, nonce, Instant.now()).attempt.flatMap {
                // 理由の詳細（検証に失敗した項目など）はログにだけ出す
                case Left(e: LoginDenied) => page("ログインできませんでした", e.getMessage, Status.Forbidden)
                case Left(e) =>
                  skotare.observe.Log.println(s"[skotare] login failed: ${e.getMessage}") *>
                    page("ログインできませんでした", "IdP からの応答を確かめられませんでした。時間をおいてもう一度試してください。", Status.Unauthorized)
                case Right(res) =>
                  val id = res.identity
                  users.upsert(id.sub, id.name, id.email, id.picture.filter(p => p.startsWith("https://") || p.startsWith("http://"))).flatMap { u =>
                    if u.disabled then page("ログインできませんでした", "このユーザーは無効にされています。", Status.Forbidden)
                    else loggedIn(u, Some(res.idToken)).flatMap(c => redirect(back, Some(c)).map(_.addCookie(loginCookie("", 0))))
                  }
              }
          }
        case (None, _, _, _) => err(Status.ServiceUnavailable, "OIDC が設定されていません")
        case _               => err(Status.BadRequest, "code と state が必要です")

    // ログアウト: セッションを消し、IdP にもログアウトを頼む（end_session_endpoint があれば）
    // ほかのサイトから勝手にログアウトさせられないよう、画面からの要求（X-Skotare）だけを受け付ける
    case req @ POST -> Root / "auth" / "logout" if !fromScreen(req) => err(Status.Forbidden, "画面から操作してください")
    case req @ POST -> Root / "auth" / "logout" =>
      sessions.revoke(req).flatMap { (idToken, c) =>
        oidc.traverse(_.logoutUri(idToken, cfg.publicUrl + "/")).map(_.flatten).flatMap { idp =>
          Ok(Json.obj("redirect" -> idp.fold("/login?loggedOut=1")(_.renderString).asJson)).map(_.addCookie(c))
        }
      }

    // 開発用: 名前だけでログインする（DEV_LOGIN=true で OIDC を設定していないときだけ）
    case req @ POST -> Root / "auth" / "dev-login" if cfg.devLogin && oidc.isEmpty =>
      body[Json](req) { j =>
        val c    = j.hcursor
        val name = c.get[String]("name").getOrElse("").trim
        if name.isEmpty then err(Status.BadRequest, "名前を入力してください")
        else
          for
            u  <- users.upsert(s"dev:$name", name, c.get[String]("email").toOption.map(_.trim).filter(_.contains("@")), None)
            ck <- loggedIn(u, None)
            r  <- Ok(Json.obj("redirect" -> SafePath.of(c.get[String]("returnTo").toOption).asJson)).map(_.addCookie(ck))
          yield r
      }

    case GET -> Root / "api" / "client-config" =>
      Ok(ClientConfigDto(cfg.devLogin && oidc.isEmpty, oidc.isDefined, cfg.version))

    case req @ GET -> Root / "api" / "me" => authed(req)(u => Ok(u.me))

    case req @ PATCH -> Root / "api" / "me" =>
      authed(req)(u => body[UpdateMeReq](req) { r =>
        val tz = r.timezone.map(_.trim).filter(_.nonEmpty)
        if tz.exists(z => scala.util.Try(java.time.ZoneId.of(z)).isFailure) then err(Status.BadRequest, "タイムゾーンが正しくありません")
        else if r.name.exists(_.trim.length > 100) then err(Status.BadRequest, "名前は100文字以内にしてください")
        else users.updateMe(u.id, r.name, tz) *> me(u)
      })

    // アイコン: 画像そのもの（PNG・JPEG・GIF・WebP、1MB まで）を本文にして送る. 消すと IdP の画像に戻る
    case req @ PUT -> Root / "api" / "me" / "avatar" =>
      authed(req) { u =>
        req.body.take(AvatarImage.MaxBytes + 1L).compile.to(Array).flatMap { data =>
          if data.length > AvatarImage.MaxBytes then err(Status.PayloadTooLarge, "画像は1MBまでにしてください")
          else AvatarImage.detect(data).fold(err(Status.UnsupportedMediaType, "PNG・JPEG・GIF・WebP の画像を選んでください")) { ct =>
            users.saveAvatar(u.id, ct, data) *> me(u)
          }
        }
      }

    case req @ DELETE -> Root / "api" / "me" / "avatar" =>
      authed(req)(u => users.deleteAvatar(u.id) *> me(u))

    // URL の v（版）が変われば別の画像なので、長くキャッシュしてよい
    case req @ GET -> Root / "api" / "users" / UuidVar(id) / "avatar" =>
      authed(req) { _ =>
        users.avatar(id).flatMap {
          case None => err(Status.NotFound, "アイコンがありません")
          case Some((ct, data)) =>
            // JSON の変換が優先されないよう、バイト列のまま返す
            IO.pure(Response[IO](Status.Ok).withBodyStream(fs2.Stream.emits(data)).putHeaders(
              headers.`Content-Type`(MediaType.unsafeParse(ct)), "Cache-Control" -> "private, max-age=31536000, immutable",
              "X-Content-Type-Options" -> "nosniff", "Content-Security-Policy" -> "default-src 'none'"))
        }
      }


    // ------------------------------------------------------------ API トークン（画面からだけ管理できる）

    case req @ GET -> Root / "api" / "tokens" => interactive(req)(u => users.tokens(u.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "tokens" =>
      interactive(req)(u => body[ApiTokenReq](req) { r =>
        val name = r.name.trim
        if name.isEmpty || name.length > 100 then err(Status.BadRequest, "名前を100文字以内で入れてください")
        else if !TokenScope.all.contains(r.scope) then err(Status.BadRequest, "スコープは read か write です")
        else if r.expiresInDays.exists(d => d < 1 || d > 3650) then err(Status.BadRequest, "有効期限は1〜3650日です")
        else
          val (token, prefix) = ApiTokens.generate()
          users.createToken(u.id, name, prefix, Crypto.sha256(token), r.scope, r.expiresInDays.map(d => Instant.now().plusSeconds(d * 86400L)))
            .flatMap(t => Created(ApiTokenCreated(t, token)))
      })

    case req @ DELETE -> Root / "api" / "tokens" / UuidVar(id) =>
      interactive(req)(u => users.revokeToken(u.id, id).ifM(NoContent(), err(Status.NotFound, "API トークンが見つかりません")))
  }
