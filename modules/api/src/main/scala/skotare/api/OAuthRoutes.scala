package skotare.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.{`Content-Type`, Authorization, Location}
import sbirds.auth.Crypto
import skotare.auth.ApiTokens
import skotare.db.*
import skotare.shared.*
import skotare.core.ErrorCode

import java.time.Instant

/**
 * 外部のサービス（skakapo など）への認可（OAuth 2.0 の認可コード + PKCE（S256）. RFC 6749・7636・7009）と、連携の確認・取り消し.
 * 発行するアクセストークンは、そのユーザーの API トークン（REST API と MCP をその利用者の権限で使える. 1時間で切れる）と、
 * 新しい組に替える refresh token（使うたびに替わる. 最後の更新から90日使わなければ切れる）.
 * スコープは read（参照のみ）と write（読み書き. read を含む）. クライアントに許した範囲を超える要求は invalid_scope.
 */
final class OAuthRoutes(users: UserStore, val sessions: SessionAuth, cfg: ApiConfig) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "oauth" / "authorize" => getOauthAuthorize(req)
    case req @ POST -> Root / "oauth" / "authorize" => postOauthAuthorize(req)
    case req @ POST -> Root / "oauth" / "token" => postOauthToken(req)
    case req @ POST -> Root / "oauth" / "revoke" => postOauthRevoke(req)
    case req @ GET -> Root / "api" / "oauth" / "grants" => getOauthGrants(req)
    case req @ DELETE -> Root / "api" / "oauth" / "grants" / clientId => deleteOauthGrant(req, clientId)
  }

  private def getOauthAuthorize(req: Request[IO]): IO[Response[IO]] =
    validate(param(req, _)) match {
      case Left(r) => r
      case Right((c, rs, _, _)) =>
        sessions.fromRequest(req).flatMap {
          // ログインしてからこの画面に戻る（API トークンでは許可できない）
          case Some(p) if p.token.isEmpty =>
            val csrf = Crypto.randomToken(24)
            val hidden = List(
              "client_id",
              "redirect_uri",
              "state",
              "code_challenge",
              "code_challenge_method",
              "response_type",
              "scope"
            )
              .flatMap(k =>
                param(req, k).map(v => s"""<input type="hidden" name="$k" value="${Html.escape(v)}">""")
              ).mkString
            consent(c, rs, p.user, hidden, csrf).map(_.addCookie(ResponseCookie(
              CsrfCookie,
              csrf,
              path = Some("/oauth"),
              httpOnly = true,
              secure = cfg.cookieSecure,
              sameSite = Some(SameSite.Strict),
              maxAge = Some(600)
            )))
          case _ =>
            IO.pure(Response[IO](Status.Found).putHeaders(
              Location(Uri.unsafeFromString(s"/auth/login?returnTo=${enc(req.uri.renderString)}")),
              "Cache-Control" -> "no-store"
            ))
        }
    }

  private def postOauthAuthorize(req: Request[IO]): IO[Response[IO]] =
    req.as[UrlForm].flatMap { u =>
      val p = (k: String) => u.getFirst(k).map(_.trim).filter(_.nonEmpty)
      validate(p) match {
        case Left(r) => r
        case Right((c, rs, state, challenge)) =>
          val csrfOk = req.cookies.find(_.name == CsrfCookie).exists(ck =>
            p("csrf").exists(v => Crypto.constantTimeEquals(ck.content, v))
          )
          sessions.fromRequest(req).flatMap {
            case Some(s) if s.token.isEmpty && !csrfOk => errorPage("画面の有効期限が切れました。連携元の画面からやり直してください")
            case Some(s) if s.token.isEmpty && p("decision").contains("allow") =>
              val code = Crypto.randomToken(32)
              users.createCode(
                Crypto.sha256(code),
                OAuthCode(c.id, c.redirectUri, s.user.id, challenge, tokenScope(rs)),
                Instant.now().plusSeconds(CodeTtlSeconds)
              ) *>
                sbirds.observe.Log.println(
                  s"[skotare] oauth: ${s.user.id} authorized ${c.id} (${granted(tokenScope(rs))})"
                ) *>
                back(c.redirectUri, "code" -> Some(code), "state" -> state)
            case Some(s) if s.token.isEmpty => back(c.redirectUri, "error" -> Some("access_denied"), "state" -> state)
            case _ => errorPage("ログインが切れました。連携元の画面からやり直してください")
          }.map(_.removeCookie(ResponseCookie(CsrfCookie, "", path = Some("/oauth"))))
      }
    }

  private def postOauthToken(req: Request[IO]): IO[Response[IO]] =
    basicClient(req) match {
      case None => oauthError(Status.Unauthorized, "invalid_client", "クライアントの認証に失敗しました")
      case Some(c) =>
        form(req) { f =>
          f.get("grant_type") match {
            case Some("authorization_code") => byCode(c, f)
            case Some("refresh_token") => byRefresh(c, f)
            case g => oauthError(
                Status.BadRequest,
                if g.isEmpty then "invalid_request" else "unsupported_grant_type",
                "grant_type は authorization_code か refresh_token です"
              )
          }
        }
    }

  private def postOauthRevoke(req: Request[IO]): IO[Response[IO]] =
    basicClient(req) match {
      case None => oauthError(Status.Unauthorized, "invalid_client", "クライアントの認証に失敗しました")
      case Some(c) => form(req)(f =>
          f.get("token").traverse_(t => users.revokeOAuthToken(Crypto.sha256(t), c.id)) *> Ok(Json.obj())
        )
    }

  private def getOauthGrants(req: Request[IO]): IO[Response[IO]] =
    interactive(req)(u =>
      users.grants(u.id).flatMap(gs =>
        Ok(gs.map {
          (id, scope, at, used) =>
            OAuthGrantDto(id, client(id).fold(id)(_.name), scope, at.toString, used.map(_.toString))
        })
      )
    )

  private def deleteOauthGrant(req: Request[IO], clientId: String): IO[Response[IO]] =
    interactive(req)(u => users.revokeGrant(u.id, clientId).ifM(NoContent(), err(ErrorCode.ConnectedAppNotFound)))

  private val CodeTtlSeconds = 60L
  private val AccessTtlSeconds = 3600L
  private val RefreshDays = 90
  private val CsrfCookie = "skotare_oauth_csrf"

  private def client(id: String): Option[OAuthClient] = cfg.oauthClients.find(_.id == id)

  private def enc(s: String): String = java.net.URLEncoder.encode(s, "UTF-8")

  /** redirect_uri に結果を付けて戻す. */
  private def back(uri: String, params: (String, Option[String])*): IO[Response[IO]] = {
    val q = params.collect { case (k, Some(v)) => s"$k=${enc(v)}" }.mkString("&")
    IO.pure(Response[IO](Status.Found).putHeaders(
      Location(Uri.unsafeFromString(uri + (if uri.contains("?") then "&" else "?") + q)),
      "Cache-Control" -> "no-store"
    ))
  }

  // ---------------------------------------------------------------- スコープ

  /** 要求されたスコープ（空白区切り. なければ read）. 知らないもの・クライアントに許していないものが含まれていれば None. */
  private def requested(allowed: Set[String], scope: Option[String]): Option[Set[String]] = {
    val rs =
      scope.map(_.split(' ').map(_.trim).filter(_.nonEmpty).toSet).filter(_.nonEmpty).getOrElse(Set(TokenScope.Read))
    Option.when(rs.forall(allowed.contains))(rs)
  }

  /** api_token のスコープ（write は読み書き. read を含む）. */
  private def tokenScope(rs: Set[String]): String =
    if rs.contains(TokenScope.Write) then TokenScope.Write else TokenScope.Read

  /** トークンの応答に返すスコープ. */
  private def granted(scope: String): String = if scope == TokenScope.Write then "read write" else "read"

  private def clientScopes(c: OAuthClient): Set[String] = c.scopes.toSet.intersect(TokenScope.all.toSet)

  // ---------------------------------------------------------------- 許可の画面（サーバーで HTML を組み立てる）

  /**
   * 許可の画面. CSP の form-action は付けない（フォームの送信後のリダイレクトの連鎖にも効くため、
   * 連携元が callback の後に別の origin へ戻すと止まる）. CSRF は Cookie と hidden の値の照合で防ぐ.
   */
  private def page(title: String, body: String, status: Status = Status.Ok): IO[Response[IO]] = {
    val html =
      s"""<!DOCTYPE html><html lang="ja"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1">
         |<title>${Html.escape(title)} | skotare</title><style>
         |:root{color-scheme:light dark}body{margin:0;font-family:system-ui,-apple-system,"Hiragino Sans","Noto Sans JP",sans-serif;background:#f1f5f9;color:#1e293b;
         |display:flex;min-height:100vh;align-items:center;justify-content:center;padding:1rem}
         |@media(prefers-color-scheme:dark){body{background:#0f172a;color:#e2e8f0}.card{background:#1c273d!important;border-color:#283548!important}}
         |.card{background:#fff;border:1px solid #e2e8f0;border-radius:14px;padding:1.6rem 1.8rem;max-width:440px;width:100%;
         |box-shadow:0 11px 15px -7px rgba(0,0,0,.2),0 24px 38px 3px rgba(0,0,0,.14)}
         |h1{font-size:1.15rem;margin:0 0 .8rem}p{line-height:1.7;margin:.4rem 0}.muted{color:#64748b;font-size:.85rem}.logo{color:#0d9488;font-weight:800}
         |.scopes{list-style:none;padding:0;margin:.6rem 0}.scopes li{border:1px solid #cbd5e1;border-radius:8px;padding:.5rem .7rem;margin:.4rem 0}
         |.scopes code{font-weight:700}
         |.actions{display:flex;gap:.6rem;justify-content:flex-end;margin-top:1.2rem}button{font:inherit;border-radius:8px;padding:.5rem 1.1rem;cursor:pointer;
         |border:1px solid #cbd5e1;background:transparent;color:inherit}button.primary{background:#0d9488;border-color:#0d9488;color:#fff}
         |ul{padding-left:1.2rem;margin:.4rem 0}</style></head><body><div class="card"><div class="logo">skotare</div>$body</div></body></html>""".stripMargin
    IO.pure(Response[IO](status).withEntity(html)(using EntityEncoder.stringEncoder).putHeaders(
      `Content-Type`(MediaType.text.html, Charset.`UTF-8`),
      "Cache-Control" -> "no-store",
      "X-Frame-Options" -> "DENY",
      "Content-Security-Policy" -> "default-src 'none'; style-src 'unsafe-inline'; frame-ancestors 'none'"
    ))
  }

  private def errorPage(msg: String): IO[Response[IO]] =
    page(
      "認可できません",
      s"<h1>認可できません</h1><p>${Html.escape(msg)}</p><p class=\"muted\">連携元のサービスの設定を確認してください。</p>",
      Status.BadRequest
    )

  /** 認可の要求の確認（client_id・redirect_uri が不正なら、リダイレクトせずに 400）. 正しければ (クライアント, スコープ, state, code_challenge). */
  private def validate(p: String => Option[String])
    : Either[IO[Response[IO]], (OAuthClient, Set[String], Option[String], String)] =
    (p("client_id").flatMap(client), p("redirect_uri")) match {
      case (None, _) => Left(errorPage("登録されていないクライアントです（client_id）"))
      case (Some(c), r) if !r.contains(c.redirectUri) => Left(errorPage("戻り先（redirect_uri）が登録されたものと一致しません"))
      case (Some(c), _) =>
        val state = p("state")
        if !p("response_type").contains("code") then
          Left(back(c.redirectUri, "error" -> Some("unsupported_response_type"), "state" -> state))
        else if !p("code_challenge_method").contains("S256") ||
          !p("code_challenge").exists(_.matches("^[A-Za-z0-9_-]{43}$"))
        then
          Left(back(
            c.redirectUri,
            "error" -> Some("invalid_request"),
            "error_description" -> Some("PKCE（S256）が必要です"),
            "state" -> state
          ))
        else
          requested(clientScopes(c), p("scope")) match {
            case None => Left(back(c.redirectUri, "error" -> Some("invalid_scope"), "state" -> state))
            case Some(rs) => Right((c, rs, state, p("code_challenge").get))
          }
    }

  private def consent(c: OAuthClient, rs: Set[String], user: User, hidden: String, csrf: String): IO[Response[IO]] = {
    val name = Html.escape(c.name)
    val write = rs.contains(TokenScope.Write)
    val items =
      (List(TokenScope.Read -> "監視・障害・通知先・ステータスページを読む") ++
        Option.when(write)(TokenScope.Write -> "監視・通知先・ステータスページを作る・変える・削除する、すぐに確かめる").toList)
        .map((k, v) => s"<li><code>$k</code> — ${Html.escape(v)}</li>").mkString
    page(
      s"$name との連携",
      s"""<h1>$name との連携</h1>
         |<p><b>$name</b> が、あなた（${Html.escape(user.name)}）の権限で skotare を使います。許可するスコープ:</p>
         |<ul class="scopes">$items</ul>
         |<ul class="muted"><li>${if write then "変更は、あなたの名前で行います" else "読み取りだけです（作成・変更・削除はしません）"}</li>
         |<li>設定の「連携しているサービス」から、いつでも連携をやめられます</li></ul>
         |<form method="post" action="/oauth/authorize">$hidden<input type="hidden" name="csrf" value="$csrf">
         |<div class="actions"><button type="submit" name="decision" value="deny">拒否する</button>
         |<button type="submit" name="decision" value="allow" class="primary">許可する</button></div></form>""".stripMargin
    )
  }

  // ---------------------------------------------------------------- トークンの発行・失効（クライアントの認証は Basic）

  private def oauthError(status: Status, code: String, desc: String): IO[Response[IO]] =
    IO.pure(Response[IO](status).withEntity(Json.obj("error" -> code.asJson, "error_description" -> desc.asJson))
      .putHeaders("Cache-Control" -> "no-store", "Pragma" -> "no-cache").putHeaders(
        (if status == Status.Unauthorized then List[Header.ToRaw]("WWW-Authenticate" -> "Basic realm=\"skotare\"")
         else Nil)*
      ))

  private def basicClient(req: Request[IO]): Option[OAuthClient] =
    req.headers.get[Authorization].collect { case Authorization(BasicCredentials(id, secret)) => (id, secret) }
      .flatMap((id, secret) => client(id).filter(c => Crypto.constantTimeEquals(c.secret, secret)))

  private def form(req: Request[IO])(f: Map[String, String] => IO[Response[IO]]): IO[Response[IO]] =
    req.as[UrlForm].attempt.flatMap {
      case Left(_) => oauthError(Status.BadRequest, "invalid_request", "application/x-www-form-urlencoded で送ってください")
      case Right(u) => f(u.values.view.mapValues(_.headOption.map(_.trim).getOrElse("")).filter(_._2.nonEmpty).toMap)
    }

  /** 新しいトークンの組の値（アクセストークン, refresh token）と、それを登録するもの（値はハッシュだけ）を作る. */
  private def issue(c: OAuthClient): (String, String, (java.util.UUID, String) => OAuthIssue) = {
    val (access, prefix) = ApiTokens.generate()
    val refresh = Crypto.randomToken(32)
    (
      access,
      refresh,
      (user, scope) =>
        OAuthIssue(
          user,
          c.id,
          s"${c.name} 連携",
          prefix,
          Crypto.sha256(access),
          Crypto.sha256(refresh),
          scope,
          Instant.now().plusSeconds(AccessTtlSeconds)
        )
    )
  }

  private def tokenResponse(access: String, refresh: String, scope: String): IO[Response[IO]] =
    Ok(Json.obj(
      "access_token" -> access.asJson,
      "token_type" -> "Bearer".asJson,
      "expires_in" -> AccessTtlSeconds.asJson,
      "refresh_token" -> refresh.asJson,
      "scope" -> granted(scope).asJson
    )).map(_.putHeaders("Cache-Control" -> "no-store", "Pragma" -> "no-cache"))

  private def byCode(c: OAuthClient, f: Map[String, String]): IO[Response[IO]] =
    (f.get("code"), f.get("redirect_uri"), f.get("code_verifier")) match {
      case (Some(code), Some(uri), Some(verifier)) if verifier.length >= 43 && verifier.length <= 128 =>
        users.consumeCode(Crypto.sha256(code)).flatMap {
          case Some(oc)
              if oc.clientId == c.id && oc.redirectUri == uri &&
                Crypto.constantTimeEquals(oc.codeChallenge, Crypto.pkceChallenge(verifier)) =>
            val (access, refresh, g) = issue(c)
            users.grant(g(oc.userId, oc.scope)) *> tokenResponse(access, refresh, oc.scope)
          case _ => oauthError(Status.BadRequest, "invalid_grant", "認可コードが無効です（期限切れ・使用済み・不一致）")
        }
      case _ => oauthError(Status.BadRequest, "invalid_request", "code・redirect_uri・code_verifier（43〜128文字）を指定してください")
    }

  /** refresh token で新しい組に替える. scope を付ければ狭められる（広げられない）. */
  private def byRefresh(c: OAuthClient, f: Map[String, String]): IO[Response[IO]] =
    f.get("refresh_token") match {
      case None => oauthError(Status.BadRequest, "invalid_request", "refresh_token を指定してください")
      case Some(rt) =>
        val (access, refresh, g) = issue(c)
        users.refresh(
          Crypto.sha256(rt),
          c.id,
          RefreshDays,
          {
            (uid, current) =>
              // クライアントに許すスコープを後から狭めていれば、それに合わせる
              val have =
                (if current == TokenScope.Write then Set(TokenScope.Read, TokenScope.Write) else Set(TokenScope.Read))
                  .intersect(clientScopes(c) + TokenScope.Read)
              f.get("scope").fold(Some(have))(s => requested(have, Some(s))).toRight(()).map(w => g(uid, tokenScope(w)))
          }
        ).flatMap {
          case Some(Right(next)) => tokenResponse(access, refresh, next.scope)
          case Some(Left(_)) => oauthError(Status.BadRequest, "invalid_scope", "許可されたスコープより広いスコープは求められません")
          case None => oauthError(Status.BadRequest, "invalid_grant", "refresh token が無効です（期限切れ・使用済み・取り消し済み）")
        }
    }

}
