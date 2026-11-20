package skotare.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import sbirds.auth.Crypto
import sbirds.oauth.{OAuthCode, OAuthProvider}
import skotare.auth.ApiTokens
import skotare.db.*
import skotare.shared.*
import skotare.core.ErrorCode

import java.time.Instant
import java.util.UUID

/**
 * 外部のサービス（skakapo など）への認可（OAuth 2.0 の認可コード + PKCE. 流れは sbirds.oauth.OAuthProvider）と、連携の確認・取り消し.
 * 発行するアクセストークンは、そのユーザーの API トークン（REST API と MCP をその利用者の権限で使える. 1時間で切れる）と、
 * 新しい組に替える refresh token（使うたびに替わる. 最後の更新から90日使わなければ切れる）.
 * スコープは read（参照のみ）と write（読み書き. read を含む）. クライアントに許した範囲を超える要求は invalid_scope.
 */
final class OAuthRoutes(users: UserStore, val sessions: SessionAuth, cfg: ApiConfig) extends RouteSupport {

  private val AccessTtlSeconds = 3600L
  private val RefreshDays = 90

  private val host = new OAuthProvider.Host[Principal] {
    val settings = OAuthProvider.Settings(
      "skotare",
      cfg.cookieSecure,
      accent = "#0d9488",
      extraCss =
        ".scopes{list-style:none;padding:0;margin:.6rem 0}.scopes li{border:1px solid #cbd5e1;border-radius:8px;padding:.5rem .7rem;margin:.4rem 0}.scopes code{font-weight:700}",
      loginRedirectNoStore = true,
      backNoStore = true,
      formValue = v => Some(v.trim).filter(_.nonEmpty),
      unreadableForm = "要求の形式が正しくありません。連携元の画面からやり直してください"
    )
    val codes = users.oauthCodes

    def client(id: String): IO[Option[OAuthClient]] = IO.pure(OAuthRoutes.this.client(id))

    // 許可は画面のログイン（Cookie のセッション）だけ（API トークンでは許可できない）
    def browserSession(req: Request[IO]): IO[Option[Principal]] =
      sessions.fromRequest(req).map(_.filter(_.token.isEmpty))

    def userId(s: Principal): UUID = s.user.id

    override def scopes(c: OAuthClient, requested: Option[String]): Option[List[String]] =
      OAuthProvider.requestedScopes(clientScopes(c), requested, List(TokenScope.Read))

    override def codeScope(c: OAuthClient, scopes: List[String]): String = tokenScope(scopes)

    def consent(req: Request[IO], c: OAuthClient, s: Principal, rs: List[String]): IO[OAuthProvider.Consent] = {
      val name = Html.escape(c.name)
      val write = rs.contains(TokenScope.Write)
      val items =
        (List(TokenScope.Read -> "監視・障害・通知先・ステータスページを読む") ++
          Option.when(write)(TokenScope.Write -> "監視・通知先・ステータスページを作る・変える・削除する、すぐに確かめる").toList)
          .map((k, v) => s"<li><code>$k</code> — ${Html.escape(v)}</li>").mkString
      IO.pure(OAuthProvider.Consent(
        s"$name との連携",
        s"""<h1>$name との連携</h1>
           |<p><b>$name</b> が、あなた（${Html.escape(s.user.name)}）の権限で skotare を使います。許可するスコープ:</p>
           |<ul class="scopes">$items</ul>
           |<ul class="muted"><li>${if write then "変更は、あなたの名前で行います" else "読み取りだけです（作成・変更・削除はしません）"}</li>
           |<li>設定の「連携しているサービス」から、いつでも連携をやめられます</li></ul>""".stripMargin
      ))
    }

    override def authorized(req: Request[IO], s: Principal, c: OAuthClient, rs: List[String]): IO[Unit] =
      sbirds.observe.Log.println(s"[skotare] oauth: ${s.user.id} authorized ${c.id} (${granted(tokenScope(rs))})")

    def issue(req: Request[IO], c: OAuthClient, oc: OAuthCode): IO[OAuthProvider.Token] = {
      val (access, refresh, g) = newTokens(c)
      users.grant(g(oc.userId, oc.scope)).as(token(access, refresh, oc.scope))
    }

    /** refresh token で新しい組に替える. scope を付ければ狭められる（広げられない）. */
    override def refresh: Option[OAuthProvider.Refresh] = Some { (_, c, rt, f) =>
      val (access, refresh, g) = newTokens(c)
      users.refresh(
        Crypto.sha256(rt),
        c.id,
        RefreshDays,
        {
          (uid, current) =>
            // クライアントに許すスコープを後から狭めていれば、それに合わせる
            val have = (if current == TokenScope.Write then TokenScope.all else List(TokenScope.Read))
              .filter((TokenScope.Read :: clientScopes(c)).contains)
            f.get("scope").fold(Option(have))(s => OAuthProvider.requestedScopes(have, Some(s), List(TokenScope.Read)))
              .toRight(()).map(w => g(uid, tokenScope(w)))
        }
      ).map {
        case Some(Right(next)) => Right(token(access, refresh, next.scope))
        case Some(Left(_)) => Left(OAuthProvider.Failure.invalidScope("許可されたスコープより広いスコープは求められません"))
        case None => Left(OAuthProvider.Failure.invalidGrant("refresh token が無効です（期限切れ・使用済み・取り消し済み）"))
      }
    }

    def revoke(req: Request[IO], c: OAuthClient, token: String): IO[Unit] =
      users.revokeOAuthToken(Crypto.sha256(token), c.id)
  }

  val routes: HttpRoutes[IO] = new OAuthProvider(host).routes <+> HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "oauth" / "grants" => getOauthGrants(req)
    case req @ DELETE -> Root / "api" / "oauth" / "grants" / clientId => deleteOauthGrant(req, clientId)
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

  private def client(id: String): Option[OAuthClient] = cfg.oauthClients.find(_.id == id)

  // ---------------------------------------------------------------- スコープ

  /** api_token のスコープ（write は読み書き. read を含む）. */
  private def tokenScope(rs: List[String]): String =
    if rs.contains(TokenScope.Write) then TokenScope.Write else TokenScope.Read

  /** トークンの応答に返すスコープ. */
  private def granted(scope: String): String = if scope == TokenScope.Write then "read write" else "read"

  private def clientScopes(c: OAuthClient): List[String] = c.scopes.filter(TokenScope.all.contains)

  // ---------------------------------------------------------------- トークンの発行

  /** 新しいトークンの組の値（アクセストークン, refresh token）と、それを登録するもの（値はハッシュだけ）を作る. */
  private def newTokens(c: OAuthClient): (String, String, (UUID, String) => OAuthIssue) = {
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

  private def token(access: String, refresh: String, scope: String): OAuthProvider.Token =
    OAuthProvider.Token(access, granted(scope), Some(AccessTtlSeconds), Some(refresh))
}
