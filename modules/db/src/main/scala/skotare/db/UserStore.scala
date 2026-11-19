package skotare.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skotare.shared.*

import java.time.Instant
import java.util.UUID

/** avatarUrl は表示するアイコン（保存したものがあればその URL、なければ IdP の画像）. */
final case class User(
  id: UUID,
  sub: String,
  name: String,
  email: Option[String],
  avatarUrl: Option[String],
  timezone: String,
  disabled: Boolean,
  avatarUploaded: Boolean) {
  def me: MeDto = MeDto(id, name, email, avatarUrl, timezone, avatarUploaded)
  def brief: UserBrief = me.brief
}

/** app_user の列（別名 a）. */
object UserCols {

  /** 表示するアイコンの URL. 保存したアイコンがあれば /api/users/{id}/avatar?v={版}（版を変えるとブラウザのキャッシュが切り替わる）. */
  def avatar(
    a: String
  ): Fragment = Fragment.const(
    s"(case when $a.avatar_version is not null then '/api/users/' || $a.id || '/avatar?v=' || $a.avatar_version else $a.avatar_url end)"
  )
}

final case class OAuthCode(
  clientId: String,
  redirectUri: String,
  userId: UUID,
  codeChallenge: String,
  scope: String)

/** OAuth で発行するトークンの組（値はハッシュだけ）. scope は read か write. */
final case class OAuthIssue(
  userId: UUID,
  clientId: String,
  name: String,
  prefix: String,
  accessHash: Array[Byte],
  refreshHash: Array[Byte],
  scope: String,
  expires: Instant)

/** ユーザー・セッション・ログイン途中の状態・API トークン・OAuth の認可. */
final class UserStore(
  xa: Transactor[IO]) extends TxStore(xa) {

  private val cols = fr"app_user.id, sub, name, email," ++ UserCols.avatar("app_user") ++
    fr", timezone, disabled, avatar_version is not null"

  /** ログインのたびに作成・更新する（名前・メール・アイコンは IdP の値に合わせる）. */
  def upsert(
    sub: String,
    name: String,
    email: Option[String],
    avatar: Option[String]
  ): IO[User] = tx(
    (fr"""insert into app_user (id, sub, name, email, avatar_url, last_login_at) values (${UUID.randomUUID()}, $sub, $name, $email, $avatar, now())
          on conflict (sub) do update set name = excluded.name, email = excluded.email,
            avatar_url = excluded.avatar_url, last_login_at = now()
          returning""" ++ cols).query[User].unique
  )

  def find(
    id: UUID
  ): IO[Option[User]] = tx(findC(id))

  def findC(
    id: UUID
  ): ConnectionIO[Option[User]] = (fr"select" ++ cols ++ fr"from app_user where id = $id").query[User].option

  def briefs(
    ids: List[UUID]
  ): ConnectionIO[Map[UUID, UserBrief]] =
    if ids.isEmpty then Map.empty[UUID, UserBrief].pure[ConnectionIO]
    else
      (fr"select" ++ cols ++ fr"from app_user where id = any(${ids.distinct.toArray})").query[User].to[List].map(_.map(
        u => u.id -> u.brief
      ).toMap)

  /** メールアドレス・ID で1人を決める（共有の相手の指定）. */
  def resolve(
    s: String
  ): IO[Option[User]] = tx {
    val t = s.trim
    scala.util.Try(UUID.fromString(t)).toOption match {
      case Some(id) => findC(id)
      case None => (fr"select" ++ cols ++
          fr"from app_user where not disabled and lower(email) = lower($t) order by last_login_at desc nulls last limit 1")
          .query[User].option
    }
  }

  def updateMe(
    id: UUID,
    name: Option[String],
    timezone: Option[String]
  ): IO[Unit] = tx {
    val sets =
      List(name.map(_.trim).filter(_.nonEmpty).map(v => fr"name = $v"), timezone.map(v => fr"timezone = $v")).flatten
    if sets.isEmpty then ().pure[ConnectionIO]
    else (fr"update app_user" ++ Sql.set(sets) ++ fr"where id = $id").update.run.void
  }

  // ---------------------------------------------------------------- アイコン

  /** アイコンを保存する（版を1つ進める）. */
  def saveAvatar(
    id: UUID,
    contentType: String,
    data: Array[Byte]
  ): IO[Unit] = tx(
    sql"""insert into user_avatar (user_id, content_type, data) values ($id, $contentType, $data)
          on conflict (user_id) do update set content_type = excluded.content_type, data = excluded.data, updated_at = now()""".update.run *>
      sql"update app_user set avatar_version = coalesce(avatar_version, 0) + 1 where id = $id".update.run.void
  )

  /** 保存したアイコンを消す（IdP の画像に戻る）. */
  def deleteAvatar(
    id: UUID
  ): IO[Unit] = tx(
    sql"delete from user_avatar where user_id = $id".update.run *>
      sql"update app_user set avatar_version = null where id = $id".update.run.void
  )

  def avatar(
    id: UUID
  ): IO[Option[(String, Array[Byte])]] = tx(
    sql"select content_type, data from user_avatar where user_id = $id".query[(String, Array[Byte])].option
  )

  // ---------------------------------------------------------------- セッション

  def createSession(
    hash: Array[Byte],
    userId: UUID,
    idToken: Option[String],
    expires: Instant
  ): IO[Unit] = tx(
    sql"insert into session (token_hash, user_id, id_token, expires_at) values ($hash, $userId, $idToken, $expires)".update.run.void
  )

  def findSession(
    hash: Array[Byte],
    now: Instant
  ): IO[Option[User]] = tx(
    sql"select user_id from session where token_hash = $hash and expires_at > $now".query[UUID].option.flatMap {
      case None => none[User].pure[ConnectionIO]
      case Some(uid) => findC(uid).map(_.filterNot(_.disabled))
    }
  )

  /** セッションの ID トークン（OIDC でログインしたとき）. */
  def sessionIdToken(
    hash: Array[Byte]
  ): IO[Option[String]] =
    tx(sql"select id_token from session where token_hash = $hash".query[Option[String]].option.map(_.flatten))

  /** セッションを消し、IdP のログアウトに渡す ID トークンを返す. */
  def deleteSession(
    hash: Array[Byte]
  ): IO[Option[String]] = tx(
    sql"delete from session where token_hash = $hash returning id_token".query[Option[String]].option.map(_.flatten)
  )

  def purgeExpired(
    now: Instant
  ): IO[Unit] = tx(
    sql"delete from session where expires_at < $now".update.run *>
      sql"delete from login_state where created_at < now() - interval '1 hour'".update.run.void
  )

  /** プッシュ通知を受け取る端末のある人（無効にした人は除く. 購読は sbirds-lib の PushStore の表）. */
  def pushSubscribers: IO[List[UUID]] =
    tx(sql"select distinct s.user_id from push_subscription s join app_user u on u.id = s.user_id where not u.disabled".query[
      UUID
    ].to[List])

  // ---------------------------------------------------------------- API トークン

  private val tokenCols = fr"id, name, prefix, scope, created_at, expires_at, last_used_at"

  private def tokenDto(
    t: (UUID, String, String, String, Instant, Option[Instant], Option[Instant])
  ): ApiTokenDto =
    ApiTokenDto(t._1, t._2, t._3, t._4, t._5.toString, t._6.map(_.toString), t._7.map(_.toString))

  def createToken(
    userId: UUID,
    name: String,
    prefix: String,
    hash: Array[Byte],
    scope: String,
    expires: Option[Instant]
  ): IO[ApiTokenDto] = tx(
    (fr"insert into api_token (id, user_id, name, prefix, token_hash, scope, expires_at) values (${UUID.randomUUID()}, $userId, $name, $prefix, $hash, $scope, $expires) returning" ++
      tokenCols)
      .query[(UUID, String, String, String, Instant, Option[Instant], Option[Instant])].unique.map(tokenDto)
  )

  /** 失効していないもの（期限切れも含む. 画面で期限切れと分かるように）. OAuth で発行したものは「連携しているサービス」に出す. */
  def tokens(
    userId: UUID
  ): IO[List[ApiTokenDto]] = tx(
    (fr"select" ++ tokenCols ++
      fr"from api_token where user_id = $userId and revoked_at is null and oauth_client is null order by created_at desc")
      .query[(UUID, String, String, String, Instant, Option[Instant], Option[Instant])].to[List].map(_.map(tokenDto))
  )

  def revokeToken(
    userId: UUID,
    id: UUID
  ): IO[Boolean] = tx(
    sql"update api_token set revoked_at = now() where id = $id and user_id = $userId and revoked_at is null and oauth_client is null".update.run.map(
      _ > 0
    )
  )

  /** 有効なトークンの持ち主とスコープ. 最後に使った日時は1分に1回だけ書く. */
  def findToken(
    hash: Array[Byte],
    now: Instant
  ): IO[Option[(User, UUID, String)]] = tx(
    sql"""select id, user_id, scope from api_token
          where token_hash = $hash and revoked_at is null and (expires_at is null or expires_at > $now)"""
      .query[(UUID, UUID, String)].option.flatMap {
        case None => none[(User, UUID, String)].pure[ConnectionIO]
        case Some((id, uid, scope)) =>
          sql"update api_token set last_used_at = $now where id = $id and (last_used_at is null or last_used_at < $now - interval '1 minute')".update.run *>
            findC(uid).map(_.filterNot(_.disabled).map(u => (u, id, scope)))
      }
  )

  // ---------------------------------------------------------------- OAuth の提供側（認可コード・連携のトークン）

  def createCode(
    hash: Array[Byte],
    c: OAuthCode,
    expires: Instant
  ): IO[Unit] = tx(
    sql"""insert into oauth_code (code_hash, client_id, redirect_uri, user_id, code_challenge, scope, expires_at)
          values ($hash, ${c.clientId}, ${c.redirectUri}, ${c.userId}, ${c.codeChallenge}, ${c.scope}, $expires)""".update.run *>
      sql"delete from oauth_code where expires_at < now() - interval '1 hour'".update.run.void
  )

  /** 認可コードを使う（有効期限内で、まだ使っていないものだけ. 使ったら使用済みにする）. */
  def consumeCode(
    hash: Array[Byte]
  ): IO[Option[OAuthCode]] = tx(
    sql"""update oauth_code set used_at = now() where code_hash = $hash and used_at is null and expires_at > now()
          returning client_id, redirect_uri, user_id, code_challenge, scope""".query[OAuthCode].option
  )

  private def insertGrant(
    g: OAuthIssue,
    granted: Instant,
    lastUsed: Option[Instant]
  ): ConnectionIO[Unit] =
    sql"""insert into api_token (id, user_id, name, prefix, token_hash, scope, expires_at, last_used_at, oauth_client, refresh_hash, granted_at)
          values (${UUID.randomUUID()}, ${g.userId}, ${g.name}, ${g.prefix}, ${g.accessHash}, ${g.scope}, ${g.expires}, $lastUsed,
                  ${g.clientId}, ${g.refreshHash}, $granted)""".update.run.void

  /** 許可したときのトークンの組を登録する（そのユーザーにそのクライアントが以前に発行したものは失効させる）. */
  def grant(
    g: OAuthIssue
  ): IO[Unit] = tx(
    sql"""update api_token set revoked_at = now() where user_id = ${g.userId} and oauth_client = ${g.clientId} and revoked_at is null""".update.run *>
      insertGrant(g, Instant.now(), None)
  )

  /**
   * refresh token で新しい組に替える. 前の組は失効させる（同じ refresh token は2度使えない）.
   * 有効なのは、そのクライアントが発行した、失効していない、最後の更新から refreshDays 日以内のもので、利用者が無効にされていないとき.
   * next は (ユーザー, 今のスコープ) から新しい組を作る（Left なら替えない. 求められたスコープが広すぎるときなど）.
   * 有効な refresh token でなければ None.
   */
  def refresh[E](
    hash: Array[Byte],
    clientId: String,
    refreshDays: Int,
    next: (UUID, String) => Either[E, OAuthIssue]
  ): IO[Option[Either[E, OAuthIssue]]] = tx(
    sql"""select t.id, t.user_id, t.scope, t.granted_at, t.last_used_at from api_token t join app_user u on u.id = t.user_id
          where t.refresh_hash = $hash and t.oauth_client = $clientId and t.revoked_at is null and not u.disabled
            and t.created_at > now() - make_interval(days => $refreshDays)
          for update of t"""
      .query[(UUID, UUID, String, Option[Instant], Option[Instant])].option.flatMap {
        case None => none[Either[E, OAuthIssue]].pure[ConnectionIO]
        case Some((id, uid, scope, granted, lastUsed)) =>
          next(uid, scope).traverse(g =>
            sql"update api_token set revoked_at = now() where id = $id".update.run *>
              insertGrant(g, granted.getOrElse(Instant.now()), lastUsed).as(g)
          )
            .map(Some(_))
      }
  )

  /** クライアントからの失効（RFC 7009）. アクセストークンか refresh token の、その組を失効させる（なければ何もしない）. */
  def revokeOAuthToken(
    hash: Array[Byte],
    clientId: String
  ): IO[Unit] = tx(
    sql"""update api_token set revoked_at = now()
          where (token_hash = $hash or refresh_hash = $hash) and oauth_client = $clientId and revoked_at is null""".update.run.void
  )

  /** 連携しているサービス（クライアントの ID, スコープ, 許可した日時, 最後に使った日時）. */
  def grants(
    userId: UUID
  ): IO[List[(String, String, Instant, Option[Instant])]] = tx(
    sql"""select oauth_client, scope, coalesce(granted_at, created_at), last_used_at from api_token
          where user_id = $userId and oauth_client is not null and revoked_at is null order by granted_at"""
      .query[(String, String, Instant, Option[Instant])].to[List]
  )

  /** 連携をやめる（そのクライアントのアクセストークン・refresh token をすべて失効させる）. */
  def revokeGrant(
    userId: UUID,
    clientId: String
  ): IO[Boolean] = tx(
    sql"update api_token set revoked_at = now() where user_id = $userId and oauth_client = $clientId and revoked_at is null".update.run.map(
      _ > 0
    )
  )

  // ---------------------------------------------------------------- ログイン途中の状態（OIDC）

  def saveLoginState(
    state: String,
    verifier: String,
    nonce: String,
    returnTo: String
  ): IO[Unit] = tx(
    sql"insert into login_state (state, verifier, nonce, return_to) values ($state, $verifier, $nonce, $returnTo)".update.run.void
  )

  /** 取り出して消す（1回だけ・15分以内）. */
  def takeLoginState(
    state: String
  ): IO[Option[(String, String, String)]] = tx(
    sql"""delete from login_state where state = $state and created_at > now() - interval '15 minutes'
          returning verifier, nonce, return_to""".query[(String, String, String)].option
  )
}
