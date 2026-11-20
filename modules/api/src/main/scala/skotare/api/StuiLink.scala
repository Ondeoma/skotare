package skotare.api

import sbirds.shared.SafePath
import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.{Authorization, Location}
import sbirds.auth.Crypto
import sbirds.links.IntegrationTokenStore
import skotare.auth.SecretBox
import skotare.db.*
import skotare.shared.*
import skotare.core.ErrorCode

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

/**
 * stui（チャット）との連携のクライアント側.
 * 利用者が stui と OAuth（認可コード + PKCE）で連携すると、通知先「stui」に、自分が参加しているチャンネルを選べる.
 * 通知は、その通知先を作った人のトークンで stui の POST /api/apps/notify に送る（stui のボット「skotare」からの投稿になる）.
 * stui 側の OAUTH_CLIENTS に skotare を登録する（redirectUri は {PUBLIC_URL}/api/integrations/stui/callback、scopes は read・write）.
 * トークンは APP_SECRET の鍵で暗号化して保存する.
 */
final class StuiLink(cfg: Option[LinkService],
                     tokens: IntegrationTokenStore,
                     http: Client[IO],
                     box: Option[SecretBox],
                     publicUrl: String,
                     val sessions: SessionAuth) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    /** 設定されている連携と、自分が連携しているか. */
    case req @ GET -> Root / "api" / "integrations" => getIntegrations(req)
    case req @ GET -> Root / "api" / "integrations" / "stui" / "channels" => getIntegrationsStuiChannels(req)
    case req @ GET -> Root / "api" / "integrations" / "stui" / "connect" => getIntegrationsStuiConnect(req)
    case req @ GET -> Root / "api" / "integrations" / "stui" / "callback" => getIntegrationsStuiCallback(req)
    case req @ DELETE -> Root / "api" / "integrations" / "stui" / "connection" => deleteIntegrationsStuiConnection(req)
  }

  private def getIntegrations(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u =>
      cfg match {
        case None => Ok(List.empty[IntegrationDto])
        case Some(c) =>
          // 名前は sruru に登録した表示名（なければサービスの名前）
          (connected(u.id), sessions.appNames(req)).flatMapN((on, names) =>
            Ok(List(IntegrationDto(Provider, names.getOrElse(Provider, Provider), c.url, on)))
          )
      }
    )

  private def getIntegrationsStuiChannels(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u =>
      channels(u.id).flatMap {
        case Right(cs) => Ok(cs.sortBy(_.name.toLowerCase))
        case Left(e) => err(ErrorCode.StuiRequestFailed, e)
      }
    )

  private def getIntegrationsStuiConnect(req: Request[IO]): IO[Response[IO]] =
    interactive(req)(u =>
      (cfg, box) match {
        case (None, _) => err(ErrorCode.StuiNotConfigured)
        case (_, None) => err(ErrorCode.StuiNeedsAppSecret)
        case (Some(c), Some(_)) =>
          val state = Crypto.randomToken(); val verifier = Crypto.randomToken(48)
          tokens.saveState(
            state,
            u.id,
            Provider,
            verifier,
            SafePath.of(param(req, "returnTo"), "/notifiers"),
            Instant.now().plusSeconds(600)
          ) *>
            found(Uri.unsafeFromString(c.url + "/oauth/authorize").withQueryParams(Map(
              "response_type" -> "code",
              "client_id" -> c.clientId,
              "redirect_uri" -> redirectUri,
              "scope" -> "read write",
              "state" -> state,
              "code_challenge" -> Crypto.pkceChallenge(verifier),
              "code_challenge_method" -> "S256"
            )).renderString)
      }
    )

  private def getIntegrationsStuiCallback(req: Request[IO]): IO[Response[IO]] =
    interactive(req)(u =>
      (cfg, box, param(req, "state")) match {
        case (Some(c), Some(b), Some(state)) =>
          tokens.takeState(state, Provider).flatMap {
            case None => back("/notifiers", "expired")
            case Some((uid, _, to)) if uid != u.id => back(to, "error")
            case Some((_, verifier, to)) =>
              param(req, "code") match {
                case None => back(to, if param(req, "error").contains("access_denied") then "denied" else "error")
                case Some(code) =>
                  val form = UrlForm(
                    "grant_type" -> "authorization_code",
                    "code" -> code,
                    "redirect_uri" -> redirectUri,
                    "code_verifier" -> verifier
                  )
                  val tr = Request[IO](
                    Method.POST,
                    Uri.unsafeFromString(c.apiUrl + "/oauth/token")
                  ).withEntity(form).putHeaders(basic(c))
                  http.run(tr).use(r =>
                    if r.status.isSuccess then r.as[Json].map(_.hcursor.get[String]("access_token").toOption)
                    else IO.pure(None)
                  )
                    .timeout(15.seconds).handleError(_ => None).flatMap {
                      case Some(t) =>
                        tokens.saveToken(u.id, Provider, b.encrypt(t)) *> back(to, "connected")
                      case None => back(to, "error")
                    }
              }
          }
        case _ => err(ErrorCode.StuiStateMissing)
      }
    )

  private def deleteIntegrationsStuiConnection(req: Request[IO]): IO[Response[IO]] =
    interactive(req)(u =>
      cfg.traverse_(c =>
        token(u.id).flatMap(_.traverse_ { t =>
          http.run(Request[IO](
            Method.POST,
            Uri.unsafeFromString(c.apiUrl + "/oauth/revoke")
          ).withEntity(UrlForm("token" -> t)).putHeaders(basic(c)))
            .use(_ => IO.unit).timeout(10.seconds).handleError(_ => ())
        })
      ) *> tokens.deleteToken(u.id, Provider) *> NoContent()
    )

  val Provider = "stui"

  private def redirectUri: String = s"${publicUrl.stripSuffix("/")}/api/integrations/stui/callback"

  private def basic(c: LinkService): Authorization = Authorization(BasicCredentials(c.clientId, c.clientSecret))

  private def bearer(t: String): Authorization = Authorization(Credentials.Token(AuthScheme.Bearer, t))

  private def found(to: String): IO[Response[IO]] =
    IO.pure(Response[IO](Status.Found).putHeaders(
      Location(Uri.fromString(to).getOrElse(Uri(path = Uri.Path.Root))),
      "Cache-Control" -> "no-store"
    ))

  /** 戻り先に結果を付ける（integration=stui&result=…）. */
  private def back(to: String, result: String): IO[Response[IO]] =
    found(to + (if to.contains("?") then "&" else "?") + s"integration=stui&result=$result")

  def token(userId: UUID): IO[Option[String]] =
    tokens.token(userId, Provider).map(_.flatMap(t => box.flatMap(_.decrypt(t))))

  def connected(userId: UUID): IO[Boolean] = token(userId).map(_.isDefined)

  /** 本人のトークンで、通知先に選べるチャンネル（参加していて、アーカイブしていない公開・非公開のチャンネル）. */
  def channels(userId: UUID): IO[Either[String, List[StuiChannelDto]]] =
    (cfg, box) match {
      case (None, _) => IO.pure(Left("stui との連携が設定されていません"))
      case (_, None) => IO.pure(Left("stui と連携するには APP_SECRET を設定してください"))
      case (Some(c), Some(_)) =>
        token(userId).flatMap {
          case None => IO.pure(Left("stui と連携してください"))
          case Some(t) =>
            val req = Request[IO](
              Method.GET,
              Uri.unsafeFromString(c.apiUrl + "/api/channels")
            ).putHeaders(bearer(t), "Accept" -> "application/json")
            http.run(req).use { r =>
              if r.status == Status.Unauthorized then
                tokens.deleteToken(userId, Provider).as(Left("stui との連携が切れました。連携し直してください"))
              else if !r.status.isSuccess then IO.pure(Left(s"stui が ${r.status.code} を返しました"))
              else
                r.as[List[Json]].map(cs =>
                  Right(cs.flatMap { j =>
                    val h = j.hcursor
                    (h.get[UUID]("id"), h.get[String]("name"), h.get[String]("kind")).tupled.toOption
                      .filter((_, _, k) => k == "public" || k == "private")
                      .filterNot(_ => h.get[Boolean]("archived").contains(true))
                      .filterNot(_ => h.get[Boolean]("member").contains(false))
                      .map((id, name, kind) => StuiChannelDto(id, name, kind, h.get[String]("role").contains("admin")))
                  })
                )
            }.timeout(15.seconds).handleError(_ => Left("stui に接続できませんでした"))
        }
    }

  /**
   * 通知先「stui のチャンネル」へ送る（作った人のトークンで、ボット「skotare」として投稿する）.
   * 失敗は例外にする（行列が間隔を延ばして送り直す）. トークンが失効していたら、その人の連携を外す.
   */
  def post(n: NotifierRow, text: String): IO[Unit] = {
    val fail = (m: String) => IO.raiseError[Unit](new IllegalStateException(m))
    (cfg, n.config.channelId, n.config.connectedBy) match {
      case (None, _, _) => fail("stui との連携が設定されていません（STUI_URL など）")
      case (_, None, _) | (_, _, None) => fail("stui のチャンネルが選ばれていません")
      case (Some(c), Some(ch), Some(owner)) =>
        token(owner).flatMap {
          case None => fail("この通知先を作った人の stui との連携が切れています（通知先を開き、連携し直してチャンネルを選び直してください）")
          case Some(t) =>
            val req = Request[IO](Method.POST, Uri.unsafeFromString(c.apiUrl + "/api/apps/notify"))
              .withEntity(Json.obj("text" -> text.asJson, "channel" -> ch.toString.asJson))
              .putHeaders(bearer(t), "User-Agent" -> "skotare")
            http.run(req).use(r => IO.pure(r.status.code)).timeout(20.seconds).flatMap {
              case code if code >= 200 && code < 300 => IO.unit
              case 401 => tokens.deleteToken(owner, Provider) *> fail("stui との連携が切れています（連携し直してください）")
              case 403 => fail("stui に投稿できません（作った人がチャンネルを抜けたか、チャンネルがアーカイブされました）")
              case 404 => fail("stui のチャンネルが見つかりません")
              case code => fail(s"HTTP $code")
            }
        }
    }
  }

  /**
   * 本人のトークンで、チャンネルに受信 Webhook を作る（チャンネルの管理者だけが作れる）. (Webhook の ID, 送る URL).
   * stui が返す URL は利用者向けの公開 URL なので、サーバーから呼ぶ URL（STUI_INTERNAL_URL）があれば置き換える.
   */
  def createHook(userId: UUID, channelId: UUID, name: String): IO[Either[String, (UUID, String)]] =
    withToken(userId) {
      (c, t) =>
        val body = Json.obj(
          "channelId" -> channelId.asJson,
          "name" -> s"skotare: $name".take(100).asJson,
          "username" -> "skotare".asJson,
          "iconUrl" -> Json.Null,
          "enabled" -> true.asJson
        )
        http.run(Request[IO](Method.POST, Uri.unsafeFromString(c.apiUrl + "/api/hooks/incoming")).withEntity(body)
          .putHeaders(bearer(t), "Accept" -> "application/json")).use { r =>
          if r.status == Status.Unauthorized then
            tokens.deleteToken(userId, Provider).as(Left("stui との連携が切れました。連携し直してください"))
          else if r.status == Status.Forbidden then IO.pure(Left("受信 Webhook を作れるのは、チャンネルの管理者だけです"))
          else if !r.status.isSuccess then IO.pure(Left(s"stui が ${r.status.code} を返しました"))
          else
            r.as[Json].map { j =>
              (j.hcursor.downField("hook").get[UUID]("id"), j.hcursor.get[String]("url")).tupled.toOption
                .map((id, url) =>
                  id -> (if url.startsWith(c.url + "/") then c.apiUrl + url.drop(c.url.length) else url)
                )
                .toRight("stui の応答が正しくありません")
            }
        }.timeout(15.seconds).handleError(_ => Left("stui に接続できませんでした"))
    }

  /** 作った受信 Webhook を消す（本人のトークンで. 消せなくても続ける）. */
  def deleteHook(userId: UUID, hookId: UUID): IO[Unit] =
    withToken(userId)((c, t) =>
      http.run(Request[IO](
        Method.DELETE,
        Uri.unsafeFromString(s"${c.apiUrl}/api/hooks/incoming/$hookId")
      ).putHeaders(bearer(t)))
        .use(_ => IO.pure(Right(()))).timeout(10.seconds)
    ).void.handleError(_ => ())

  private def withToken[A](userId: UUID)(f: (LinkService, String) => IO[Either[String, A]]): IO[Either[String, A]] =
    cfg.fold(IO.pure(Left("stui との連携が設定されていません")))(c =>
      token(userId).flatMap(_.fold(IO.pure(Left("stui と連携してください")))(t => f(c, t)))
    )

}
