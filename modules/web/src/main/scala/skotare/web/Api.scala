package skotare.web

import io.circe.*
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalajs.dom
import skotare.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/** API の呼び出しのエラー. */
final case class ApiError(
  status: Int,
  message: String) extends Exception(message)

/** バックエンドの REST API. 401 はログインへ、エラーは {"error": ...} の文言にする. */
object Api {

  private def request(
    method: dom.HttpMethod,
    url: String,
    body: Option[Json] = None
  ): Future[Option[Json]] =
    send(method, url, body.map(b => (b.noSpaces, "application/json")))

  /** 本文（と Content-Type）を付けて送る. JSON 以外（画像など）もそのまま送れる. */
  private def send(
    method: dom.HttpMethod,
    url: String,
    payload: Option[(dom.BodyInit, String)]
  ): Future[Option[Json]] = {
    val init = new dom.RequestInit {}
    init.method = method
    init.credentials = dom.RequestCredentials.`same-origin`
    // 更新の要求には X-Skotare を付ける（サーバーはこれのない更新を CSRF として断る）
    val hs = js.Dictionary[String]("X-Skotare" -> "1")
    payload.foreach {
      (
        b,
        ct
      ) =>
        hs("Content-Type") = ct
        init.body = b
    }
    init.headers = hs
    dom.fetch(url, init).toFuture.flatMap { res =>
      if (res.status == 401) {
        val back = dom.window.location.pathname + dom.window.location.search
        if (!dom.window.location.pathname.startsWith("/login"))
          dom.window.location.href = "/auth/login?returnTo=" + js.URIUtils.encodeURIComponent(back)
        Future.failed(ApiError(401, "ログインしてください"))
      } else if (!res.ok) {
        res.text().toFuture.recover(_ => "").flatMap { t =>
          val msg = parse(
            t
          ).toOption.flatMap(_.hcursor.get[String]("error").toOption).getOrElse(s"${res.status} ${res.statusText}")
          Future.failed(ApiError(res.status, msg))
        }
      } else if (res.status == 204) Future.successful(None)
      else res.text().toFuture.map(t => if (t.isEmpty) None else Some(parse(t).fold(throw _, identity)))
    }
  }

  private def as[A: Decoder](
    j: Option[Json]
  ): A = j.getOrElse(Json.Null).as[A].fold(e => throw ApiError(0, e.getMessage), identity)

  def get[A: Decoder](
    url: String
  ): Future[A] = request(dom.HttpMethod.GET, url).map(as[A])
  def post[A: Decoder](
    url: String,
    body: Json = Json.obj()
  ): Future[A] = request(dom.HttpMethod.POST, url, Some(body)).map(as[A])
  def put[A: Decoder](
    url: String,
    body: Json
  ): Future[A] = request(dom.HttpMethod.PUT, url, Some(body)).map(as[A])
  def patch[A: Decoder](
    url: String,
    body: Json
  ): Future[A] = request(dom.HttpMethod.PATCH, url, Some(body)).map(as[A])
  def delete(
    url: String
  ): Future[Unit] = request(dom.HttpMethod.DELETE, url).map(_ => ())
  def delete(
    url: String,
    body: Json
  ): Future[Unit] = request(dom.HttpMethod.DELETE, url, Some(body)).map(_ => ())

  def enc(
    s: String
  ): String = js.URIUtils.encodeURIComponent(s)

  // ---------------------------------------------------------------- 認証・自分

  def clientConfig(): Future[ClientConfigDto] = get[ClientConfigDto]("/api/client-config")
  def me(): Future[MeDto] = get[MeDto]("/api/me")
  def devLogin(
    name: String,
    email: String,
    returnTo: String
  ): Future[Json] =
    post[Json](
      "/auth/dev-login",
      Json.obj("name" -> name.asJson, "email" -> email.asJson, "returnTo" -> returnTo.asJson)
    )
  def logout(): Future[String] = post[Json]("/auth/logout").map(_.hcursor.get[String]("redirect").getOrElse("/"))
  def updateMe(
    r: UpdateMeReq
  ): Future[MeDto] = patch[MeDto]("/api/me", r.asJson)

  /** アイコンを保存する（画像そのものを送る）・消す. */
  def uploadAvatar(
    image: dom.Blob
  ): Future[MeDto] = send(dom.HttpMethod.PUT, "/api/me/avatar", Some((image, image.`type`))).map(as[MeDto])
  def deleteAvatar(): Future[MeDto] = send(dom.HttpMethod.DELETE, "/api/me/avatar", None).map(as[MeDto])

  // ---------------------------------------------------------------- API トークン

  def tokens(): Future[List[ApiTokenDto]] = get[List[ApiTokenDto]]("/api/tokens")
  def createToken(
    r: ApiTokenReq
  ): Future[ApiTokenCreated] = post[ApiTokenCreated]("/api/tokens", r.asJson)
  def revokeToken(
    id: UUID
  ): Future[Unit] = delete(s"/api/tokens/$id")

  // ---------------------------------------------------------------- 連携しているサービス（OAuth）

  def oauthGrants(): Future[List[OAuthGrantDto]] = get[List[OAuthGrantDto]]("/api/oauth/grants")
  def revokeOAuthGrant(
    clientId: String
  ): Future[Unit] = delete("/api/oauth/grants/" + enc(clientId))

  // ---------------------------------------------------------------- 監視

  def summary(): Future[SummaryDto] = get[SummaryDto]("/api/summary")
  def monitors(): Future[List[MonitorDto]] = get[List[MonitorDto]]("/api/monitors")
  def monitor(
    id: UUID
  ): Future[MonitorDetailDto] = get[MonitorDetailDto](s"/api/monitors/$id")
  def saveMonitor(
    id: Option[UUID],
    r: MonitorReq
  ): Future[MonitorDto] = id match {
    case None => post[MonitorDto]("/api/monitors", r.asJson)
    case Some(id) => put[MonitorDto](s"/api/monitors/$id", r.asJson)
  }
  def testMonitor(
    r: MonitorReq
  ): Future[ProbeResultDto] = post[ProbeResultDto]("/api/monitors/test", r.asJson)
  def checkNow(
    id: UUID
  ): Future[Unit] = post[Json](s"/api/monitors/$id/check").map(_ => ())
  def deleteMonitor(
    id: UUID
  ): Future[Unit] = delete(s"/api/monitors/$id")
  def incidents(
    open: Boolean
  ): Future[List[IncidentDto]] = get[List[IncidentDto]]("/api/incidents?limit=200" + (if (open) "&open=true" else ""))

  // ---------------------------------------------------------------- 通知先・ステータスページ

  def notifiers(): Future[List[NotifierDto]] = get[List[NotifierDto]]("/api/notifiers")
  def saveNotifier(
    id: Option[UUID],
    r: NotifierReq
  ): Future[NotifierDto] = id match {
    case None => post[NotifierDto]("/api/notifiers", r.asJson)
    case Some(id) => put[NotifierDto](s"/api/notifiers/$id", r.asJson)
  }
  def testNotifier(
    id: UUID
  ): Future[Unit] = post[Json](s"/api/notifiers/$id/test").map(_ => ())
  def deleteNotifier(
    id: UUID
  ): Future[Unit] = delete(s"/api/notifiers/$id")

  // stui との連携（通知先にチャンネルを選ぶ）. 連携を始めるのは画面の遷移（/api/integrations/stui/connect）
  def integrations(): Future[List[IntegrationDto]] = get[List[IntegrationDto]]("/api/integrations")
  def stuiChannels(): Future[List[StuiChannelDto]] = get[List[StuiChannelDto]]("/api/integrations/stui/channels")
  def disconnectStui(): Future[Unit] = delete("/api/integrations/stui/connection")

  def pages(): Future[List[StatusPageDto]] = get[List[StatusPageDto]]("/api/pages")
  def savePage(
    id: Option[UUID],
    r: StatusPageReq
  ): Future[StatusPageDto] = id match {
    case None => post[StatusPageDto]("/api/pages", r.asJson)
    case Some(id) => put[StatusPageDto](s"/api/pages/$id", r.asJson)
  }
  def deletePage(
    id: UUID
  ): Future[Unit] = delete(s"/api/pages/$id")

  /** ゲストに見せるページ（ログインしなくてよい）. */
  def publicStatus(
    slug: String
  ): Future[PublicStatusDto] = get[PublicStatusDto]("/api/public/status/" + enc(slug))

}
