package skotare.web

import io.circe.*
import io.circe.syntax.*
import org.scalajs.dom
import skotare.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** バックエンドの REST API（呼び出しの共通部分は sbirds.web.ApiBase. 401 はログインへ、エラーは {"error": ...} の文言にする）. */
object Api extends sbirds.web.ApiBase {

  // ---------------------------------------------------------------- 認証・自分

  def clientConfig(): Future[ClientConfigDto] = get[ClientConfigDto]("/api/client-config")
  def me(): Future[MeDto] = get[MeDto]("/api/me")
  def devLogin(name: String, email: String, returnTo: String): Future[Json] =
    post[Json](
      "/auth/dev-login",
      Json.obj("name" -> name.asJson, "email" -> email.asJson, "returnTo" -> returnTo.asJson)
    )
  def logout(): Future[String] = post[Json]("/auth/logout").map(_.hcursor.get[String]("redirect").getOrElse("/"))
  def updateMe(r: UpdateMeReq): Future[MeDto] = patch[MeDto]("/api/me", r.asJson)

  /** アイコンを保存する（画像そのものを送る）・消す. */
  def uploadAvatar(image: dom.Blob): Future[MeDto] =
    putRaw[MeDto]("/api/me/avatar", image, Some(image.`type`))
  def deleteAvatar(): Future[MeDto] = request(dom.HttpMethod.DELETE, "/api/me/avatar").map(as[MeDto])

  // ---------------------------------------------------------------- 連携しているサービス（OAuth）

  def oauthGrants(): Future[List[OAuthGrantDto]] = get[List[OAuthGrantDto]]("/api/oauth/grants")
  def revokeOAuthGrant(clientId: String): Future[Unit] = delete("/api/oauth/grants/" + enc(clientId))

  // ---------------------------------------------------------------- 監視

  def summary(): Future[SummaryDto] = get[SummaryDto]("/api/summary")
  def monitors(): Future[List[MonitorDto]] = get[List[MonitorDto]]("/api/monitors")
  def monitor(id: UUID): Future[MonitorDetailDto] = get[MonitorDetailDto](s"/api/monitors/$id")
  def saveMonitor(id: Option[UUID], r: MonitorReq): Future[MonitorDto] = id match {
    case None => post[MonitorDto]("/api/monitors", r.asJson)
    case Some(id) => put[MonitorDto](s"/api/monitors/$id", r.asJson)
  }
  def testMonitor(r: MonitorReq): Future[ProbeResultDto] = post[ProbeResultDto]("/api/monitors/test", r.asJson)
  def checkNow(id: UUID): Future[Unit] = post[Json](s"/api/monitors/$id/check").map(_ => ())
  def deleteMonitor(id: UUID): Future[Unit] = delete(s"/api/monitors/$id")
  def incidents(open: Boolean): Future[List[IncidentDto]] =
    get[List[IncidentDto]]("/api/incidents?limit=200" + (if (open) "&open=true" else ""))

  // ---------------------------------------------------------------- 通知先・ステータスページ

  def notifiers(): Future[List[NotifierDto]] = get[List[NotifierDto]]("/api/notifiers")
  def saveNotifier(id: Option[UUID], r: NotifierReq): Future[NotifierDto] = id match {
    case None => post[NotifierDto]("/api/notifiers", r.asJson)
    case Some(id) => put[NotifierDto](s"/api/notifiers/$id", r.asJson)
  }
  def testNotifier(id: UUID): Future[Unit] = post[Json](s"/api/notifiers/$id/test").map(_ => ())
  def deleteNotifier(id: UUID): Future[Unit] = delete(s"/api/notifiers/$id")

  // stui との連携（通知先にチャンネルを選ぶ）. 連携を始めるのは画面の遷移（/api/integrations/stui/connect）
  def integrations(): Future[List[IntegrationDto]] = get[List[IntegrationDto]]("/api/integrations")
  def stuiChannels(): Future[List[StuiChannelDto]] = get[List[StuiChannelDto]]("/api/integrations/stui/channels")
  def disconnectStui(): Future[Unit] = delete("/api/integrations/stui/connection")

  def pages(): Future[List[StatusPageDto]] = get[List[StatusPageDto]]("/api/pages")
  def savePage(id: Option[UUID], r: StatusPageReq): Future[StatusPageDto] = id match {
    case None => post[StatusPageDto]("/api/pages", r.asJson)
    case Some(id) => put[StatusPageDto](s"/api/pages/$id", r.asJson)
  }
  def deletePage(id: UUID): Future[Unit] = delete(s"/api/pages/$id")

  /** ゲストに見せるページ（ログインしなくてよい）. */
  def publicStatus(slug: String): Future[PublicStatusDto] = get[PublicStatusDto]("/api/public/status/" + enc(slug))

}
