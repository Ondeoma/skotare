package skotare.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import skotare.shared.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** 画面全体の状態. 監視の一覧と全体の様子は15秒ごとに読み直す（画面を開いている間）. */
object Store {

  val me: RV[Option[MeDto]] = RV(None)
  val config: RV[Option[ClientConfigDto]] = RV(None)
  val monitors: RV[List[MonitorDto]] = RV(Nil)
  val summary: RV[Option[SummaryDto]] = RV(None)
  val loading: RV[Boolean] = RV(false)

  def go(path: String): Unit = Router.go(path)

  // ---------------------------------------------------------------- 監視

  def refresh(): Unit = if (me.v.isDefined) {
    loading := true
    Api.monitors().zip(Api.summary()).onComplete { r =>
      loading := false
      r.foreach {
        (ms, s) => monitors := ms; summary := Some(s)
      }
    }
  }

  private var polling: Option[EffectHandle] = None

  /** すぐ読み、あとは定期的に読み直す（タブが裏にある間は止め、戻ったらすぐ読む）. */
  def start(): Unit = if (polling.isEmpty) {
    refresh()
    polling = Some(useVisiblePolling(15000)(refresh()))
  }

  def monitor(id: java.util.UUID): Option[MonitorDto] = monitors.v.find(_.id == id)
}

/** アイコン（sbirds-lib の parts/Avatar.st）に出す人. */
extension (u: UserBrief) {
  def person: Person = Person(u.id.toString, u.name, u.avatarUrl)
}

/** この端末だけの表示の設定（localStorage. 使えないときは既定値）. */
object Prefs extends sbirds.web.LocalPrefs("skotare")

/** 画面の色（自動 = OS の設定、ライト、ダーク）. */
object Theme extends sbirds.web.ThemeSwitch(Prefs, sbirds.web.ThemeSwitch.Plain)
