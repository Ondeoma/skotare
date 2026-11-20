package skotare.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom
import skotare.shared.*

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.{Failure, Success}

final case class Toast(id: Int, text: String, error: Boolean)

/** 画面全体の状態. 監視の一覧と全体の様子は15秒ごとに読み直す（画面を開いている間）. */
object Store {

  val me: RV[Option[MeDto]] = RV(None)
  val config: RV[Option[ClientConfigDto]] = RV(None)
  val monitors: RV[List[MonitorDto]] = RV(Nil)
  val summary: RV[Option[SummaryDto]] = RV(None)
  val loading: RV[Boolean] = RV(false)
  val toasts: RV[List[Toast]] = RV(Nil)

  // ---------------------------------------------------------------- トースト

  private var toastSeq = 0

  def toast(text: String, error: Boolean = false): Unit = {
    toastSeq += 1
    val t = Toast(toastSeq, text, error)
    toasts := (toasts.v :+ t).takeRight(4)
    dom.window.setTimeout(() => dismiss(t.id), if (error) 9000 else 5000)
  }

  def dismiss(id: Int): Unit = toasts := toasts.v.filterNot(_.id == id)

  def error(e: Throwable): Unit = e match {
    case ApiError(401, _) => ()
    case other => toast(other.getMessage, error = true)
  }

  /** Future の失敗をトーストで知らせる. */
  def report[A](f: Future[A])(ok: A => Unit): Unit = f.onComplete {
    case Success(a) => ok(a)
    case Failure(e) => error(e)
  }

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

  private var timer: Option[Int] = None

  /** 定期的に読み直す（タブが裏にある間は止める）. */
  def start(): Unit = if (timer.isEmpty) {
    refresh()
    timer = Some(dom.window.setInterval(
      () => if (dom.document.visibilityState.asInstanceOf[String] == "visible") refresh(),
      15000
    ))
    dom.document.addEventListener(
      "visibilitychange",
      (_: dom.Event) => if (dom.document.visibilityState.asInstanceOf[String] == "visible") refresh()
    )
  }

  def monitor(id: java.util.UUID): Option[MonitorDto] = monitors.v.find(_.id == id)
}

/** URL のクエリ. */
object Query {
  def get(k: String): Option[String] =
    Option(new dom.URL(dom.window.location.href).searchParams.get(k)).filter(_.nonEmpty)
}

/** DOM の補助. */
object Dom {
  def value(e: dom.Event): String = e.target.asInstanceOf[js.Dynamic].value.asInstanceOf[String]
  def checked(e: dom.Event): Boolean = e.target.asInstanceOf[dom.HTMLInputElement].checked

  def copy(text: String, message: String = "コピーしました"): Unit = {
    dom.window.navigator.clipboard.writeText(text)
    Store.toast(message)
  }
}

/** 画面内のダイアログ（ブラウザ標準の confirm・prompt の代わり）. 表示は parts/DialogHost.st. */
final case class DialogReq(message: String,
                           okLabel: String,
                           danger: Boolean,
                           input: Option[RV[String]],
                           choices: List[(String, String)],
                           onOk: String => Unit,
                           onCancel: () => Unit)

/** この端末だけの表示の設定（localStorage. 使えないときは既定値）. */
object Prefs extends sbirds.web.LocalPrefs("skotare")

/** 画面の色（自動 = OS の設定、ライト、ダーク）. */
object Theme extends sbirds.web.ThemeSwitch(Prefs, sbirds.web.ThemeSwitch.Plain)
