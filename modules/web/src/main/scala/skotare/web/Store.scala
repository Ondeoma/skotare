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
    case other            => toast(other.getMessage, error = true)
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
      r.foreach { (ms, s) => monitors := ms; summary := Some(s) }
    }
  }

  private var timer: Option[Int] = None

  /** 定期的に読み直す（タブが裏にある間は止める）. */
  def start(): Unit = if (timer.isEmpty) {
    refresh()
    timer = Some(dom.window.setInterval(() => if (dom.document.visibilityState.asInstanceOf[String] == "visible") refresh(), 15000))
    dom.document.addEventListener("visibilitychange", (_: dom.Event) => if (dom.document.visibilityState.asInstanceOf[String] == "visible") refresh())
  }

  def monitor(id: java.util.UUID): Option[MonitorDto] = monitors.v.find(_.id == id)
}

/** URL のクエリ. */
object Query {
  def get(k: String): Option[String] = Option(new dom.URL(dom.window.location.href).searchParams.get(k)).filter(_.nonEmpty)
}

/** 表示の書式. */
object Fmt {
  private def pad(n: Double, w: Int = 2): String = { val s = n.toInt.toString; "0" * math.max(0, w - s.length) + s }

  /** 日時（ISO 8601）をこの端末の時刻で（2026-11-17 09:08）. */
  def dateTime(iso: String): String = {
    val d = new js.Date(iso)
    if (d.getTime().isNaN) iso
    else s"${d.getFullYear().toInt}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}"
  }

  def time(iso: String): String = { val d = new js.Date(iso); s"${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}" }

  /** 「3分前」など. */
  def ago(iso: String): String = {
    val diff = (js.Date.now() - js.Date.parse(iso)) / 1000
    if (diff < 60) "たった今"
    else if (diff < 3600) s"${(diff / 60).toInt}分前"
    else if (diff < 86400) s"${(diff / 3600).toInt}時間前"
    else if (diff < 86400 * 30) s"${(diff / 86400).toInt}日前"
    else dateTime(iso).take(10)
  }

  /** 期間（3時間12分）. to が None なら今まで. */
  def span(from: String, to: Option[String]): String = {
    val sec = ((to.map(js.Date.parse).getOrElse(js.Date.now()) - js.Date.parse(from)) / 1000).toLong.max(0)
    if (sec < 60) s"${sec}秒"
    else if (sec < 3600) s"${sec / 60}分"
    else if (sec < 86400) s"${sec / 3600}時間${(sec % 3600) / 60}分"
    else s"${sec / 86400}日${(sec % 86400) / 3600}時間"
  }

  /** 応答時間（ミリ秒. なければ —）. */
  def ms(v: Option[Int]): String = v.fold("—")(x => s"${x}ms")

  def pct(v: Option[Double]): String = v.fold("—") { x => if (x >= 99.995) "100%" else f"$x%.2f%%" }

  def interval(sec: Int): String = if (sec % 3600 == 0) s"${sec / 3600}時間" else if (sec % 60 == 0) s"${sec / 60}分" else s"${sec}秒"

  /** 状態の色の class. */
  def statusClass(s: String): String = s"st st-$s"
  /** 稼働率の帯の色の class（100%・99%以上・それ未満・データなし）. */
  def barClass(u: Option[Double]): String = u match {
    case None                 => "bar none"
    case Some(x) if x >= 99.9 => "bar ok"
    case Some(x) if x >= 95   => "bar warn"
    case Some(_)              => "bar bad"
  }
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
final case class DialogReq(message: String, okLabel: String, danger: Boolean, input: Option[RV[String]], choices: List[(String, String)],
                           onOk: String => Unit, onCancel: () => Unit)

object Dialog {
  val current: RV[Option[DialogReq]] = RV(None)

  def confirm(message: String, ok: String = "OK", danger: Boolean = true)(f: => Unit): Unit =
    current := Some(DialogReq(message, ok, danger, None, Nil, _ => f, () => ()))

  def prompt(message: String, default: String = "", ok: String = "OK")(f: String => Unit): Unit =
    current := Some(DialogReq(message, ok, danger = false, Some(RV(default)), Nil, f, () => ()))

  /** 選択肢（値 → 表示名）から1つ選ぶ. */
  def choose(message: String, choices: List[(String, String)], ok: String = "OK", danger: Boolean = false,
             onCancel: () => Unit = () => ())(f: String => Unit): Unit =
    current := Some(DialogReq(message, ok, danger, Some(RV(choices.headOption.map(_._1).getOrElse(""))), choices, f, onCancel))

  def close(ok: Boolean): Unit = current.v.foreach { r =>
    current := None
    if (ok) r.onOk(r.input.map(_.v).getOrElse("")) else r.onCancel()
  }
}

/** この端末だけの表示の設定（localStorage. 使えないときは既定値）. */
object Prefs {
  def get(key: String, default: String): String =
    scala.util.Try(Option(dom.window.localStorage.getItem("skotare." + key))).toOption.flatten.getOrElse(default)

  def set(key: String, value: String): Unit = scala.util.Try(dom.window.localStorage.setItem("skotare." + key, value))
}

/** 画面の色（自動 = OS の設定、ライト、ダーク）. */
object Theme {
  val current: RV[String] = RV(Prefs.get("theme", "system"))

  def apply(): Unit = {
    val el = dom.document.documentElement
    if (current.v == "system") el.removeAttribute("data-theme") else el.setAttribute("data-theme", current.v)
  }

  def set(v: String): Unit = { current := v; Prefs.set("theme", v); apply() }

  val options: List[(String, String)] = List("system" -> "自動（OS に合わせる）", "light" -> "ライト", "dark" -> "ダーク")
}
