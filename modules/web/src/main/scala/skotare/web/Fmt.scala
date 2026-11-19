package skotare.web

import scala.scalajs.js

/** 表示の書式. */
object Fmt {
  private def pad(
    n: Double,
    w: Int = 2
  ): String = { val s = n.toInt.toString; "0" * math.max(0, w - s.length) + s }

  /** 日時（ISO 8601）をこの端末の時刻で（2026-11-17 09:08）. */
  def dateTime(
    iso: String
  ): String = {
    val d = new js.Date(iso)
    if (d.getTime().isNaN) iso
    else
      s"${d.getFullYear().toInt}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}"
  }

  def time(
    iso: String
  ): String = { val d = new js.Date(iso); s"${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}" }

  /** 「3分前」など. */
  def ago(
    iso: String
  ): String = {
    val diff = (js.Date.now() - js.Date.parse(iso)) / 1000
    if (diff < 60) "たった今"
    else if (diff < 3600) s"${(diff / 60).toInt}分前"
    else if (diff < 86400) s"${(diff / 3600).toInt}時間前"
    else if (diff < 86400 * 30) s"${(diff / 86400).toInt}日前"
    else dateTime(iso).take(10)
  }

  /** 期間（3時間12分）. to が None なら今まで. */
  def span(
    from: String,
    to: Option[String]
  ): String = {
    val sec = ((to.map(js.Date.parse).getOrElse(js.Date.now()) - js.Date.parse(from)) / 1000).toLong.max(0)
    if (sec < 60) s"${sec}秒"
    else if (sec < 3600) s"${sec / 60}分"
    else if (sec < 86400) s"${sec / 3600}時間${(sec % 3600) / 60}分"
    else s"${sec / 86400}日${(sec % 86400) / 3600}時間"
  }

  /** 応答時間（ミリ秒. なければ —）. */
  def ms(
    v: Option[Int]
  ): String = v.fold("—")(x => s"${x}ms")

  def pct(
    v: Option[Double]
  ): String = v.fold("—") { x => if (x >= 99.995) "100%" else f"$x%.2f%%" }

  def interval(
    sec: Int
  ): String = if (sec % 3600 == 0) s"${sec / 3600}時間" else if (sec % 60 == 0) s"${sec / 60}分" else s"${sec}秒"

  /** 状態の色の class. */
  def statusClass(
    s: String
  ): String = s"st st-$s"

  /** 稼働率の帯の色の class（100%・99%以上・それ未満・データなし）. */
  def barClass(
    u: Option[Double]
  ): String = u match {
    case None => "bar none"
    case Some(x) if x >= 99.9 => "bar ok"
    case Some(x) if x >= 95 => "bar warn"
    case Some(_) => "bar bad"
  }
}
