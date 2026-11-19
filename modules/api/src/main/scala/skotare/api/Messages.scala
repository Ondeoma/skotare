package skotare.api

import io.circe.Json

import java.time.Instant

/** 通知の文面（stui・skereru で共通）. */
object Messages {
  private def str(
    j: Json,
    path: String*
  ): String = path.foldLeft(j.hcursor: io.circe.ACursor)(_.downField(_)).as[String].getOrElse("")

  def subject(
    p: Json
  ): String = {
    val name = str(p, "monitor", "name")
    val region = str(p, "region", "name")
    str(p, "event") match {
      case "down" => s"🔴 停止: $name"
      case "up" => s"🟢 復旧: $name"
      case "closed" => s"⚪ 障害を閉じました: $name"
      case "region.suspect" => s"⚠️ 拠点の異常の疑い: $region"
      case "region.offline" => s"⚠️ 拠点の停止: $region"
      case "region.ok" => s"✅ 拠点の回復: $region"
      case _ => s"🔔 テスト: $name"
    }
  }

  /** stui のアイコン. */
  def emoji(
    p: Json
  ): String = str(p, "event") match {
    case "down" => ":red_circle:"
    case "closed" => ":white_circle:"
    case e if e.startsWith("region.") && e != "region.ok" => ":warning:"
    case _ => ":large_green_circle:"
  }

  /**
   * 監視先の側が決められる文字列（応答の説明・DNS の答え・ping の出力など）を、Markdown の code span に入れる
   * （リンクや強調を差し込ませない）. 改行とバッククォートは置き換える.
   */
  def code(
    s: String
  ): String = "`" + s.replace('`', '\'').replace('\n', ' ').replace('\r', ' ').take(500) + "`"

  /** メールなどの文字だけの本文（Markdown の印を外す）. */
  def plain(
    p: Json
  ): String = body(p).replace("**", "").replace("`", "").replaceAll("\\\\(.)", "$1")

  /** Markdown で意味を持つ記号を打ち消す（監視の名前に、強調やリンクを差し込ませない）. */
  def esc(
    s: String
  ): String = s.flatMap(ch => if "\\`*_[]()<>#!|~".contains(ch) then s"\\$ch" else ch.toString)

  /** 本文（Markdown）. */
  def body(
    p: Json
  ): String =
    if str(p, "event").startsWith("region.") then regionBody(p) else monitorBody(p)

  private def regionBody(
    p: Json
  ): String = {
    val note = str(p, "event") match {
      case "region.ok" => "この拠点の結果を、ふたたび判定に数えます。"
      case _ => "skotare 側（この拠点）の問題の疑いがあるため、この拠点の結果は判定に数えません（監視先の停止とは判断しません）。"
    }
    s"**${esc(subject(p))}**\n${code(str(p, "message"))}\n$note\n[状況を見る](${str(p, "url")})"
  }

  private def monitorBody(
    p: Json
  ): String = {
    val since = p.hcursor.get[String]("downSince").toOption
      .flatMap(s => scala.util.Try(Instant.parse(s)).toOption)
      .flatMap(s => scala.util.Try(Instant.parse(str(p, "at"))).toOption.map(a => java.time.Duration.between(s, a)))
      .map(d => s"\n停止していた時間: ${d.toHours}時間${d.toMinutesPart}分${d.toSecondsPart}秒").getOrElse("")
    s"**${esc(subject(p))}**\n対象: ${code(str(p, "monitor", "target"))}\n${code(str(p, "message"))}$since\n[詳しく見る](${str(p, "url")})"
  }
}
