package skotare.api

import cats.effect.*
import cats.effect.std.Semaphore
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.Authorization
import skotare.auth.SecretBox
import skotare.core.Probe
import skotare.db.*
import skotare.observe.{Log, Metrics, Telemetry}
import skotare.push.{PushMessage, PushService, PushStore}
import skotare.shared.*

import java.time.Instant
import scala.concurrent.duration.*

/** node はこのノードの名前（確認の結果に残す）、region は拠点の名前、concurrency は同時に確かめる数. */
final case class WorkerConfig(node: String, publicUrl: String, version: String, concurrency: Int = 16, region: String = "default")

/** 通知の文面（stui・skereru で共通）. */
object Messages:
  private def str(j: Json, path: String*): String = path.foldLeft(j.hcursor: io.circe.ACursor)(_.downField(_)).as[String].getOrElse("")

  def subject(p: Json): String =
    val name = str(p, "monitor", "name")
    val region = str(p, "region", "name")
    str(p, "event") match
      case "down" => s"🔴 停止: $name"
      case "up"   => s"🟢 復旧: $name"
      case "closed" => s"⚪ 障害を閉じました: $name"
      case "region.suspect" => s"⚠️ 拠点の異常の疑い: $region"
      case "region.offline" => s"⚠️ 拠点の停止: $region"
      case "region.ok"      => s"✅ 拠点の回復: $region"
      case _      => s"🔔 テスト: $name"

  /** stui のアイコン. */
  def emoji(p: Json): String = str(p, "event") match
    case "down" => ":red_circle:"
    case "closed" => ":white_circle:"
    case e if e.startsWith("region.") && e != "region.ok" => ":warning:"
    case _ => ":large_green_circle:"

  /**
   * 監視先の側が決められる文字列（応答の説明・DNS の答え・ping の出力など）を、Markdown の code span に入れる
   * （リンクや強調を差し込ませない）. 改行とバッククォートは置き換える.
   */
  def code(s: String): String = "`" + s.replace('`', '\'').replace('\n', ' ').replace('\r', ' ').take(500) + "`"

  /** メールなどの文字だけの本文（Markdown の印を外す）. */
  def plain(p: Json): String = body(p).replace("**", "").replace("`", "").replaceAll("\\\\(.)", "$1")

  /** Markdown で意味を持つ記号を打ち消す（監視の名前に、強調やリンクを差し込ませない）. */
  def esc(s: String): String = s.flatMap(ch => if "\\`*_[]()<>#!|~".contains(ch) then s"\\$ch" else ch.toString)

  /** 本文（Markdown）. */
  def body(p: Json): String =
    if str(p, "event").startsWith("region.") then regionBody(p) else monitorBody(p)

  private def regionBody(p: Json): String =
    val note = str(p, "event") match
      case "region.ok" => "この拠点の結果を、ふたたび判定に数えます。"
      case _ => "skotare 側（この拠点）の問題の疑いがあるため、この拠点の結果は判定に数えません（監視先の停止とは判断しません）。"
    s"**${esc(subject(p))}**\n${code(str(p, "message"))}\n$note\n[状況を見る](${str(p, "url")})"

  private def monitorBody(p: Json): String =
    val since = p.hcursor.get[String]("downSince").toOption
      .flatMap(s => scala.util.Try(Instant.parse(s)).toOption)
      .flatMap(s => scala.util.Try(Instant.parse(str(p, "at"))).toOption.map(a => java.time.Duration.between(s, a)))
      .map(d => s"\n停止していた時間: ${d.toHours}時間${d.toMinutesPart}分${d.toSecondsPart}秒").getOrElse("")
    s"**${esc(subject(p))}**\n対象: ${code(str(p, "monitor", "target"))}\n${code(str(p, "message"))}$since\n[詳しく見る](${str(p, "url")})"

object Senders:
  /** 送れなかった理由（短く. 相手の応答の本文や内部の詳細は含めない）. */
  def describe(e: Throwable): String = e match
    case _: java.util.concurrent.TimeoutException => "時間内に応答がありません"
    case _: java.net.ConnectException | _: java.nio.channels.UnresolvedAddressException => "接続できません"
    case _: IllegalArgumentException | _: IllegalStateException => Option(e.getMessage).getOrElse("設定が正しくありません")
    case _ => Option(e.getMessage).filter(_.startsWith("HTTP ")).getOrElse(e.getClass.getSimpleName)

/** 通知先へ送る（stui の受信 Webhook・skereru の送信 API・汎用の Webhook）. */
final class Senders(http: Client[IO], box: Option[SecretBox], val publicUrl: String):

  private def secretOf(n: NotifierRow): Either[String, String] =
    if n.secret.isEmpty then Left("秘密の値（URL・APIキー）が設定されていません")
    else box.toRight("APP_SECRET が設定されていないため、通知先の秘密の値を読めません").flatMap(_.decrypt(n.secret).toRight("秘密の値を読めません（APP_SECRET を変えましたか？）"))

  private def post(uri: String, body: Json, auth: Option[String] = None): IO[Unit] =
    IO.fromEither(Uri.fromString(uri).left.map(_ => new IllegalArgumentException("URL が正しくありません"))).flatMap { u =>
      val req = Request[IO](Method.POST, u).withEntity(body)
      http.run(auth.fold(req)(t => req.putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, t))))).use { r =>
        if r.status.isSuccess then IO.unit
        // 相手の本文は返さない（内部のホストの応答を、画面・記録へ持ち出させない）
        else IO.raiseError(new RuntimeException(s"HTTP ${r.status.code}"))
      }.timeout(20.seconds)
    }

  def send(n: NotifierRow, payload: Json): IO[Unit] =
    IO.fromEither(secretOf(n).left.map(new IllegalStateException(_))).flatMap { secret =>
      Telemetry.span("notify.send", "kind" -> n.kind)(n.kind match
        // stui: チャンネルの受信 Webhook（本文は Markdown）
        case NotifierKind.Stui =>
          post(secret, Json.obj("text" -> Messages.body(payload).asJson, "username" -> "skotare".asJson,
            "icon_emoji" -> Messages.emoji(payload).asJson))
        // skereru: メールを送る（APIキーの send のスコープ）
        case NotifierKind.Skereru =>
          val text = Messages.plain(payload)
          post(s"${n.config.baseUrl.stripSuffix("/")}/api/send", Json.obj(
            "from" -> n.config.from.asJson, "to" -> n.config.to.asJson, "cc" -> Json.arr(), "bcc" -> Json.arr(),
            "subject" -> s"[skotare] ${Messages.subject(payload)}".asJson, "text" -> text.asJson, "html" -> Json.Null,
            "uploadIds" -> Json.arr(), "inReplyTo" -> Json.Null, "forwardOf" -> Json.Null, "draftId" -> Json.Null), Some(secret))
        case _ => post(secret, payload)
      ) <* Metrics.count("skotare.notifications.sent", "kind" -> n.kind)
    }

  /** 試しに送る本文. */
  def testPayload(n: NotifierRow): Json = Json.obj(
    "event" -> "test".asJson, "monitor" -> Json.obj("name" -> n.name.asJson, "target" -> "-".asJson),
    "message" -> "skotare からのテストの通知です".asJson, "at" -> Instant.now().toString.asJson, "url" -> publicUrl.asJson)

/**
 * 状態の変化をその場で知らせる: ログ・メトリクス・プッシュ通知（通知先への送信は行列から）.
 * 確認（Worker）・拠点の判定・監視の変更（ルート）のどれで変わっても、同じように知らせる.
 */
final class Announcer(push: Option[(PushStore, PushService)], publicUrl: String):

  private def pushAll(msg: PushMessage): IO[Unit] =
    push.traverse_((store, svc) => store.subscribers.flatMap(_.traverse_(u => svc.send(u, msg))).start.void)

  def transition(t: Transition): IO[Unit] =
    val msg = PushMessage(Messages.subject(t.payload(publicUrl)), s"${t.target}\n${t.message}".take(300),
      s"/monitors/${t.monitorId}", Some(s"monitor-${t.monitorId}"))
    val line = s"[skotare] ${t.event.toUpperCase} ${t.name} (${t.target}): ${t.message}"
    (if t.down then Log.warn(line) else Log.info(line)) *>
      Metrics.count("skotare.monitor.transitions", "to" -> t.to, "kind" -> t.kind) *> pushAll(msg)

  def region(c: RegionChange): IO[Unit] =
    (if c.to == RegionHealth.Ok then Log.info(s"[skotare] REGION ${c.region} ${c.from} -> ${c.to}")
     else Log.warn(s"[skotare] REGION ${c.region} ${c.from} -> ${c.to}: ${c.reason}")) *>
      Metrics.count("skotare.region.transitions", "to" -> c.to, "region" -> c.region) *>
      pushAll(PushMessage(Messages.subject(c.payload(publicUrl)), c.reason.take(300), "/", Some(s"region-${c.region}")))

/**
 * 監視を回す（どのノードでも同じものを動かす. 同じ拠点のノードは DB で取り合うので、台数を増やすと分担する. 拠点ごとに全ての監視を確かめる）.
 *  - 確認: 1秒ごとに、空いている数だけ自分の拠点で時刻の来た監視を取り、確かめて結果を残す
 *  - 判定: 15秒ごとに、拠点の健全性を判定し直す（1つのノードだけが行う）
 *  - 通知: 2秒ごとに、行列から取って送る（失敗したら間隔を延ばして送り直す）
 *  - 心拍: 30秒ごとにノードの生存を書く
 *  - 掃除: 1時間ごとに古い結果を消す
 */
final class Worker(monitors: MonitorStore, notifiers: NotifierStore, nodes: NodeStore, senders: Senders,
    announcer: Announcer, cfg: WorkerConfig):

  private def every(d: FiniteDuration, what: String)(io: IO[Unit]): IO[Nothing] =
    (io.handleErrorWith(e => Log.warn(s"[skotare] $what failed: $e")) *> IO.sleep(d)).foreverM

  def run: IO[Unit] =
    for
      started <- IO.realTimeInstant
      sem     <- Semaphore[IO](cfg.concurrency.toLong)
      _ <- (
             // 心拍は30秒ごと. 拠点で確かめる監視の行は、起動直後と10分ごとに補う
             Ref[IO].of(0L).flatMap(n => every(30.seconds, "heartbeat")(n.getAndUpdate(_ + 1).flatMap(i =>
               nodes.beat(cfg.node, cfg.region, cfg.version, started, fill = i % 20 == 0)))),
             every(1.second, "check")(checkTick(sem)),
             IO.sleep(5.seconds) *> every(15.seconds, "judge")(judgeTick),
             every(2.seconds, "notify")(notifyTick),
             IO.sleep(1.minute) *> every(1.hour, "purge")((monitors.purge, notifiers.purge, nodes.purge).tupled.void)
           ).parTupled
    yield ()

  private def checkTick(sem: Semaphore[IO]): IO[Unit] =
    sem.available.flatMap { free =>
      if free <= 0 then IO.unit
      else monitors.claimDue(cfg.region, free.toInt).flatMap(_.traverse_(m => sem.acquire *> check(m).guarantee(sem.release).start.void))
    }

  def check(m: DueMonitor): IO[Unit] =
    Telemetry.span("monitor.check", "monitor.kind" -> m.kind, "monitor.name" -> m.name, "region" -> m.region) {
      for
        r  <- Probe.run(m.kind, m.config, m.timeoutMs.millis)
        at <- IO.realTimeInstant
        _  <- Metrics.count("skotare.checks", "kind" -> m.kind, "result" -> (if r.ok then "ok" else "fail"), "region" -> m.region)
        _  <- r.latencyMs.traverse_(ms => Metrics.record("skotare.check.latency", ms.toDouble, "ms", "kind" -> m.kind, "region" -> m.region))
        t  <- monitors.record(m.id, m.region, r, cfg.node, at, cfg.publicUrl)
        _  <- t.traverse_(announcer.transition)
      yield ()
    }.handleErrorWith(e => Log.warn(s"[skotare] check ${m.name} failed: $e"))

  /** 拠点の健全性を判定し直す（15秒ごと. テストからも呼ぶ）. */
  def judgeTick: IO[Unit] =
    IO.realTimeInstant.flatMap(at => monitors.judgeRegions(at, cfg.publicUrl)).flatMap { (rs, ts) =>
      rs.traverse_(announcer.region) *> ts.traverse_(announcer.transition)
    }

  /** 行列から取って送る（2秒ごと. テストからも呼ぶ）. */
  def notifyTick: IO[Unit] =
    notifiers.claim(20).flatMap(_.traverse_ { item =>
      senders.send(item.notifier, item.payload).attempt.flatMap {
        case Right(_) => notifiers.sent(item.id)
        case Left(e) =>
          Log.warn(s"[skotare] notify ${item.notifier.name} failed (${item.attempts}): ${e.getMessage}") *>
            Metrics.count("skotare.notifications.failed", "kind" -> item.notifier.kind) *>
            notifiers.failed(item.id, item.attempts, Senders.describe(e))
      }
    })
