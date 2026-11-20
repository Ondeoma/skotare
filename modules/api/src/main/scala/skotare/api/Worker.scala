package skotare.api

import cats.effect.*
import cats.effect.std.Semaphore
import cats.syntax.all.*
import skotare.core.{ErrorCode, Probe}
import skotare.db.*
import sbirds.observe.{Log, Metrics, Telemetry}

import scala.concurrent.duration.*

/**
 * node はこのノードの名前（確認の結果に残す）、region は拠点の名前、concurrency は同時に確かめる数.
 * probePolicy は監視でつないでよい宛先（既定は外のアドレスだけ. PROBE_ALLOW_INTERNAL）.
 */
final case class WorkerConfig(node: String,
                              publicUrl: String,
                              version: String,
                              concurrency: Int = 16,
                              region: String = "default",
                              probePolicy: sbirds.net.NetGuard.Policy = sbirds.net.NetGuard.Policy.publicOnly)

/**
 * 監視を回す（どのノードでも同じものを動かす. 同じ拠点のノードは DB で取り合うので、台数を増やすと分担する. 拠点ごとに全ての監視を確かめる）.
 *  - 確認: 1秒ごとに、空いている数だけ自分の拠点で時刻の来た監視を取り、確かめて結果を残す
 *  - 判定: 15秒ごとに、拠点の健全性を判定し直す（1つのノードだけが行う）
 *  - 通知: 2秒ごとに、行列から取って送る（失敗したら間隔を延ばして送り直す）
 *  - 心拍: 30秒ごとにノードの生存を書く
 *  - 掃除: 1時間ごとに古い結果を消す
 */
final class Worker(monitors: MonitorStore,
                   notifiers: NotifierStore,
                   nodes: NodeStore,
                   senders: Senders,
                   announcer: Announcer,
                   cfg: WorkerConfig) {

  /** d ごとに繰り返す. 失敗しても止めず、code を付けてログに出す. */
  private def every(d: FiniteDuration, code: ErrorCode)(io: IO[Unit]): IO[Nothing] =
    (io.handleErrorWith(e => Log.report(code, e.toString, e)) *> IO.sleep(d)).foreverM

  def run: IO[Unit] =
    for {
      started <- IO.realTimeInstant
      sem <- Semaphore[IO](cfg.concurrency.toLong)
      _ <- (
        // 心拍は30秒ごと. 拠点で確かめる監視の行は、起動直後と10分ごとに補う
        Ref[IO].of(0L).flatMap(n =>
          every(30.seconds, ErrorCode.NodeHeartbeatFailed)(n.getAndUpdate(_ + 1).flatMap(i =>
            nodes.beat(cfg.node, cfg.region, cfg.version, started, fill = i % 20 == 0)
          ))
        ),
        every(1.second, ErrorCode.DueMonitorClaimFailed)(checkTick(sem)),
        IO.sleep(5.seconds) *> every(15.seconds, ErrorCode.RegionJudgeFailed)(judgeTick),
        every(2.seconds, ErrorCode.NotifyQueueFailed)(notifyTick),
        IO.sleep(1.minute) *>
          every(1.hour, ErrorCode.HistoryPurgeFailed)((monitors.purge, notifiers.purge, nodes.purge).tupled.void)
      ).parTupled
    } yield ()

  private def checkTick(sem: Semaphore[IO]): IO[Unit] =
    sem.available.flatMap { free =>
      if free <= 0 then IO.unit
      else
        monitors.claimDue(cfg.region, free.toInt).flatMap(_.traverse_(m =>
          sem.acquire *> check(m).guarantee(sem.release).start.void
        ))
    }

  def check(m: DueMonitor): IO[Unit] =
    Telemetry.span("monitor.check", "monitor.kind" -> m.kind, "monitor.name" -> m.name, "region" -> m.region) {
      for {
        r <- Probe.run(m.kind, m.config, m.timeoutMs.millis, cfg.probePolicy)
        at <- IO.realTimeInstant
        _ <- Metrics.count(
          "skotare.checks",
          "kind" -> m.kind,
          "result" -> (if r.ok then "ok" else "fail"),
          "region" -> m.region
        )
        _ <- r.latencyMs.traverse_(ms =>
          Metrics.record("skotare.check.latency", ms.toDouble, "ms", "kind" -> m.kind, "region" -> m.region)
        )
        t <- monitors.record(m.id, m.region, r, cfg.node, at, cfg.publicUrl)
        _ <- t.traverse_(announcer.transition)
      } yield ()
    }.handleErrorWith(e => Log.report(ErrorCode.MonitorCheckFailed, s"${m.name}: $e", e))

  /** 拠点の健全性を判定し直す（15秒ごと. テストからも呼ぶ）. */
  def judgeTick: IO[Unit] =
    IO.realTimeInstant.flatMap(at => monitors.judgeRegions(at, cfg.publicUrl)).flatMap {
      (rs, ts) =>
        rs.traverse_(announcer.region) *> ts.traverse_(announcer.transition)
    }

  /** 行列から取って送る（2秒ごと. テストからも呼ぶ）. */
  def notifyTick: IO[Unit] =
    notifiers.claim(20).flatMap(_.traverse_ { item =>
      senders.send(item.notifier, item.payload).attempt.flatMap {
        case Right(_) => notifiers.sent(item.id)
        case Left(e) =>
          Log.report(ErrorCode.NotificationSendFailed, s"${item.notifier.name} (${item.attempts}): ${e.getMessage}") *>
            Metrics.count("skotare.notifications.failed", "kind" -> item.notifier.kind) *>
            notifiers.failed(item.id, item.attempts, Senders.describe(e))
      }
    })
}
