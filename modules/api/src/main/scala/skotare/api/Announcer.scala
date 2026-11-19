package skotare.api

import cats.effect.*
import cats.syntax.all.*
import skotare.db.*
import sbirds.observe.{Log, Metrics}
import sbirds.push.{PushMessage, PushService}
import skotare.shared.*

/**
 * 状態の変化をその場で知らせる: ログ・メトリクス・プッシュ通知（通知先への送信は行列から）.
 * 確認（Worker）・拠点の判定・監視の変更（ルート）のどれで変わっても、同じように知らせる.
 */
final class Announcer(push: Option[(UserStore, PushService)], publicUrl: String) {

  private def pushAll(msg: PushMessage): IO[Unit] =
    push.traverse_((users, svc) => users.pushSubscribers.flatMap(_.traverse_(u => svc.send(u, msg))).start.void)

  def transition(t: Transition): IO[Unit] = {
    val msg = PushMessage(
      Messages.subject(t.payload(publicUrl)),
      s"${t.target}\n${t.message}".take(300),
      s"/monitors/${t.monitorId}",
      Some(s"monitor-${t.monitorId}")
    )
    val line = s"[skotare] ${t.event.toUpperCase} ${t.name} (${t.target}): ${t.message}"
    (if t.down then Log.warn(line) else Log.info(line)) *>
      Metrics.count("skotare.monitor.transitions", "to" -> t.to, "kind" -> t.kind) *> pushAll(msg)
  }

  def region(c: RegionChange): IO[Unit] =
    (if c.to == RegionHealth.Ok then Log.info(s"[skotare] REGION ${c.region} ${c.from} -> ${c.to}")
     else Log.warn(s"[skotare] REGION ${c.region} ${c.from} -> ${c.to}: ${c.reason}")) *>
      Metrics.count("skotare.region.transitions", "to" -> c.to, "region" -> c.region) *>
      pushAll(PushMessage(Messages.subject(c.payload(publicUrl)), c.reason.take(300), "/", Some(s"region-${c.region}")))
}
