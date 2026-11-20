package skotare.db

import sbirds.db.*
import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import io.circe.Json
import io.circe.syntax.*
import skotare.shared.*

import java.util.UUID

/** 通知先（secret は暗号化した秘密の値）. */
final case class NotifierRow(id: UUID,
                             name: String,
                             kind: String,
                             config: NotifierConfig,
                             secret: String,
                             allMonitors: Boolean,
                             monitorIds: List[UUID],
                             enabled: Boolean) {
  def dto: NotifierDto = NotifierDto(id, name, kind, config, secret.nonEmpty, allMonitors, monitorIds, enabled)
}

/** 送る通知（行列から取ったもの）. */
final case class OutboxItem(id: UUID, notifier: NotifierRow, payload: Json, attempts: Int)

/** 通知先と、送る通知の行列. */
final class NotifierStore(xa: Transactor[IO]) extends TxStore(xa) {

  private val cols =
    fr"""n.id, n.name, n.kind, n.config, n.secret, n.all_monitors,
    coalesce((select array_agg(x.monitor_id) from notifier_monitor x where x.notifier_id = n.id), '{}'), n.enabled from notifier n"""

  private def row(t: (UUID, String, String, Json, String, Boolean, Array[UUID], Boolean)): NotifierRow =
    NotifierRow(t._1, t._2, t._3, t._4.as[NotifierConfig].getOrElse(NotifierConfig()), t._5, t._6, t._7.toList, t._8)

  private type Raw = (UUID, String, String, Json, String, Boolean, Array[UUID], Boolean)

  def list: IO[List[NotifierRow]] =
    tx((fr"select" ++ cols ++ fr"order by lower(n.name)").query[Raw].to[List].map(_.map(row)))

  def find(id: UUID): IO[Option[NotifierRow]] = tx(findC(id))
  private def findC(id: UUID) = (fr"select" ++ cols ++ fr"where n.id = $id").query[Raw].option.map(_.map(row))

  /** 作る・変える（secret が None なら今の秘密の値のまま）. */
  def save(id: Option[UUID], r: NotifierReq, secret: Option[String]): IO[Option[NotifierRow]] = tx {
    val nid = id.getOrElse(UUID.randomUUID())
    val write = id match {
      case None =>
        sql"""insert into notifier (id, name, kind, config, secret, all_monitors, enabled)
              values ($nid, ${r.name}, ${r.kind}, ${r.config.asJson}, ${secret.getOrElse(
            ""
          )}, ${r.allMonitors}, ${r.enabled})""".update.run
      case Some(_) =>
        (fr"update notifier set name = ${r.name}, kind = ${r.kind}, config = ${r.config.asJson}, all_monitors = ${r.allMonitors}, enabled = ${r.enabled}" ++
          secret.fold(fr"")(s => fr", secret = $s") ++ fr"where id = $nid").update.run
    }
    write.flatMap {
      case 0 => none[NotifierRow].pure[ConnectionIO]
      case _ =>
        sql"delete from notifier_monitor where notifier_id = $nid".update.run *>
          // 選んだ監視は1つの文でまとめて入れる（消えた監視の ID は入らない）
          (if r.monitorIds.isEmpty then 0.pure[ConnectionIO]
           else
             sql"insert into notifier_monitor (notifier_id, monitor_id) select $nid, id from monitor where id = any(${r.monitorIds.distinct.toArray})".update.run) *>
          findC(nid)
    }
  }

  def delete(id: UUID): IO[Boolean] = tx(sql"delete from notifier where id = $id".update.run.map(_ > 0))

  // ---------------------------------------------------------------- 行列（複数ノードで取り合う）

  /**
   * 送る時刻が来たものを取る. 取ったものは5分先に延ばしておく（送っている間にノードが落ちたら、5分後にほかのノードが送り直す）.
   */
  def claim(n: Int): IO[List[OutboxItem]] = tx(
    for {
      // 無効にした通知先の分は送らずに閉じる
      _ <- sql"""update notification_outbox o set sent_at = now(), last_error = '通知先を無効にしたため送りませんでした'
                 from notifier n where n.id = o.notifier_id and not n.enabled and o.sent_at is null""".update.run
      xs <-
        sql"""with c as (
                    update notification_outbox o set next_at = now() + interval '5 minutes', attempts = o.attempts + 1
                    where o.id in (select id from notification_outbox where sent_at is null and next_at <= now() order by next_at limit $n for update skip locked)
                    returning o.id, o.notifier_id, o.payload, o.attempts)
                  select c.id, c.payload, c.attempts, n.id, n.name, n.kind, n.config, n.secret, n.all_monitors, n.enabled
                  from c join notifier n on n.id = c.notifier_id"""
          .query[(UUID, Json, Int, UUID, String, String, Json, String, Boolean, Boolean)].to[List]
    } yield xs.map((id, p, a, nid, name, kind, cfg, secret, all, en) =>
      // 送るのに監視の選択（monitorIds）は要らない
      OutboxItem(
        id,
        NotifierRow(nid, name, kind, cfg.as[NotifierConfig].getOrElse(NotifierConfig()), secret, all, Nil, en),
        p,
        a
      )
    )
  )

  def sent(id: UUID): IO[Unit] =
    tx(sql"update notification_outbox set sent_at = now(), last_error = null where id = $id".update.run.void)

  /** 送れなかった. 間隔を延ばして送り直す（1・2・4…分. 最大10回で諦める）. */
  def failed(id: UUID, attempts: Int, error: String): IO[Unit] = tx {
    val give = attempts >= 10
    val wait = math.min(60, 1 << math.min(attempts - 1, 6))
    (if give then
       sql"update notification_outbox set sent_at = now(), last_error = ${"諦めました: " + error.take(500)} where id = $id"
     else
       sql"update notification_outbox set next_at = now() + make_interval(mins => $wait), last_error = ${error.take(500)} where id = $id"
    ).update.run.void
  }

  def purge: IO[Int] = tx(sql"delete from notification_outbox where sent_at < now() - interval '30 days'".update.run)
}
