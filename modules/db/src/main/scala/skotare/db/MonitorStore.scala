package skotare.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import io.circe.Json
import io.circe.syntax.*
import skotare.core.{Judge, ProbeResult}
import skotare.shared.*

import java.time.{Instant, LocalDate, ZoneOffset}
import java.util.UUID

/** 確かめる時刻が来た監視（ノードが自分の拠点の分として取ったもの）. */
final case class DueMonitor(id: UUID, name: String, kind: String, config: MonitorConfig, timeoutMs: Int, region: String)

/** 拠点の健全性が変わったこと（通知・ログに使う）. */
final case class RegionChange(region: String, from: String, to: String, reason: String, at: Instant):
  def payload(publicUrl: String): Json = Json.obj(
    "event" -> s"region.$to".asJson,
    "region" -> Json.obj("name" -> region.asJson, "health" -> to.asJson, "from" -> from.asJson),
    "message" -> reason.asJson,
    "at" -> at.toString.asJson,
    "url" -> publicUrl.asJson
  )

/**
 * 状態が変わったこと（通知・プッシュ・メトリクスに使う）. event は、停止 down・復旧 up・
 * 停止中に設定を変えて障害を閉じた closed（to は未確認 pending か一時停止 paused）.
 */
final case class Transition(monitorId: UUID, name: String, kind: String, target: String, from: String, to: String, message: String,
    at: Instant, downSince: Option[Instant]):
  def down: Boolean = to == MonStatus.Down
  def event: String = to match
    case MonStatus.Down => "down"
    case MonStatus.Up   => "up"
    case _              => "closed"

  /** 通知の本文（stui・skereru・Webhook で共通）. */
  def payload(publicUrl: String): Json = Json.obj(
    "event" -> event.asJson,
    "status" -> to.asJson,
    "monitor" -> Json.obj("id" -> monitorId.asJson, "name" -> name.asJson, "kind" -> kind.asJson, "target" -> target.asJson),
    "message" -> message.asJson,
    "at" -> at.toString.asJson,
    "downSince" -> downSince.map(_.toString).asJson,
    "url" -> s"$publicUrl/monitors/$monitorId".asJson
  )

private final case class MonitorRow(id: UUID, name: String, kind: String, config: Json, intervalSec: Int, timeoutMs: Int, retries: Int,
    enabled: Boolean, description: String, status: String, statusSince: Instant, lastCheckedAt: Option[Instant], lastLatencyMs: Option[Int],
    lastMessage: Option[String], uptime24h: Option[Double], uptime30d: Option[Double], recent: String, canary: Boolean, confirmRegions: Int):
  def dto(regions: List[RegionStateDto]): MonitorDto =
    val cfg = config.as[MonitorConfig].getOrElse(MonitorConfig())
    MonitorDto(id, name, kind, cfg, cfg.target(kind), intervalSec, timeoutMs, retries, enabled, description, status, statusSince.toString,
      lastCheckedAt.map(_.toString), lastLatencyMs, lastMessage, uptime24h, uptime30d, recent.toList.map(_ == '1'), canary, confirmRegions, regions)

/**
 * 監視・確認の結果・障害.
 * 監視は拠点ごとに確かめ（monitor_region）、健全な拠点の票を合わせて監視の状態（monitor.status）を決める（[[skotare.core.Judge]]）.
 */
final class MonitorStore(xa: Transactor[IO]) extends TxStore(xa):

  /** 確認の結果を残す日数（日ごとの集計 check_daily は DailyKeepDays 日）. */
  val KeepDays = 30
  val DailyKeepDays = 400
  /** ステータスページの日ごとの帯の日数. */
  val Days = 90

  private val today = fr"(now() at time zone 'UTC')::date"

  private val cols: Fragment =
    fr"m.id, m.name, m.kind, m.config, m.interval_sec, m.timeout_ms, m.retries, m.enabled, m.description, m.status, m.status_since," ++
      fr"m.last_checked_at, m.last_latency_ms, m.last_message," ++
      // 24時間は確認の結果から、30日は日ごとの集計から
      fr"""(select round(avg(case when r.up then 100.0 else 0 end), 3)::float8 from check_result r
            where r.monitor_id = m.id and r.up is not null and r.checked_at > now() - interval '24 hours'),""" ++
      fr"(select round(100.0 * sum(d.up_n) / nullif(sum(d.total_n), 0), 3)::float8 from check_daily d where d.monitor_id = m.id and d.day > " ++
      today ++ fr"- 30)," ++
      // 最近の帯: 確認の間隔ごとに区切った30区間（拠点の数や、早めた確認で行の数が偏らないように）
      fr"""coalesce((select string_agg(case when b.up then '1' else '0' end, '' order by b.t) from (
              select date_bin(make_interval(secs => m.interval_sec), r.checked_at, timestamptz '2000-01-01') t, bool_and(r.up) up
              from check_result r where r.monitor_id = m.id and r.up is not null and r.checked_at > now() - make_interval(secs => m.interval_sec * 31)
              group by 1 order by 1 desc limit 30) b), '')""" ++
      fr", m.canary, m.confirm_regions"

  /** 拠点ごとの状態（ids を付けるとその監視だけ）. */
  private def regionStatesC(ids: Option[List[UUID]]): ConnectionIO[Map[UUID, List[RegionStateDto]]] =
    (fr"""select mr.monitor_id, mr.region, mr.status, mr.status_since, mr.last_checked_at, mr.last_latency_ms, mr.last_message, r.health = 'ok'
          from monitor_region mr join region r on r.name = mr.region""" ++ Fragments.whereAndOpt(ids.map(xs => fr"mr.monitor_id = any(${xs.toArray})")) ++
      fr"order by mr.region")
      .query[(UUID, String, String, Instant, Option[Instant], Option[Int], Option[String], Boolean)].to[List]
      .map(_.groupMap(_._1)((_, r, st, since, at, l, msg, c) => RegionStateDto(r, st, since.toString, at.map(_.toString), l, msg, c)))

  /** 監視の一覧（ids を付けるとその監視だけ）. */
  private def listC(ids: Option[List[UUID]]): ConnectionIO[List[MonitorDto]] =
    for
      rows <- (fr"select" ++ cols ++ fr"from monitor m" ++ Fragments.whereAndOpt(ids.map(xs => fr"m.id = any(${xs.toArray})")) ++
                fr"order by lower(m.name), m.id").query[MonitorRow].to[List]
      rs   <- if rows.isEmpty then Map.empty[UUID, List[RegionStateDto]].pure[ConnectionIO] else regionStatesC(ids)
    yield rows.map(r => r.dto(rs.getOrElse(r.id, Nil)))

  def list: IO[List[MonitorDto]] = tx(listC(None))

  /** 指定した監視だけ（ステータスページ）. */
  def listOf(ids: List[UUID]): IO[List[MonitorDto]] = if ids.isEmpty then IO.pure(Nil) else tx(listC(Some(ids)))

  /** 名前と状態だけ（ステータスページ. 稼働率などの重い集計をしない）. */
  def statuses(ids: List[UUID]): IO[Map[UUID, (String, String)]] =
    if ids.isEmpty then IO.pure(Map.empty)
    else tx(sql"select id, name, status from monitor where id = any(${ids.toArray})".query[(UUID, String, String)].to[List]
      .map(_.map((id, n, s) => id -> (n, s)).toMap))

  def find(id: UUID): IO[Option[MonitorDto]] = tx(findC(id))

  private def findC(id: UUID): ConnectionIO[Option[MonitorDto]] = listC(Some(List(id))).map(_.headOption)

  def create(r: MonitorReq, by: UUID): IO[MonitorDto] = tx {
    val id = UUID.randomUUID()
    val st = if r.enabled then MonStatus.Pending else MonStatus.Paused
    sql"""insert into monitor (id, name, kind, config, interval_sec, timeout_ms, retries, enabled, description, status, created_by, canary, confirm_regions)
          values ($id, ${r.name}, ${r.kind}, ${r.config.asJson}, ${r.intervalSec}, ${r.timeoutMs}, ${r.retries}, ${r.enabled}, ${r.description}, $st, $by,
                  ${r.canary}, ${r.confirmRegions})"""
      .update.run *>
      sql"insert into monitor_region (monitor_id, region) select $id, name from region".update.run *> findC(id).map(_.get)
  }

  /**
   * 変える. 種類・設定・有効かを変えたら、状態を確かめ直す（未確認に戻してすぐ確かめる）.
   * 停止中に変えたら障害を閉じ、「設定の変更で終えた」ことを知らせる（停止の通知だけが残らないように）.
   * 間隔を短くしたら、次の確認を新しい間隔までに早める.
   */
  def update(id: UUID, r: MonitorReq, publicUrl: String): IO[Option[(MonitorDto, Option[Transition])]] = tx {
    sql"select name, kind, config, enabled, status, interval_sec from monitor where id = $id for no key update"
      .query[(String, String, Json, Boolean, String, Int)].option.flatMap {
        case None => none[(MonitorDto, Option[Transition])].pure[ConnectionIO]
        case Some((name, kind, cfg, enabled, status, interval)) =>
          val changed = kind != r.kind || cfg.as[MonitorConfig].toOption != Some(r.config) || enabled != r.enabled
          val to = if r.enabled then MonStatus.Pending else MonStatus.Paused
          // 確かめ直すときは、前の結果（停止の原因など）も消す
          val reset = if !changed then fr"" else fr", status = $to, status_since = now(), last_message = null, last_checked_at = null, last_latency_ms = null"
          val now = Instant.now()
          val regions =
            if changed then sql"update monitor_region set status = 'pending', status_since = now(), fail_streak = 0, next_check_at = now() where monitor_id = $id".update.run
            else if r.intervalSec < interval then
              sql"update monitor_region set next_check_at = least(next_check_at, now() + make_interval(secs => ${r.intervalSec})) where monitor_id = $id".update.run
            else 0.pure[ConnectionIO]
          val close =
            if !changed then none[Transition].pure[ConnectionIO]
            else closeIncidentC(id, now).flatMap {
              case Some(since) if status == MonStatus.Down =>
                val msg = if r.enabled then "設定を変えたため、障害を閉じました（確かめ直します）" else "一時停止したため、障害を閉じました"
                val t = Transition(id, name, kind, MonitorConfig.targetOf(cfg, kind), status, to, msg, now, Some(since))
                enqueue(t, publicUrl).as(Some(t))
              case _ => none[Transition].pure[ConnectionIO]
            }
          (fr"""update monitor set name = ${r.name}, kind = ${r.kind}, config = ${r.config.asJson}, interval_sec = ${r.intervalSec},
                timeout_ms = ${r.timeoutMs}, retries = ${r.retries}, enabled = ${r.enabled}, description = ${r.description},
                canary = ${r.canary}, confirm_regions = ${r.confirmRegions}, updated_at = now()""" ++
            reset ++ fr"where id = $id").update.run *> regions *> (close, findC(id)).mapN((t, m) => m.map(_ -> t))
      }
  }

  def delete(id: UUID): IO[Boolean] = tx(sql"delete from monitor where id = $id".update.run.map(_ > 0))

  /** 次の確認をすぐにする（今すぐ確かめる）. */
  def checkSoon(id: UUID): IO[Boolean] = tx(
    sql"update monitor_region set next_check_at = now() where monitor_id = $id and exists (select 1 from monitor where id = $id and enabled)".update.run.map(_ > 0))

  // ---------------------------------------------------------------- 確認（拠点ごと. 同じ拠点のノードで取り合う）

  /**
   * この拠点で確かめる時刻が来たものを n 件取り、次の時刻を進める. 「for update skip locked」なので、同じ拠点のほかのノードが取ったものは飛ばす
   * （同じ監視を同じ拠点の2台で同時に確かめない. ノードが止まっても、次の時刻には同じ拠点のほかのノードが取る）.
   */
  def claimDue(region: String, n: Int): IO[List[DueMonitor]] = tx(
    sql"""update monitor_region mr set next_check_at = now() + make_interval(secs => m.interval_sec)
          from monitor m
          where m.id = mr.monitor_id and mr.region = $region and mr.monitor_id in (
            select x.monitor_id from monitor_region x join monitor y on y.id = x.monitor_id
            where x.region = $region and y.enabled and x.next_check_at <= now()
            order by x.next_check_at limit $n for update of x skip locked)
          returning m.id, m.name, m.kind, m.config, m.timeout_ms"""
      .query[(UUID, String, String, Json, Int)].to[List]
      .map(_.map((id, name, k, c, t) => DueMonitor(id, name, k, c.as[MonitorConfig].getOrElse(MonitorConfig()), t, region)))
  )

  /** 判定に使う監視の行. */
  private final case class MonRow(name: String, kind: String, config: Json, status: String, enabled: Boolean, confirm: Int, retries: Int)
  /**
   * ある拠点から見た状態. ok はその拠点が健全か、stale は確かめる時刻を大きく過ぎているか
   * （心拍はあるのに確認が回っていない拠点の古い票で、判定を止めたり誤らせたりしないため、票に数えない）.
   */
  private final case class RegionRow(region: String, status: String, streak: Int, message: String, ok: Boolean, stale: Boolean):
    def votes: Boolean = ok && !stale

  private val stale: Fragment = Sql.stale

  /** 監視の行をロックして読む（for no key update: 結果・障害の挿入（外部キー）は止めない. 拠点どうしの結果は1つずつ順に合わせる）. */
  private def lockMonitorC(id: UUID): ConnectionIO[Option[MonRow]] =
    sql"select name, kind, config, status, enabled, confirm_regions, retries from monitor where id = $id for no key update".query[MonRow].option

  private def regionRowsC(id: UUID): ConnectionIO[List[RegionRow]] =
    (fr"select mr.region, mr.status, mr.fail_streak, coalesce(mr.last_message, ''), r.health = 'ok'," ++ stale ++
      fr"from monitor_region mr join region r on r.name = mr.region join monitor m on m.id = mr.monitor_id where mr.monitor_id = $id order by mr.region")
      .query[RegionRow].to[List]

  /**
   * 拠点の結果を残し、その拠点から見た状態を進める（失敗が retries 回を超えて続いたら停止、成功したら正常.
   * 停止と判断する前の失敗では、次の確認を早める（20秒後））. そのうえで、健全な拠点の票で監視の状態を決め直す.
   */
  def record(id: UUID, region: String, r: ProbeResult, node: String, at: Instant, publicUrl: String): IO[Option[Transition]] = tx {
    lockMonitorC(id).flatMap {
      case Some(m) if m.enabled =>
        regionRowsC(id).flatMap { rows =>
          rows.find(_.region == region) match
            case None => none[Transition].pure[ConnectionIO] // 拠点の行がない（拠点を片づけた）
            case Some(cur) =>
              val streak = if r.ok then 0 else cur.streak + 1
              val next = if r.ok then MonStatus.Up else if streak > m.retries then MonStatus.Down else cur.status
              val since = if next != cur.status then fr", status = $next, status_since = $at" else fr""
              val sooner = if !r.ok && next != MonStatus.Down then fr", next_check_at = least(next_check_at, now() + interval '20 seconds')" else fr""
              val msg = r.message.take(500)
              val updated = rows.map(x => if x.region == region then x.copy(status = next, streak = streak, message = msg, stale = false) else x)
              // 一覧に出す最後の結果と稼働率の標本は、判定に数える拠点のものだけ（疑わしい拠点の失敗・早めた確認で偏らせない）
              val counted = cur.ok || !rows.exists(_.votes)
              for
                _  <- (fr"update monitor_region set fail_streak = $streak, last_checked_at = $at, last_latency_ms = ${r.latencyMs}, last_message = $msg" ++
                        since ++ sooner ++ fr"where monitor_id = $id and region = $region").update.run
                tv <- decideC(id, m, updated, at, publicUrl, Option.when(counted)(r.latencyMs -> msg))
                _  <- sql"""insert into check_result (monitor_id, checked_at, ok, latency_ms, message, node, region, up)
                            values ($id, $at, ${r.ok}, ${r.latencyMs}, $msg, $node, $region, ${tv._2.filter(_ => counted)})""".update.run
                _  <- tv._2.filter(_ => counted).traverse_(up =>
                        sql"""insert into check_daily (monitor_id, day, up_n, total_n) values ($id, (${at}::timestamptz at time zone 'UTC')::date, ${if up then 1 else 0}, 1)
                              on conflict (monitor_id, day) do update set up_n = check_daily.up_n + excluded.up_n, total_n = check_daily.total_n + 1""".update.run)
              yield tv._1
        }
      case _ => none[Transition].pure[ConnectionIO] // 確かめている間に止められた・消された
    }
  }

  private def verdictOf(s: String): Option[Boolean] = Option.when(s == MonStatus.Up || s == MonStatus.Down)(s == MonStatus.Up)

  /**
   * 健全な拠点の、古くない票で監視の状態を決め、変わったら障害を開く・閉じ、通知の行列に入れる. 呼ぶ側で monitor の行をロックしておく.
   * 票で決まらなければ、状態は変えない（保留）. 停止の間は、原因（停止と見ている拠点とその理由）を最後の結果として出し続ける.
   * last は一覧に出す最後の結果（応答時間, 説明）. 返すのは (状態の変化, 稼働率に数える判定（正常 true・停止 false. 未確認は None）).
   */
  private def decideC(id: UUID, m: MonRow, rs: List[RegionRow], at: Instant, publicUrl: String,
      last: Option[(Option[Int], String)]): ConnectionIO[(Option[Transition], Option[Boolean])] =
    val votes = rs.filter(_.votes)
    val next = Judge.overall(votes.map(_.status), m.confirm).getOrElse(m.status)
    val downs = votes.filter(_.status == MonStatus.Down)
    // 拠点が複数あるときは、どの拠点から届かないかを添える
    val cause = (if rs.size > 1 then downs.map(x => s"${x.region}: ${x.message}") else downs.map(_.message)).mkString(" / ").take(500)
    val changed = next != m.status
    // 停止の間は原因を出し続ける（停止の票がなくなって保留している間も、成功の文面で上書きしない）
    val upMsg = votes.find(_.status == MonStatus.Up).fold("")(_.message)
    // 復旧したときは、停止の原因を残さない（拠点の判定で変わったときも）
    val lastMsg =
      if next == MonStatus.Down then Option.when(cause.nonEmpty)(cause)
      else if changed && next == MonStatus.Up then Some(last.fold(upMsg)(_._2))
      else last.map(_._2)
    val sets = List(
      last.map(l => fr"last_checked_at = $at, last_latency_ms = ${l._1}"),
      lastMsg.map(x => fr"last_message = $x"),
      Option.when(changed)(fr"status = $next, status_since = $at")
    ).flatten
    val target = MonitorConfig.targetOf(m.config, m.kind)
    for
      _ <- (if sets.isEmpty then 0.pure[ConnectionIO] else (fr"update monitor" ++ Sql.set(sets) ++ fr"where id = $id").update.run)
      downSince <- (if changed && next == MonStatus.Up then closeIncidentC(id, at) else none[Instant].pure[ConnectionIO])
      _ <- (if changed && next == MonStatus.Down then
              sql"insert into incident (id, monitor_id, started_at, cause) values (${UUID.randomUUID()}, $id, $at, $cause) on conflict do nothing".update.run
            else 0.pure[ConnectionIO])
      // 初めて確かめて正常だったとき（未確認 → 正常）は知らせない
      t = Option.when(changed && (next == MonStatus.Down || downSince.isDefined))(
            Transition(id, m.name, m.kind, target, m.status, next, if next == MonStatus.Down then cause else upMsg, at, downSince))
      _ <- t.traverse_(tr => enqueue(tr, publicUrl))
    yield (t, verdictOf(next))

  /** 拠点の判定を1つのノードだけが行うための advisory lock の番号. */
  private val JudgeLock = 0x736b6f74617265L

  /**
   * 拠点の健全性を判定し直す（どれか1つのノードが行う. ほかのノードが判定中なら何もしない）.
   * 健全性が変わったら、拠点の変化を通知の行列に入れ、状態が変わりうる監視だけをロックして判定し直す
   * （疑わしくなった拠点の票を外す・健全に戻った拠点の票を戻す）. 止まった拠点の状態は未確認に戻す（戻ったときに古い票で判定しない）.
   */
  def judgeRegions(at: Instant, publicUrl: String): IO[(List[RegionChange], List[Transition])] = tx {
    sql"select pg_try_advisory_xact_lock($JudgeLock)".query[Boolean].unique.flatMap {
      case false => (List.empty[RegionChange], List.empty[Transition]).pure[ConnectionIO]
      case true =>
        for
          regions <- (fr"select r.name, r.health, r.reason," ++ Sql.aliveNodes(fr"r.name") ++ fr"from region r order by r.name")
                       .query[(String, String, String, Int)].to[List]
          states  <- (fr"select mr.monitor_id, mr.region, mr.status, m.canary, m.status, m.confirm_regions," ++ stale ++
                       fr"from monitor_region mr join monitor m on m.id = mr.monitor_id where m.enabled")
                       .query[(UUID, String, String, Boolean, String, Int, Boolean)].to[List]
          judged   = Judge.regions(regions.map(r => r._1 -> r._4), states.map(s => Judge.Vote(s._1, s._2, s._3, s._4, s._7)))
          both     = regions.zip(judged)
          changes  = both.collect { case ((n, from, _, _), (to, reason)) if from != to => RegionChange(n, from, to, reason, at) }
          _ <- both.traverse_ { case ((n, from, oldReason, _), (to, reason)) =>
                 if from != to then sql"update region set health = $to, reason = $reason, since = $at where name = $n".update.run
                 else if oldReason != reason then sql"update region set reason = $reason where name = $n".update.run
                 else 0.pure[ConnectionIO]
               }
          _ <- changes.traverse_(c => enqueuePayload(c.payload(publicUrl), None))
          ok = both.collect { case ((n, _, _, _), (RegionHealth.Ok, _)) => n }.toSet
          // 状態が変わりうる監視（新しい票の集まりで、今と違う判定になるもの）だけを、id の順にロックして決め直す
          candidates = if changes.isEmpty then Nil else states.groupBy(_._1).toList.collect {
                         case (id, xs) if Judge.overall(xs.filter(x => ok(x._2) && !x._7).map(_._3), xs.head._6).exists(_ != xs.head._5) => id
                       }.sorted
          ts <- candidates.traverse(id => lockMonitorC(id).flatMap {
                  case Some(m) if m.enabled => regionRowsC(id).flatMap(rs => decideC(id, m, rs, at, publicUrl, None)).map(_._1)
                  case _ => none[Transition].pure[ConnectionIO]
                }).map(_.flatten)
          // 止まった拠点の状態は未確認に戻す（戻ったときに古い票で判定しない）. 監視→拠点の行の順にロックするよう、判定の後に行う
          _ <- changes.filter(_.to == RegionHealth.Offline).traverse_(c =>
                 sql"update monitor_region set status = 'pending', status_since = $at, fail_streak = 0 where region = ${c.region}".update.run)
        yield (changes, ts)
    }
  }

  /** 続いている障害を閉じ、始まった時刻を返す. */
  private def closeIncidentC(id: UUID, at: Instant): ConnectionIO[Option[Instant]] =
    sql"update incident set resolved_at = $at where monitor_id = $id and resolved_at is null returning started_at".query[Instant].option

  /** 状態の変化を、この監視を送る通知先ごとに行列へ入れる. */
  private def enqueue(t: Transition, publicUrl: String): ConnectionIO[Unit] = enqueuePayload(t.payload(publicUrl), Some(t.monitorId))

  /** monitor が None（拠点の変化）なら、すべての監視を送る通知先にだけ入れる. */
  private def enqueuePayload(payload: Json, monitor: Option[UUID]): ConnectionIO[Unit] =
    (fr"insert into notification_outbox (id, notifier_id, payload) select gen_random_uuid(), n.id, $payload from notifier n where n.enabled and (n.all_monitors" ++
      monitor.fold(fr"")(id => fr"or exists (select 1 from notifier_monitor x where x.notifier_id = n.id and x.monitor_id = $id)") ++ fr")").update.run.void

  // ---------------------------------------------------------------- 見る

  def checks(id: UUID, limit: Int): IO[List[CheckDto]] = tx(
    sql"select checked_at, ok, latency_ms, message, node, region from check_result where monitor_id = $id order by checked_at desc limit $limit"
      .query[(Instant, Boolean, Option[Int], String, String, String)].to[List].map(_.map((a, o, l, m, n, r) => CheckDto(a.toString, o, l, m, n, r)))
  )

  private val incidentCols = fr"i.id, i.monitor_id, m.name, i.started_at, i.resolved_at, i.cause from incident i join monitor m on m.id = i.monitor_id"
  private def incidentDto(t: (UUID, UUID, String, Instant, Option[Instant], String)) =
    IncidentDto(t._1, t._2, t._3, t._4.toString, t._5.map(_.toString), t._6)

  /** 障害の一覧（新しい順）. monitor を付けるとその監視だけ、openOnly で続いているものだけ. */
  def incidents(monitor: Option[UUID], openOnly: Boolean, limit: Int): IO[List[IncidentDto]] =
    incidentsWhere(List(monitor.map(id => fr"i.monitor_id = $id"), Option.when(openOnly)(fr"i.resolved_at is null")).flatten, limit)

  /** 指定した監視の、since より後に終わったか続いている障害（ステータスページ）. */
  def incidentsOf(ids: List[UUID], since: Instant, limit: Int): IO[List[IncidentDto]] =
    if ids.isEmpty then IO.pure(Nil)
    else incidentsWhere(List(fr"i.monitor_id = any(${ids.toArray})", fr"coalesce(i.resolved_at, 'infinity'::timestamptz) > $since"), limit)

  private def incidentsWhere(conds: List[Fragment], limit: Int): IO[List[IncidentDto]] = tx(
    (fr"select" ++ incidentCols ++ (if conds.isEmpty then fr"" else fr"where" ++ conds.reduce(_ ++ fr"and" ++ _)) ++
      fr"order by i.started_at desc limit $limit").query[(UUID, UUID, String, Instant, Option[Instant], String)].to[List].map(_.map(incidentDto))
  )

  /** 日ごとの稼働率（今日まで days 日. 確認のない日は None）. */
  def days(ids: List[UUID], days: Int, today: LocalDate): IO[Map[UUID, List[DayBar]]] =
    if ids.isEmpty then IO.pure(Map.empty)
    else tx(
      sql"""select monitor_id, day, 100.0 * up_n / total_n from check_daily
            where monitor_id = any(${ids.toArray}) and total_n > 0 and day >= ${today.minusDays(days - 1L)}""".query[(UUID, LocalDate, Double)].to[List]
    ).map { rows =>
      val by = rows.groupBy(_._1).view.mapValues(_.map(r => r._2 -> r._3).toMap).toMap
      ids.map(id => id -> (0 until days).toList.reverse.map(i => today.minusDays(i.toLong)).map(d =>
        DayBar(d.toString, by.get(id).flatMap(_.get(d)).map(v => math.round(v * 100) / 100.0)))).toMap
    }

  /** 日ごとの集計から、今日まで days 日の稼働率（%. 判定のある確認の数で重み付け）. */
  def uptimeOver(ids: List[UUID], days: Int, today: LocalDate): IO[Map[UUID, Double]] =
    if ids.isEmpty then IO.pure(Map.empty)
    else tx(
      sql"""select monitor_id, round(100.0 * sum(up_n) / sum(total_n), 2)::float8 from check_daily
            where monitor_id = any(${ids.toArray}) and day >= ${today.minusDays(days - 1L)} group by monitor_id having sum(total_n) > 0"""
        .query[(UUID, Double)].to[List].map(_.toMap)
    )

  def detail(id: UUID): IO[Option[MonitorDetailDto]] =
    find(id).flatMap(_.traverse { m =>
      (checks(id, 100), incidents(Some(id), false, 20), days(List(id), Days, LocalDate.now(ZoneOffset.UTC)))
        .mapN((cs, is, ds) => MonitorDetailDto(m, cs, is, ds.getOrElse(id, Nil)))
    })

  /** 状態ごとの監視の数（正常・停止・一時停止・未確認）と、続いている障害の数. */
  def counts: IO[(Int, Int, Int, Int, Int)] = tx(
    sql"""select count(*) filter (where status = 'up'), count(*) filter (where status = 'down'),
                 count(*) filter (where status = 'paused'), count(*) filter (where status = 'pending'),
                 (select count(*) from incident where resolved_at is null) from monitor"""
      .query[(Int, Int, Int, Int, Int)].unique
  )

  /** 古い確認の結果を消す. */
  def purge: IO[Int] = tx(
    sql"delete from check_result where checked_at < now() - make_interval(days => $KeepDays)".update.run *>
      (fr"delete from check_daily where day <" ++ today ++ fr"- $DailyKeepDays").update.run
  )
