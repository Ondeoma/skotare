package skotare.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skotare.shared.*

import java.time.Instant
import java.util.UUID

/** ステータスページ. */
final class PageStore(xa: Transactor[IO]) extends TxStore(xa):

  private def items(id: UUID): ConnectionIO[List[PageItem]] =
    sql"select monitor_id, display_name from status_page_monitor where page_id = $id order by position, monitor_id"
      .query[(UUID, Option[String])].to[List].map(_.map(PageItem.apply))

  private val cols = fr"id, slug, title, description, notice, public from status_page"

  private def withItems(t: (UUID, String, String, String, String, Boolean)): ConnectionIO[StatusPageDto] =
    items(t._1).map(is => StatusPageDto(t._1, t._2, t._3, t._4, t._5, t._6, is))

  def list: IO[List[StatusPageDto]] = tx(
    (fr"select" ++ cols ++ fr"order by title").query[(UUID, String, String, String, String, Boolean)].to[List].flatMap(_.traverse(withItems))
  )

  def find(id: UUID): IO[Option[StatusPageDto]] = tx(findC(fr"id = $id"))
  def bySlug(slug: String): IO[Option[StatusPageDto]] = tx(findC(fr"slug = $slug"))

  private def findC(cond: Fragment) =
    (fr"select" ++ cols ++ fr"where" ++ cond).query[(UUID, String, String, String, String, Boolean)].option.flatMap(_.traverse(withItems))

  /** 作る・変える. slug がほかのページと同じなら Left. */
  def save(id: Option[UUID], r: StatusPageReq): IO[Either[String, Option[StatusPageDto]]] = tx {
    val pid = id.getOrElse(UUID.randomUUID())
    sql"select count(*) from status_page where slug = ${r.slug} and id <> $pid".query[Int].unique.flatMap {
      case n if n > 0 => Left("その URL の名前はほかのページで使っています").pure[ConnectionIO]
      case _ =>
        val write = id match
          case None => sql"""insert into status_page (id, slug, title, description, notice, public)
                             values ($pid, ${r.slug}, ${r.title}, ${r.description}, ${r.notice}, ${r.public})""".update.run
          case Some(_) => sql"""update status_page set slug = ${r.slug}, title = ${r.title}, description = ${r.description},
                                notice = ${r.notice}, public = ${r.public} where id = $pid""".update.run
        write.flatMap {
          case 0 => Right(none[StatusPageDto]).pure[ConnectionIO]
          case _ =>
            sql"delete from status_page_monitor where page_id = $pid".update.run *>
              r.items.distinctBy(_.monitorId).zipWithIndex.traverse_ { (it, i) =>
                sql"""insert into status_page_monitor (page_id, monitor_id, position, display_name)
                      select $pid, id, $i, ${it.displayName.map(_.trim).filter(_.nonEmpty)} from monitor where id = ${it.monitorId}""".update.run
              } *> findC(fr"id = $pid").map(Right(_))
        }
    }
  }

  def delete(id: UUID): IO[Boolean] = tx(sql"delete from status_page where id = $id".update.run.map(_ > 0))

/**
 * 動いているノードと拠点（冗長構成）. 心拍を書き、90秒以上書いていないノードは止まったとみなす.
 * 心拍のたびに、自分の拠点と、その拠点で確かめる監視の行（monitor_region）を用意する.
 */
final class NodeStore(xa: Transactor[IO]) extends TxStore(xa):

  /**
   * 心拍. 拠点が新しくできたとき、または fill のとき（起動直後と、ときどき）に、その拠点で確かめる監視の行を補う
   * （監視の作成と拠点の作成が同時に起きて漏れた行も、いずれ補われる）.
   */
  def beat(name: String, region: String, version: String, started: Instant, fill: Boolean = true): IO[Unit] = tx(
    for
      created <- sql"insert into region (name) values ($region) on conflict do nothing".update.run
      _ <- sql"""insert into node (name, region, version, started_at, seen_at) values ($name, $region, $version, $started, now())
                 on conflict (name) do update set region = excluded.region, version = excluded.version, started_at = excluded.started_at, seen_at = now()""".update.run
      _ <- (if fill || created > 0 then sql"insert into monitor_region (monitor_id, region) select id, $region from monitor on conflict do nothing".update.run
            else 0.pure[ConnectionIO])
    yield ()
  )

  def list: IO[List[NodeDto]] = tx(
    (fr"select name, version, started_at, seen_at, seen_at >" ++ Sql.AliveWindow ++ fr""", region from node
          where seen_at > now() - interval '1 day' order by region, name""").query[(String, String, Instant, Instant, Boolean, String)].to[List]
      .map(_.map((n, v, s, l, a, r) => NodeDto(n, v, s.toString, l.toString, a, r)))
  )

  def regions: IO[List[RegionDto]] = tx(
    (fr"select r.name, r.health, r.reason, r.since," ++ Sql.aliveNodes(fr"r.name") ++ fr"from region r order by r.name")
      .query[(String, String, String, Instant, Int)].to[List]
      .map(_.map((n, h, r, s, c) => RegionDto(n, h, r, s.toString, c)))
  )

  /** 1日以上心拍のないノードと、動いているノードが1日以上ない拠点を片づける（拠点の確認の状態も消える）. */
  def purge: IO[Int] = tx(
    sql"delete from node where seen_at < now() - interval '1 day'".update.run *>
      sql"delete from region r where r.created_at < now() - interval '1 day' and not exists (select 1 from node n where n.region = r.name)".update.run
  )
