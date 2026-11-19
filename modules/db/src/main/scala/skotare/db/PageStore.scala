package skotare.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skotare.shared.*

import java.util.UUID

/** ステータスページ. */
final class PageStore(xa: Transactor[IO]) extends TxStore(xa) {

  private def items(id: UUID): ConnectionIO[List[PageItem]] =
    sql"select monitor_id, display_name from status_page_monitor where page_id = $id order by position, monitor_id"
      .query[(UUID, Option[String])].to[List].map(_.map(PageItem.apply))

  private val cols = fr"id, slug, title, description, notice, public from status_page"

  private def withItems(t: (UUID, String, String, String, String, Boolean)): ConnectionIO[StatusPageDto] =
    items(t._1).map(is => StatusPageDto(t._1, t._2, t._3, t._4, t._5, t._6, is))

  def list: IO[List[StatusPageDto]] = tx((fr"select" ++ cols ++ fr"order by title").query[(
    UUID,
    String,
    String,
    String,
    String,
    Boolean
  )].to[List].flatMap(_.traverse(withItems)))

  def find(id: UUID): IO[Option[StatusPageDto]] = tx(findC(fr"id = $id"))
  def bySlug(slug: String): IO[Option[StatusPageDto]] = tx(findC(fr"slug = $slug"))

  private def findC(cond: Fragment) = (fr"select" ++ cols ++ fr"where" ++ cond).query[(
    UUID,
    String,
    String,
    String,
    String,
    Boolean
  )].option.flatMap(_.traverse(withItems))

  /** 作る・変える. slug がほかのページと同じなら Left. */
  def save(id: Option[UUID], r: StatusPageReq): IO[Either[String, Option[StatusPageDto]]] = tx {
    val pid = id.getOrElse(UUID.randomUUID())
    sql"select count(*) from status_page where slug = ${r.slug} and id <> $pid".query[Int].unique.flatMap {
      case n if n > 0 => Left("その URL の名前はほかのページで使っています").pure[ConnectionIO]
      case _ =>
        val write = id match {
          case None => sql"""insert into status_page (id, slug, title, description, notice, public)
                             values ($pid, ${r.slug}, ${r.title}, ${r.description}, ${r.notice}, ${r.public})""".update.run
          case Some(_) =>
            sql"""update status_page set slug = ${r.slug}, title = ${r.title}, description = ${r.description},
                                notice = ${r.notice}, public = ${r.public} where id = $pid""".update.run
        }
        write.flatMap {
          case 0 => Right(none[StatusPageDto]).pure[ConnectionIO]
          case _ =>
            sql"delete from status_page_monitor where page_id = $pid".update.run *>
              r.items.distinctBy(_.monitorId).zipWithIndex.traverse_ {
                (it, i) =>
                  sql"""insert into status_page_monitor (page_id, monitor_id, position, display_name)
                      select $pid, id, $i, ${it.displayName.map(
                      _.trim
                    ).filter(_.nonEmpty)} from monitor where id = ${it.monitorId}""".update.run
              } *> findC(fr"id = $pid").map(Right(_))
        }
    }
  }

  def delete(id: UUID): IO[Boolean] = tx(sql"delete from status_page where id = $id".update.run.map(_ > 0))
}
