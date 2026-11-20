package skotare.db

import doobie.*
import doobie.implicits.*

/** ノード・拠点の生死を見る SQL の部品（skotare 固有. 共通の部品は sbirds.db.Sql）. */
object NodeSql {

  /** ノードが止まったとみなすまでの、心拍のない時間. */
  val AliveWindow: Fragment = fr"now() - interval '90 seconds'"

  /** 拠点の行（mr）が、確かめる時刻を間隔（監視 m）の3倍・最低2分より過ぎている（確認が回っていない. 古い票として数えない）. */
  val stale: Fragment = fr"mr.next_check_at < now() - make_interval(secs => greatest(m.interval_sec * 3, 120))"

  /** 拠点で動いているノードの数（region は拠点の名前の式）. */
  def aliveNodes(region: Fragment): Fragment =
    fr"(select count(*)::int from node n where n.region =" ++ region ++ fr"and n.seen_at >" ++ AliveWindow ++ fr")"
}
