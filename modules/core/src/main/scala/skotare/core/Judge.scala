package skotare.core

import skotare.shared.*

/**
 * 複数の拠点の結果から、「skotare 側（拠点）の故障」か「監視先の故障」かを判定する.
 *
 *  1. 拠点の健全性（[[Judge.region]]）
 *     - 動いているノードがない → offline
 *     - 基準点（canary）の過半数に届かない → suspect（その拠点のネットワーク・DNS などの故障の疑い）
 *     - ほかの拠点では正常な監視先の半分以上（3件以上）に、その拠点だけが届かない → suspect
 *  2. 監視の状態（[[Judge.overall]]）: 健全な拠点の票だけを数え、停止と見ている拠点が必要な数（既定は過半数）に達したら停止.
 *     健全な拠点が1つもなければ、監視先の状態は判断できないので変えない（保留. 拠点の異常だけを知らせる）.
 */
object Judge {

  /** 疑うのに要る「その拠点だけ届かない監視先」の最小の数（監視が少ないうちに、たまたまの失敗で疑わないため）. */
  val MinLonely = 3

  /**
   * 拠点の材料. canaryUp・canaryDown はその拠点から見た基準点の状態の数.
   * lonelyDown は「ほかの動いている拠点では正常なのに、この拠点では停止」の監視先の数、comparable は比べられる監視先の数.
   */
  final case class RegionFacts(aliveNodes: Int,
                               canaryUp: Int,
                               canaryDown: Int,
                               lonelyDown: Int,
                               comparable: Int,
                               stalled: Boolean = false)

  /** (health, 理由). */
  def region(f: RegionFacts): (String, String) = {
    val canaries = f.canaryUp + f.canaryDown
    if f.aliveNodes <= 0 then RegionHealth.Offline -> "動いているノードがありません"
    else if f.stalled then RegionHealth.Suspect -> "ノードは動いていますが、確認が回っていません（確かめる時刻を大きく過ぎています）"
    else if canaries > 0 && f.canaryDown * 2 > canaries then
      RegionHealth.Suspect -> s"基準点 ${f.canaryDown}/$canaries 件に届きません（この拠点のネットワークの故障の疑い）"
    else if f.lonelyDown >= MinLonely && f.lonelyDown * 2 >= f.comparable then
      RegionHealth.Suspect -> s"ほかの拠点では正常な監視先 ${f.lonelyDown}/${f.comparable} 件に、この拠点だけが届きません"
    else RegionHealth.Ok -> ""
  }

  /** 停止と判断するのに要る拠点の数. confirm が 0 なら過半数、拠点の数より多ければすべて. */
  def need(voters: Int, confirm: Int): Int = if confirm <= 0 then voters / 2 + 1 else confirm min voters

  /**
   * 健全な拠点の票（未確認 pending も1票として数える）から、監視の状態を決める.
   * 停止の票が必要な数に達したら停止、残りの票がすべて停止でも達しないほど正常の票があれば正常、どちらでもなければ None（今のまま）.
   * （作ったばかり・確かめ直しの直後に、先に確かめた1つの拠点だけで停止と判断しないため）
   */
  def overall(votes: List[String], confirm: Int): Option[String] =
    if votes.isEmpty then None
    else {
      val n = need(votes.size, confirm)
      if votes.count(_ == MonStatus.Down) >= n then Some(MonStatus.Down)
      else if votes.count(_ == MonStatus.Up) > votes.size - n then Some(MonStatus.Up)
      else None
    }

  /** ある拠点から見た、ある監視の状態. canary は基準点か、stale は確かめる時刻を大きく過ぎているか（その状態は古いので比べない）. */
  final case class Vote(monitor: java.util.UUID,
                        region: String,
                        status: String,
                        canary: Boolean,
                        stale: Boolean = false)

  /** 拠点ごとの (health, 理由)（regions と同じ順）. regions は (拠点, 動いているノードの数)、votes は一時停止していない監視の拠点ごとの状態. */
  def regions(regions: List[(String, Int)], votes: List[Vote]): List[(String, String)] = {
    val alive = regions.filter(_._2 > 0).map(_._1).toSet
    val byMonitor = votes.groupBy(_.monitor)
    def known(v: Vote) = !v.stale && (v.status == MonStatus.Up || v.status == MonStatus.Down)
    regions.map {
      (name, nodes) =>
        val all = votes.filter(_.region == name)
        val mine = all.filterNot(_.stale)
        val canary = mine.filter(_.canary)
        def others(v: Vote) = byMonitor(v.monitor).filter(x => x.region != name && alive(x.region) && known(x))
        val comparable = mine.filter(v => !v.canary && known(v) && others(v).nonEmpty)
        val lonely = comparable.filter(v => v.status == MonStatus.Down && others(v).exists(_.status == MonStatus.Up))
        region(RegionFacts(
          nodes,
          canary.count(_.status == MonStatus.Up),
          canary.count(_.status == MonStatus.Down),
          lonely.size,
          comparable.size,
          stalled = all.nonEmpty && all.forall(_.stale)
        ))
    }
  }
}
