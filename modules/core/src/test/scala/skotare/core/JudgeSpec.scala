package skotare.core

import skotare.shared.*

import java.util.UUID

class JudgeSpec extends munit.FunSuite {
  import Judge.*
  import MonStatus.{Down, Up, Pending}

  test("監視の状態: 健全な拠点の過半数（または指定の数）が停止と見たら停止") {
    assertEquals(overall(Nil, 0), None)
    assertEquals(overall(List(Pending, Pending), 0), None)
    assertEquals(overall(List(Down), 0), Some(Down))
    assertEquals(overall(List(Up, Down), 0), Some(Up)) // 2拠点なら2つとも
    assertEquals(overall(List(Down, Down), 0), Some(Down))
    assertEquals(overall(List(Up, Down, Down), 0), Some(Down)) // 3拠点なら2つ
    assertEquals(overall(List(Up, Up, Down), 0), Some(Up))
    assertEquals(overall(List(Up, Up, Down), 1), Some(Down)) // 1拠点でも停止と見たら
    assertEquals(overall(List(Up, Down, Down), 20), Some(Up)) // すべての拠点
    assertEquals(overall(List(Down, Pending), 0), None) // 未確認の拠点も数える（先に確かめた1つだけで停止にしない）
    assertEquals(overall(List(Down, Down, Pending), 0), Some(Down))
    assertEquals(overall(List(Up, Pending, Pending), 0), None)
    assertEquals(overall(List(Up, Up, Pending), 0), Some(Up))
  }

  test("拠点の健全性: ノードがない・基準点に届かない・その拠点だけ届かない") {
    assertEquals(region(RegionFacts(0, 0, 0, 0, 0))._1, RegionHealth.Offline)
    assertEquals(region(RegionFacts(1, 0, 1, 0, 0))._1, RegionHealth.Suspect)
    assertEquals(region(RegionFacts(1, 1, 1, 0, 0))._1, RegionHealth.Ok) // 半分では疑わない
    assertEquals(region(RegionFacts(1, 0, 0, 2, 2))._1, RegionHealth.Ok) // 少なすぎる
    assertEquals(region(RegionFacts(1, 0, 0, 3, 6))._1, RegionHealth.Suspect)
    assertEquals(region(RegionFacts(1, 0, 0, 3, 10))._1, RegionHealth.Ok)
  }

  test("拠点の健全性: ほかの拠点も届かない監視先は、その拠点の異常に数えない") {
    val ms = List.fill(4)(UUID.randomUUID())
    def votes(
      a: String,
      b: String
    ) = ms.flatMap(m => List(Vote(m, "a", a, false), Vote(m, "b", b, false)))
    // a だけが届かない
    assertEquals(
      regions(List("a" -> 1, "b" -> 1), votes(Down, Up)).map(_._1),
      List(RegionHealth.Suspect, RegionHealth.Ok)
    )
    // 両方届かない（監視先の故障）
    assertEquals(regions(List("a" -> 1, "b" -> 1), votes(Down, Down)).map(_._1), List(RegionHealth.Ok, RegionHealth.Ok))
    // a の票が古い（確認が回っていない）なら比べない. すべて古ければ a を疑う
    val staleA = ms.flatMap(m => List(Vote(m, "a", Down, false, stale = true), Vote(m, "b", Up, false)))
    assertEquals(regions(List("a" -> 1, "b" -> 1), staleA).map(_._1), List(RegionHealth.Suspect, RegionHealth.Ok))
    assert(regions(List("a" -> 1, "b" -> 1), staleA).head._2.contains("確認が回っていません"))
    // b が止まっていれば、比べられない
    assertEquals(
      regions(List("a" -> 1, "b" -> 0), votes(Down, Up)).map(_._1),
      List(RegionHealth.Ok, RegionHealth.Offline)
    )
  }
}
