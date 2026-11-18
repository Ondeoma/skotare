<script lang="scala">
import skotare.web.*
import skotare.web.parts.*
import skotare.shared.*

// 監視の一覧（ホーム）: 全体の様子と、監視ごとの状態・最近の結果・稼働率
case class State(q: RV[String] = RV(Query.get("q").getOrElse("")), status: RV[String] = RV(""))

def initState() = State()

def mounted(): Unit = { pageTitle("監視"); Store.refresh() }

def shown(s: State) = computed {
  val q = s.q.v.trim.toLowerCase
  Store.monitors.v.filter(m => (s.status.v.isEmpty || m.status == s.status.v) &&
    (q.isEmpty || m.name.toLowerCase.contains(q) || m.target.toLowerCase.contains(q)))
}

def countOf(st: String) = Store.summary.mapC(_.map(x => st match {
  case MonStatus.Up => x.up; case MonStatus.Down => x.down; case MonStatus.Paused => x.paused; case _ => x.pending
}).getOrElse(0))

def filterBy(s: State, st: String): Unit = s.status := (if (s.status.v == st) "" else st)
def cardClass(s: State, st: String) = s.status.mapC(c => s"card stat stat-$st" + (if (c == st) " on" else ""))
def regions = Store.summary.mapC(_.map(_.regions).getOrElse(Nil))
def badRegions = regions.mapC(_.filter(_.health != RegionHealth.Ok))
def regionClass(r: RegionDto): String = s"chip h-${r.health}"
def regionText(r: RegionDto): String = s"${r.name} · ${RegionHealth.label(r.health)} · ノード ${r.nodes} 台"
def regionTitle(r: RegionDto): String = Store.summary.v.map(_.nodes.filter(n => n.alive && n.region == r.name).map(_.name).mkString("・")).getOrElse("")
/** 正常だが、判定に数える拠点のうち一部から届かない. */
def partialText(m: MonitorDto): String = {
  val downs = m.regions.filter(r => r.counted && r.status == MonStatus.Down).map(_.region)
  if (m.status == MonStatus.Up && downs.nonEmpty) s"一部の拠点（${downs.mkString("・")}）から届きません" else ""
}
def empty = Store.monitors.mapC(_.isEmpty)
def latency(m: MonitorDto): String = Fmt.ms(m.lastLatencyMs)
def checked(m: MonitorDto): String = m.lastCheckedAt.fold("まだ")(Fmt.ago)
val statuses = List(MonStatus.Up, MonStatus.Down, MonStatus.Paused, MonStatus.Pending)
</script>

<template>
  <div class="page">
    <div class="row head">
      <h1 class="grow">監視</h1>
      <button class="primary" @click='Store.go("/monitors/new")'>＋ 監視を追加</button>
    </div>
    <div class="stats">
      <button v-for="st in statuses" :class="cardClass(s, st)" @click="filterBy(s, st)">
        <span class="n">{{ countOf(st) }}</span><span class="small">{{ MonStatus.label(st) }}</span>
      </button>
    </div>
    <div class="regions">
      <span class="muted small">拠点</span>
      <span v-for="r in regions" :class="regionClass(r)" :title="regionTitle(r)">{{ regionText(r) }}</span>
    </div>
    <div v-for="r in badRegions" class="panel warn small">
      ⚠️ 拠点「{{ r.name }}」は{{ RegionHealth.label(r.health) }}です（{{ Fmt.ago(r.since) }}から）: {{ r.reason }}<br>
      skotare 側（この拠点）の問題として扱い、この拠点の結果は判定に数えていません。監視先の停止とは判断しません。
    </div>
    <div class="row">
      <input type="search" v-model="s.q" placeholder="名前・URL・ホストで絞り込む" class="grow">
    </div>
    <div v-if="empty" class="panel empty">
      <p>まだ監視がありません。</p>
      <p class="muted small">Web サイト（HTTP(S)）・ポート（TCP）・DNS・PING で、外から動いているかを確かめます。</p>
      <button class="primary" @click='Store.go("/monitors/new")'>最初の監視を追加する</button>
    </div>
    <table v-else class="list">
      <thead><tr><th>状態</th><th>名前</th><th class="hide-sp">最近の結果</th><th class="num">24時間</th><th class="num hide-sp">30日</th><th class="num hide-sp">応答</th><th class="hide-sp">確認</th></tr></thead>
      <tbody>
        <tr v-for="m in shown(s)" :key="m.id" class="click" @click='Store.go(s"/monitors/${m.id}")'>
          <td><span :class="Fmt.statusClass(m.status)">{{ MonStatus.label(m.status) }}</span></td>
          <td><b>{{ m.name }}</b><div class="muted small target">{{ Kind.label(m.kind) }} · {{ m.target }}</div>
            <div v-if="partialText(m).nonEmpty" class="small partial">⚠ {{ partialText(m) }}</div></td>
          <td class="hide-sp"><RecentBars :results="m.recent" /></td>
          <td class="num">{{ Fmt.pct(m.uptime24h) }}</td>
          <td class="num hide-sp">{{ Fmt.pct(m.uptime30d) }}</td>
          <td class="num hide-sp">{{ latency(m) }}</td>
          <td class="hide-sp muted small">{{ checked(m) }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<style scoped>
.head { align-items: center; }
.stats { display: grid; grid-template-columns: repeat(4, 1fr); gap: .6rem; margin: .4rem 0 .6rem; }
.stat { display: flex; flex-direction: column; align-items: flex-start; padding: .7rem 1rem; text-align: left; cursor: pointer; border: 1px solid var(--line); }
.stat .n { font-size: 1.7rem; font-weight: 700; line-height: 1.1; }
.stat.on { outline: 2px solid var(--accent); }
.stat-up .n { color: var(--up); }
.stat-down .n { color: var(--down); }
.stat-paused .n, .stat-pending .n { color: var(--muted); }
.target { max-width: 32rem; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.empty { text-align: center; padding: 2rem; }
.regions { display: flex; flex-wrap: wrap; align-items: center; gap: .4rem; margin: .2rem 0 .6rem; }
.chip { font-size: .78rem; padding: .15rem .55rem; border-radius: 999px; border: 1px solid var(--line); }
.chip.h-ok { color: var(--up); border-color: var(--up); }
.chip.h-suspect { color: #d97706; border-color: #d97706; }
.chip.h-offline { color: var(--down); border-color: var(--down); }
.warn { border-color: #d97706; margin: 0 0 .6rem; }
.partial { color: #d97706; }
@media (max-width: 720px) { .stats { grid-template-columns: repeat(2, 1fr); } }
</style>
