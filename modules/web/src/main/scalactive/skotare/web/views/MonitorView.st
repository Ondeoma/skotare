<script lang="scala">
import skotare.web.*
import skotare.web.parts.*
import skotare.shared.*

import org.scalajs.dom
import java.util.UUID

// 監視の詳細: 状態・稼働率・応答時間のグラフ・90日の帯・障害・最近の結果. 15秒ごとに読み直す
case class Props(monitorId: UUID)

case class State(d: RV[Option[MonitorDetailDto]] = RV(None), timer: RV[Int] = RV(0))

def initState() = State()

def load(s: State, props: Props): Unit = Store.report(Api.monitor(props.monitorId)) { d =>
  s.d := Some(d)
  pageTitle(d.monitor.name)
}

def mounted(s: State, props: Props): Unit = {
  load(s, props)
  s.timer := dom.window.setInterval(() => if (dom.document.visibilityState.asInstanceOf[String] == "visible") load(s, props), 15000)
}

def unmounted(s: State): Unit = dom.window.clearInterval(s.timer.v)

def m(s: State) = s.d.mapC(_.map(_.monitor).toList)
def checkNow(s: State, props: Props): Unit = Store.report(Api.checkNow(props.monitorId)) { _ =>
  Store.toast("確かめています…")
  dom.window.setTimeout(() => load(s, props), 3000)
}
def toggle(s: State, props: Props, mo: MonitorDto): Unit =
  Store.report(Api.saveMonitor(Some(mo.id), mo.toReq.copy(enabled = !mo.enabled))) { _ =>
    Store.toast(if (mo.enabled) "一時停止しました" else "再開しました"); load(s, props); Store.refresh()
  }
def remove(mo: MonitorDto): Unit =
  Dialog.confirm(s"「${mo.name}」を削除しますか？（確認の結果と障害の記録も消えます）", "削除する") {
    Store.report(Api.deleteMonitor(mo.id)) { _ => Store.toast("削除しました"); Store.refresh(); Store.go("/") }
  }

def checks(s: State) = s.d.mapC(_.map(_.checks).getOrElse(Nil))
def incidents(s: State) = s.d.mapC(_.map(_.incidents).getOrElse(Nil))
def days(s: State) = s.d.mapC(_.map(_.days).getOrElse(Nil))
def avgLatency(s: State) = s.d.mapC(_.map(_.checks.flatMap(_.latencyMs)).filter(_.nonEmpty).map(xs => s"${xs.sum / xs.size}ms").getOrElse("—"))
def hasIncidents(s: State) = incidents(s).mapC(_.nonEmpty)
def regionNote(r: RegionStateDto): String = if (r.counted) "数える" else "数えない（拠点の異常の疑い・停止）"
def confirmText(mo: MonitorDto): String = ConfirmRegions.label(mo.confirmRegions, mo.regions.size)

/** 応答時間のグラフ（最近の100回. 失敗は赤い点）. */
def chart(s: State) = s.d.mapC { od =>
  val cs = od.map(_.checks.reverse).getOrElse(Nil)
  if (cs.size < 2) "<p class=\"muted small\">まだ結果が足りません</p>"
  else {
    val (w, h, pad) = (800.0, 160.0, 4.0)
    val max = math.max(cs.flatMap(_.latencyMs).maxOption.getOrElse(1), 1).toDouble
    val dx = (w - pad * 2) / (cs.size - 1)
    def y(v: Int) = h - pad - (v / max) * (h - pad * 2 - 14)
    val pts = cs.zipWithIndex.flatMap((c, i) => c.latencyMs.filter(_ => c.ok).map(v => f"${pad + i * dx}%.1f,${y(v)}%.1f"))
    val bad = cs.zipWithIndex.filterNot(_._1.ok).map((_, i) => f"""<line x1="${pad + i * dx}%.1f" x2="${pad + i * dx}%.1f" y1="0" y2="$h%.0f" class="ng"/>""")
    s"""<svg viewBox="0 0 $w $h" preserveAspectRatio="none" class="lat"><text x="4" y="12" class="axis">${max.toInt}ms</text>${bad.mkString}
       |<polyline points="${pts.mkString(" ")}" class="line"/></svg>""".stripMargin
  }
}
</script>

<template>
  <div class="page">
    <p class="small"><a href="/" @click.prevent='Store.go("/")'>← 監視</a></p>
    <template v-for="mo in m(s)">
      <div class="row head">
        <h1 class="grow">{{ mo.name }} <span :class="Fmt.statusClass(mo.status)">{{ MonStatus.label(mo.status) }}</span><span v-if="mo.canary" class="st">基準点</span></h1>
        <button v-if="mo.enabled" @click="checkNow(s, props)">今すぐ確かめる</button>
        <button @click='Store.go(s"/monitors/${mo.id}/edit")'>編集</button>
        <button class="ghost" @click="toggle(s, props, mo)">{{ if (mo.enabled) "一時停止" else "再開" }}</button>
        <button class="ghost danger" @click="remove(mo)">削除</button>
      </div>
      <p class="muted"><code>{{ Kind.label(mo.kind) }} · {{ mo.target }}</code> · {{ Fmt.interval(mo.intervalSec) }}ごと · タイムアウト {{ mo.timeoutMs }}ms</p>
      <p v-if="mo.description.nonEmpty">{{ mo.description }}</p>
      <div class="kpis">
        <div class="card kpi"><span class="muted small">今の状態</span><b>{{ MonStatus.label(mo.status) }}</b><span class="muted small">{{ Fmt.span(mo.statusSince, None) }}前から</span></div>
        <div class="card kpi"><span class="muted small">稼働率（24時間）</span><b>{{ Fmt.pct(mo.uptime24h) }}</b></div>
        <div class="card kpi"><span class="muted small">稼働率（30日）</span><b>{{ Fmt.pct(mo.uptime30d) }}</b></div>
        <div class="card kpi"><span class="muted small">平均の応答（最近）</span><b>{{ avgLatency(s) }}</b></div>
      </div>
      <p v-if='mo.status == MonStatus.Down' class="panel downmsg">🔴 {{ mo.lastMessage.getOrElse("") }}</p>
      <section class="panel">
        <h2>拠点ごとの状態</h2>
        <p class="muted small">判定に数える拠点のうち、{{ confirmText(mo) }}が停止と見たときに停止と判断します。異常の疑いがある拠点・止まっている拠点の結果は数えません（skotare 側の問題として扱います）。</p>
        <table class="list">
          <thead><tr><th>拠点</th><th>状態</th><th class="num">応答</th><th class="hide-sp">確認</th><th>内容</th><th class="hide-sp">判定</th></tr></thead>
          <tbody><tr v-for="r in mo.regions" :class='if (r.counted) "" else "excluded"'>
            <td><b>{{ r.region }}</b></td>
            <td><span :class="Fmt.statusClass(r.status)">{{ MonStatus.label(r.status) }}</span></td>
            <td class="num">{{ Fmt.ms(r.latencyMs) }}</td>
            <td class="hide-sp muted small">{{ r.lastCheckedAt.fold("まだ")(Fmt.ago) }}</td>
            <td class="small">{{ r.message.getOrElse("") }}</td>
            <td class="hide-sp small">{{ regionNote(r) }}</td>
          </tr></tbody>
        </table>
      </section>
    </template>
    <section class="panel">
      <h2>応答時間</h2>
      <div class="chart" v-html="chart(s)"></div>
    </section>
    <section class="panel">
      <h2>90日の稼働率</h2>
      <DayBars :days="days(s).v" />
    </section>
    <section v-if="hasIncidents(s)" class="panel">
      <h2>障害</h2>
      <table class="list">
        <thead><tr><th>始まり</th><th>時間</th><th>原因</th></tr></thead>
        <tbody><tr v-for="i in incidents(s)">
          <td>{{ Fmt.dateTime(i.startedAt) }}</td>
          <td>{{ Fmt.span(i.startedAt, i.resolvedAt) }}<span v-if="i.resolvedAt.isEmpty" class="st st-down">続いている</span></td>
          <td class="small">{{ i.cause }}</td>
        </tr></tbody>
      </table>
    </section>
    <section class="panel">
      <h2>最近の結果</h2>
      <table class="list">
        <thead><tr><th>時刻</th><th>結果</th><th class="num">応答</th><th>内容</th><th class="hide-sp">拠点・ノード</th></tr></thead>
        <tbody><tr v-for="c in checks(s)">
          <td class="nowrap">{{ Fmt.dateTime(c.at) }}</td>
          <td><span :class='if (c.ok) "st st-up" else "st st-down"'>{{ if (c.ok) "OK" else "NG" }}</span></td>
          <td class="num">{{ Fmt.ms(c.latencyMs) }}</td>
          <td class="small">{{ c.message }}</td>
          <td class="hide-sp muted small">{{ c.region }} · {{ c.node }}</td>
        </tr></tbody>
      </table>
    </section>
  </div>
</template>

<style scoped>
.head { align-items: center; flex-wrap: wrap; }
.head h1 { display: flex; align-items: center; gap: .6rem; }
.kpis { display: grid; grid-template-columns: repeat(4, 1fr); gap: .6rem; margin: .6rem 0; }
.kpi { display: flex; flex-direction: column; gap: .15rem; padding: .7rem 1rem; }
.kpi b { font-size: 1.3rem; }
.downmsg { border-color: var(--down); }
tr.excluded td { opacity: .55; }
@media (max-width: 720px) { .kpis { grid-template-columns: repeat(2, 1fr); } }
</style>
