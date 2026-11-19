<script lang="scala">
import skotare.web.*
import skotare.web.parts.*
import skotare.shared.*

import org.scalajs.dom
import scala.util.{Failure, Success}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// ゲストに見せるステータスページ（ログインしなくてよい）. 1分ごとに読み直す
case class Props(slug: String)

case class State(page: RV[Option[PublicStatusDto]] = RV(None), error: RV[String] = RV(""), timer: RV[Int] = RV(0))

def initState() = State()

def load(s: State, props: Props): Unit = Api.publicStatus(props.slug).onComplete {
  case Success(p) => s.page := Some(p); s.error := ""; pageTitle(p.title)
  case Failure(e) => if (s.page.v.isEmpty) s.error := e.getMessage
}

def mounted(s: State, props: Props): Unit = {
  load(s, props)
  s.timer := dom.window.setInterval(() => load(s, props), 60000)
}

def unmounted(s: State): Unit = dom.window.clearInterval(s.timer.v)

def pages(s: State) = s.page.mapC(_.toList)
def bannerClass(p: PublicStatusDto): String = s"banner banner-${p.overall}"
def ongoing(p: PublicStatusDto): List[PublicIncident] = p.incidents.filter(_.resolvedAt.isEmpty)
def past(p: PublicStatusDto): List[PublicIncident] = p.incidents.filter(_.resolvedAt.isDefined)
</script>

<template>
  <div class="status-page">
    <p v-if="s.error.mapC(_.nonEmpty)" class="panel">{{ s.error }}</p>
    <template v-for="p in pages(s)">
      <header class="sp-head">
        <img :src='AppNames.logo("/logo.svg").v' alt="" height="36">
        <div class="grow"><h1>{{ p.title }}</h1><p v-if="p.description.nonEmpty" class="muted">{{ p.description }}</p></div>
      </header>
      <div :class="bannerClass(p)">{{ Overall.label(p.overall) }}</div>
      <div v-if="p.notice.nonEmpty" class="panel notice">📢 {{ p.notice }}</div>
      <section v-if="ongoing(p).nonEmpty" class="panel">
        <h2>発生中の障害</h2>
        <p v-for="i in ongoing(p)">🔴 <b>{{ i.name }}</b> — {{ Fmt.dateTime(i.startedAt) }} から（{{ Fmt.span(i.startedAt, None) }}）</p>
      </section>
      <section class="panel">
        <div v-for="it in p.items" class="item">
          <div class="row">
            <b class="grow">{{ it.name }}</b>
            <span class="muted small">{{ Fmt.pct(it.uptime90d) }}（90日）</span>
            <span :class="Fmt.statusClass(it.status)">{{ MonStatus.label(it.status) }}</span>
          </div>
          <DayBars :days="it.days" />
        </div>
      </section>
      <section class="panel">
        <h2>過去14日の障害</h2>
        <p v-if="past(p).isEmpty" class="muted">障害はありませんでした。</p>
        <p v-for="i in past(p)" class="small">✅ <b>{{ i.name }}</b> — {{ Fmt.dateTime(i.startedAt) }}（{{ Fmt.span(i.startedAt, i.resolvedAt) }}停止）</p>
      </section>
      <p class="muted small foot">最終更新 {{ Fmt.dateTime(p.updatedAt) }} · {{ AppNames.self }}</p>
    </template>
  </div>
</template>

<style scoped>
.status-page { max-width: 820px; margin: 0 auto; padding: 1.4rem 1rem 3rem; }
.sp-head { display: flex; align-items: center; gap: .8rem; margin-bottom: 1rem; }
.sp-head h1 { margin: 0; font-size: 1.5rem; }
.sp-head p { margin: .1rem 0 0; }
.banner { border-radius: 10px; padding: 1rem 1.2rem; font-weight: 700; font-size: 1.1rem; color: #fff; margin-bottom: 1rem; background: var(--muted); }
.banner-up { background: var(--up); }
.banner-partial { background: var(--warn); }
.banner-down { background: var(--down); }
.notice { white-space: pre-wrap; }
.item { padding: .7rem 0; border-bottom: 1px solid var(--line); }
.item:last-child { border-bottom: none; }
.item .row { align-items: center; margin-bottom: .4rem; }
.foot { text-align: center; }
</style>
