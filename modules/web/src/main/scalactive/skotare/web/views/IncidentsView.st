<script lang="scala">
import skotare.web.*
import skotare.shared.*

// 障害の一覧（停止してから復旧するまで. 新しい順）
case class State(open: RV[Boolean] = RV(false), list: RV[List[IncidentDto]] = RV(Nil), loaded: RV[Boolean] = RV(false))

def initState() = State()

def load(s: State): Unit = Store.report(Api.incidents(s.open.v)) { is => s.list := is; s.loaded := true }

def mounted(s: State): Unit = { pageTitle("障害"); load(s); watch(s.open)((_, _) => load(s)) }

def empty(s: State) = computed(s.loaded.v && s.list.v.isEmpty)
</script>

<template>
  <div class="page">
    <div class="row head"><h1 class="grow">障害</h1>
      <label class="row small"><input type="checkbox" v-model="s.open"> 続いているものだけ</label></div>
    <p v-if="empty(s)" class="panel muted">障害はありません。</p>
    <table v-else class="list">
      <thead><tr><th>監視</th><th>始まり</th><th>時間</th><th>原因</th></tr></thead>
      <tbody><tr v-for="i in s.list" class="click" @click='Store.go(s"/monitors/${i.monitorId}")'>
        <td><b>{{ i.monitorName }}</b></td>
        <td class="nowrap">{{ Fmt.dateTime(i.startedAt) }}</td>
        <td class="nowrap">{{ Fmt.span(i.startedAt, i.resolvedAt) }} <span v-if="i.resolvedAt.isEmpty" class="st st-down">続いている</span><span v-else class="st st-up">復旧</span></td>
        <td class="small">{{ i.cause }}</td>
      </tr></tbody>
    </table>
  </div>
</template>

<style scoped>
.head { align-items: center; }
</style>
