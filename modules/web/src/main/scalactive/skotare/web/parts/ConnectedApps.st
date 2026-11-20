<script lang="scala">
import skotare.web.*
import skotare.shared.*

import scala.scalajs.js

// 連携しているサービス（OAuth で skotare を使うことを許可したもの. skakapo など）の確認と取り消し
case class State(grants: RV[List[OAuthGrantDto]] = RV(Nil))

def initState() = State()

def mounted(s: State): Unit = load(s)

def load(s: State): Unit = Toasts.report(Api.oauthGrants())(gs => s.grants := gs)

def revoke(s: State, g: OAuthGrantDto): Unit =
  Dialog.confirm(s"「${g.name}」との連携を取り消しますか？（${g.name} は、もう一度許可するまで ${AppNames.selfName} を使えなくなります）", "取り消す") {
    Toasts.report(Api.revokeOAuthGrant(g.clientId)) { _ =>
      Toasts.show("連携を取り消しました")
      load(s)
    }
  }

def when(iso: String): String =
  new js.Date(iso).asInstanceOf[js.Dynamic].toLocaleString("ja-JP", js.Dynamic.literal(dateStyle = "medium", timeStyle = "short")).asInstanceOf[String]
def info(g: OAuthGrantDto): String = List(
  if (g.scope == TokenScope.Write) "読み書き（read・write）" else "参照のみ（read）",
  s"許可 ${when(g.grantedAt)}",
  g.lastUsedAt.fold("未使用")(u => s"最後に使用 ${when(u)}")
).mkString("・")

def none(s: State) = s.grants.mapC(_.isEmpty)
</script>

<template>
  <section class="panel">
    <h2>連携しているサービス</h2>
    <p class="small">あなたの権限で {{ AppNames.self }} を使うことを許可したサービス（AI エージェントの {{ AppNames.of("skakapo") }} など）です。取り消すと、そのサービスはすぐに使えなくなります。</p>
    <div v-for="g in s.grants" :key="g.clientId" class="grant">
      <div class="grow">
        <div>{{ g.name }}</div>
        <div class="muted small">{{ info(g) }}</div>
      </div>
      <button class="danger small" @click="revoke(s, g)">取り消す</button>
    </div>
    <p v-if="none(s)" class="muted small">連携しているサービスはありません。</p>
  </section>
</template>

<style scoped>
.grant { display: flex; align-items: center; gap: .6rem; padding: .5rem 0; border-bottom: 1px solid var(--line); }
</style>
