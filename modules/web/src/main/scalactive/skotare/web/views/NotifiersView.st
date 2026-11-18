<script lang="scala">
import skotare.web.*
import skotare.web.parts.*
import skotare.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// 通知先: 監視が停止・復旧したときに送る先（stui のチャンネル・skereru のメール・Webhook）
case class Edit(id: Option[UUID], name: RV[String], kind: RV[String], secret: RV[String], baseUrl: RV[String], from: RV[String], to: RV[String],
                all: RV[Boolean], monitors: RV[List[UUID]], enabled: RV[Boolean], hasSecret: Boolean, error: RV[String] = RV(""))

case class State(list: RV[List[NotifierDto]] = RV(Nil), edit: RV[Option[Edit]] = RV(None))

def initState() = State()

def load(s: State): Unit = Store.report(Api.notifiers())(s.list := _)

def mounted(s: State): Unit = { pageTitle("通知先"); load(s) }

def open(s: State, n: Option[NotifierDto]): Unit = s.edit := Some(n match {
  case Some(n) => Edit(Some(n.id), RV(n.name), RV(n.kind), RV(""), RV(n.config.baseUrl), RV(n.config.from), RV(n.config.to.mkString("\n")),
      RV(n.allMonitors), RV(n.monitorIds), RV(n.enabled), n.hasSecret)
  case None => Edit(None, RV(""), RV(NotifierKind.Stui), RV(""), RV(""), RV(""), RV(""), RV(true), RV(Nil), RV(true), false)
})

def save(s: State, e: Edit): Unit = {
  val r = NotifierReq(e.name.v.trim, e.kind.v, NotifierConfig(e.baseUrl.v.trim, e.from.v.trim, e.to.v.split(Array('\n', ',')).toList.map(_.trim).filter(_.nonEmpty)),
    Option(e.secret.v.trim).filter(_.nonEmpty), e.all.v, e.monitors.v, e.enabled.v)
  Api.saveNotifier(e.id, r).onComplete {
    case Success(_) => Store.toast("保存しました"); s.edit := None; load(s)
    case Failure(x) => e.error := x.getMessage
  }
}

def test(n: NotifierDto): Unit = Store.report(Api.testNotifier(n.id))(_ => Store.toast(s"「${n.name}」に試しに送りました"))

def remove(s: State, n: NotifierDto): Unit = Dialog.confirm(s"通知先「${n.name}」を削除しますか？", "削除する") {
  Store.report(Api.deleteNotifier(n.id)) { _ => Store.toast("削除しました"); load(s) }
}

def toggle(e: Edit, id: UUID): Unit = e.monitors := (if (e.monitors.v.contains(id)) e.monitors.v.filterNot(_ == id) else e.monitors.v :+ id)
def on(e: Edit, id: UUID) = e.monitors.mapC(_.contains(id))
def edits(s: State) = s.edit.mapC(_.toList)
def isSkereru(e: Edit) = e.kind.mapC(_ == NotifierKind.Skereru)
def secretLabel(e: Edit) = e.kind.mapC(k => NotifierKind.secretLabel(k) + (if (e.hasSecret) "（変えるときだけ入れる）" else ""))
def secretHint(e: Edit) = e.kind.mapC {
  case NotifierKind.Stui    => "stui のチャンネルの設定 →「受信 Webhook」で作った URL（https://…/hooks/…）"
  case NotifierKind.Skereru => "skereru の「設定 → APIキー・MCP」で、send のスコープを付けて発行したキー（skr_…）"
  case _                    => "停止・復旧のたびに JSON（event・monitor・message・at・url）を POST します"
}
def scope(n: NotifierDto): String = if (n.allMonitors) "すべての監視" else s"${n.monitorIds.size} 件の監視"
def empty(s: State) = s.list.mapC(_.isEmpty)
</script>

<template>
  <div class="page">
    <div class="row head"><h1 class="grow">通知先</h1><button class="primary" @click="open(s, None)">＋ 通知先を追加</button></div>
    <p class="muted small">監視が停止・復旧したときに送ります。送れなかったときは間隔を延ばして送り直します。プッシュ通知は「設定」から端末ごとに受け取れます。</p>
    <p v-if="empty(s)" class="panel muted">まだ通知先がありません。</p>
    <table v-else class="list">
      <thead><tr><th>名前</th><th>種類</th><th class="hide-sp">対象</th><th></th></tr></thead>
      <tbody><tr v-for="n in s.list">
        <td><b>{{ n.name }}</b><span v-if="!n.enabled" class="muted small">（止めています）</span></td>
        <td>{{ NotifierKind.label(n.kind) }}</td>
        <td class="hide-sp small">{{ scope(n) }}</td>
        <td class="right nowrap"><button class="small" @click="test(n)">試しに送る</button>
          <button class="small" @click="open(s, Some(n))">編集</button><button class="small ghost danger" @click="remove(s, n)">削除</button></td>
      </tr></tbody>
    </table>
    <Modal v-for="e in edits(s)" :onClose="_ => s.edit := None" :wide="true">
      <h2>{{ if (e.id.isDefined) "通知先の編集" else "通知先を追加" }}</h2>
      <form @submit.prevent="save(s, e)">
        <label class="field"><span>名前</span><input type="text" v-model="e.name" maxlength="100" placeholder="運用チャンネル"></label>
        <label class="field"><span>種類</span><select v-model="e.kind"><option v-for="k in NotifierKind.all" :value="k">{{ NotifierKind.label(k) }}</option></select></label>
        <label class="field"><span>{{ secretLabel(e) }}</span><input type="password" v-model="e.secret" autocomplete="off"></label>
        <p class="muted small">{{ secretHint(e) }}</p>
        <template v-if="isSkereru(e)">
          <label class="field"><span>skereru の URL</span><input type="url" v-model="e.baseUrl" placeholder="https://mail.example.com"></label>
          <label class="field"><span>差出人（APIキーの持ち主が送れるアドレス）</span><input type="email" v-model="e.from"></label>
          <label class="field"><span>宛先（1行に1つ）</span><textarea v-model="e.to" rows="3"></textarea></label>
        </template>
        <label class="row"><input type="checkbox" v-model="e.all"> すべての監視について送る</label>
        <div v-if="!e.all.v" class="checks">
          <label v-for="m in Store.monitors" class="row small"><input type="checkbox" :checked="on(e, m.id).v" @change="toggle(e, m.id)"> {{ m.name }}</label>
        </div>
        <label class="row"><input type="checkbox" v-model="e.enabled"> 送る（外すと止める）</label>
        <p v-if="e.error.mapC(_.nonEmpty)" class="error">{{ e.error }}</p>
        <div class="actions"><button type="button" @click="s.edit := None">やめる</button><button type="submit" class="primary">保存</button></div>
      </form>
    </Modal>
  </div>
</template>

<style scoped>
.head { align-items: center; }
.checks { max-height: 14rem; overflow: auto; border: 1px solid var(--line); border-radius: 8px; padding: .4rem .6rem; margin: .3rem 0 .6rem; }
</style>
