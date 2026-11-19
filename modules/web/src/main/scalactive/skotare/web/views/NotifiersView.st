<script lang="scala">
import skotare.web.*
import skotare.web.parts.*
import skotare.shared.*
import org.scalajs.dom

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// 通知先: 監視が停止・復旧したときに送る先（stui のチャンネル・skereru のメール・Webhook）
// stui は、stui と連携して参加しているチャンネルから選ぶ（mode = "channel"）か、受信 Webhook の URL を入れる（mode = "url"）
case class Edit(id: Option[UUID], name: RV[String], kind: RV[String], secret: RV[String], baseUrl: RV[String], from: RV[String], to: RV[String],
                all: RV[Boolean], monitors: RV[List[UUID]], enabled: RV[Boolean], hasSecret: Boolean, error: RV[String] = RV(""),
                mode: RV[String] = RV("url"), channelId: RV[String] = RV(""), hook: RV[Boolean] = RV(false), current: NotifierConfig = NotifierConfig())

case class State(list: RV[List[NotifierDto]] = RV(Nil), edit: RV[Option[Edit]] = RV(None),
                 stui: RV[Option[IntegrationDto]] = RV(None), channels: RV[List[StuiChannelDto]] = RV(Nil), channelError: RV[String] = RV(""))

def initState() = State()

def load(s: State): Unit = Store.report(Api.notifiers())(s.list := _)

def stuiOn = Store.config.mapC(_.exists(_.stui))

/** stui との連携の状態と、選べるチャンネル. */
def loadStui(s: State): Unit = Store.report(Api.integrations()) { is =>
  s.stui := is.find(_.provider == "stui")
  s.channelError := ""
  if (s.stui.v.exists(_.connected)) Api.stuiChannels().onComplete {
    case Success(cs) => s.channels := cs
    case Failure(x)  => s.channels := Nil; s.channelError := x.getMessage; Store.report(Api.integrations())(is => s.stui := is.find(_.provider == "stui"))
  }
}

def mounted(s: State): Unit = {
  pageTitle("通知先")
  load(s)
  loadStui(s)
  // stui の許可画面から戻ったとき: 結果を知らせ、連携できたらチャンネルを選ぶ画面を開く
  Query.get("integration").filter(_ == "stui").foreach { _ =>
    Query.get("result") match {
      case Some("connected") => Store.toast(s"${AppNames.nameOf("stui")} と連携しました"); open(s, None, channel = true)
      case Some("denied")    => Store.toast(s"${AppNames.nameOf("stui")} との連携をやめました")
      case _                 => Store.toast(s"${AppNames.nameOf("stui")} と連携できませんでした。もう一度試してください")
    }
    dom.window.history.replaceState(null, "", dom.window.location.pathname)
  }
}

def open(s: State, n: Option[NotifierDto], channel: Boolean = false): Unit = s.edit := Some(n match {
  case Some(n) => Edit(Some(n.id), RV(n.name), RV(n.kind), RV(""), RV(n.config.baseUrl), RV(n.config.from), RV(n.config.to.mkString("\n")),
      RV(n.allMonitors), RV(n.monitorIds), RV(n.enabled), n.hasSecret, mode = RV(if (n.config.channelId.isDefined) "channel" else "url"),
      channelId = RV(n.config.channelId.fold("")(_.toString)), hook = RV(n.config.stuiHook), current = n.config)
  case None => Edit(None, RV(""), RV(NotifierKind.Stui), RV(""), RV(""), RV(""), RV(""), RV(true), RV(Nil), RV(true), false,
      mode = RV(if (channel || stuiOn.v) "channel" else "url"))
})

def channelMode(e: Edit) = computed(e.kind.v == NotifierKind.Stui && e.mode.v == "channel")

def save(s: State, e: Edit): Unit = {
  val ch = if (channelMode(e).v) scala.util.Try(UUID.fromString(e.channelId.v)).toOption else None
  if (channelMode(e).v && ch.isEmpty) e.error := "通知するチャンネルを選んでください"
  else {
    val name = Option(e.name.v.trim).filter(_.nonEmpty)
      .orElse(ch.flatMap(id => s.channels.v.find(_.id == id)).map(c => s"${c.name}（${AppNames.nameOf("stui")}）")).getOrElse("")
    val cfg = if (ch.isDefined) NotifierConfig(channelId = ch)
              else NotifierConfig(e.baseUrl.v.trim, e.from.v.trim, e.to.v.split(Array('\n', ',')).toList.map(_.trim).filter(_.nonEmpty))
    val r = NotifierReq(name, e.kind.v, cfg, if (ch.isDefined) None else Option(e.secret.v.trim).filter(_.nonEmpty), e.all.v, e.monitors.v, e.enabled.v,
      hook = ch.isDefined && e.hook.v && canHook(s, e).v)
    Api.saveNotifier(e.id, r).onComplete {
      case Success(_) => Store.toast("保存しました"); s.edit := None; load(s)
      case Failure(x) => e.error := x.getMessage
    }
  }
}

/** stui の許可画面へ（戻ると、この画面でチャンネルを選べる）. */
def connect(): Unit = dom.window.location.href = "/api/integrations/stui/connect?returnTo=/notifiers"

def disconnect(s: State): Unit =
  Dialog.confirm(s"${AppNames.nameOf("stui")} との連携をやめますか？ あなたの連携で投稿している通知先（${AppNames.nameOf("stui")} のチャンネル）には、送れなくなります。", "連携をやめる") {
    Store.report(Api.disconnectStui()) { _ => Store.toast(s"${AppNames.nameOf("stui")} との連携をやめました"); s.channels := Nil; loadStui(s) }
  }

def test(n: NotifierDto): Unit = Store.report(Api.testNotifier(n.id))(_ => Store.toast(s"「${n.name}」に試しに送りました"))

def remove(s: State, n: NotifierDto): Unit = Dialog.confirm(s"通知先「${n.name}」を削除しますか？", "削除する") {
  Store.report(Api.deleteNotifier(n.id)) { _ => Store.toast("削除しました"); load(s) }
}

def toggle(e: Edit, id: UUID): Unit = e.monitors := (if (e.monitors.v.contains(id)) e.monitors.v.filterNot(_ == id) else e.monitors.v :+ id)
def on(e: Edit, id: UUID) = e.monitors.mapC(_.contains(id))
def edits(s: State) = s.edit.mapC(_.toList)
def isStui(e: Edit) = e.kind.mapC(_ == NotifierKind.Stui)
def isSkereru(e: Edit) = e.kind.mapC(_ == NotifierKind.Skereru)
def connected(s: State) = s.stui.mapC(_.exists(_.connected))
def secretLabel(e: Edit) = e.kind.mapC(k => NotifierKind.secretLabel(k, AppNames.nameOf) + (if (e.hasSecret) "（変えるときだけ入れる）" else ""))
def secretHint(e: Edit) = e.kind.mapC {
  case NotifierKind.Stui    => AppNames.nameOf("stui") + " のチャンネルの設定 →「受信 Webhook」で作った URL（https://…/hooks/…）"
  case NotifierKind.Skereru => s"${AppNames.nameOf("skereru")} の「設定 → APIキー・MCP」で、send のスコープを付けて発行したキー（skr_…）"
  case _                    => "停止・復旧のたびに JSON（event・monitor・message・at・url）を POST します"
}

/** 前に選んだチャンネルが、自分の選べるチャンネルにないとき（ほかの人の連携で投稿している・抜けたなど）も、選択肢に残す. */
def channelOptions(s: State, e: Edit) = computed {
  val cs = s.channels.v.map(c => c.id.toString -> s"${if (c.kind == "private") "🔒" else "#"} ${c.name}")
  e.current.channelId.filterNot(id => s.channels.v.exists(_.id == id)).map(id => id.toString -> s"# ${e.current.channelName}（前に選んだもの）").toList ++ cs
}

/**
 * 選んだチャンネルに受信 Webhook を作れるか（自分がチャンネルの管理者）. 前に作った Webhook のチャンネルのままなら、管理者でなくても使い続けられる.
 */
def canHook(s: State, e: Edit) = computed {
  s.channels.v.exists(c => c.id.toString == e.channelId.v && c.admin) || sameHook(e).v
}
def sameHook(e: Edit) = computed(e.current.stuiHook && e.current.channelId.exists(_.toString == e.channelId.v))

/** ほかの人の連携で投稿している通知先か. */
def otherOwner(e: Edit) = computed {
  !e.hook.v && e.current.connectedBy.exists(o => !Store.me.v.exists(_.id == o)) && e.current.channelId.exists(_.toString == e.channelId.v)
}

def kindLabel(n: NotifierDto): String =
  if (n.kind == NotifierKind.Stui && n.config.stuiHook) s"${AppNames.nameOf("stui")} #${n.config.channelName}（受信 Webhook）"
  else if (n.kind == NotifierKind.Stui && n.config.stuiChannel) s"${AppNames.nameOf("stui")} #${n.config.channelName}" else NotifierKind.label(n.kind, AppNames.nameOf)
def scope(n: NotifierDto): String = if (n.allMonitors) "すべての監視" else s"${n.monitorIds.size} 件の監視"
def empty(s: State) = s.list.mapC(_.isEmpty)
</script>

<template>
  <div class="page">
    <div class="row head"><h1 class="grow">通知先</h1><button class="primary" @click="open(s, None)">＋ 通知先を追加</button></div>
    <p class="muted small">監視が停止・復旧したときに送ります。送れなかったときは間隔を延ばして送り直します。プッシュ通知は「設定」から端末ごとに受け取れます。</p>
    <p v-if="stuiOn.v" class="row small link-state">
      <template v-if="connected(s).v">
        <span class="grow">{{ AppNames.of("stui") }} と連携しています。通知先に、参加しているチャンネルを選べます。</span>
        <button type="button" class="small ghost" @click="disconnect(s)">連携をやめる</button>
      </template>
      <template v-else>
        <span class="grow muted">{{ AppNames.of("stui") }} と連携すると、通知先に {{ AppNames.of("stui") }} のチャンネルを一覧から選べます。</span>
        <button type="button" class="small" @click="connect()">{{ AppNames.of("stui") }} と連携する</button>
      </template>
    </p>
    <p v-if="empty(s)" class="panel muted">まだ通知先がありません。</p>
    <table v-else class="list">
      <thead><tr><th>名前</th><th>種類</th><th class="hide-sp">対象</th><th></th></tr></thead>
      <tbody><tr v-for="n in s.list">
        <td><b>{{ n.name }}</b><span v-if="!n.enabled" class="muted small">（止めています）</span></td>
        <td>{{ kindLabel(n) }}</td>
        <td class="hide-sp small">{{ scope(n) }}</td>
        <td class="right nowrap"><button class="small" @click="test(n)">試しに送る</button>
          <button class="small" @click="open(s, Some(n))">編集</button><button class="small ghost danger" @click="remove(s, n)">削除</button></td>
      </tr></tbody>
    </table>
    <Modal v-for="e in edits(s)" :onClose="_ => s.edit := None" :wide="true">
      <h2>{{ if (e.id.isDefined) "通知先の編集" else "通知先を追加" }}</h2>
      <form @submit.prevent="save(s, e)">
        <label class="field"><span>名前</span><input type="text" v-model="e.name" maxlength="100" placeholder="運用チャンネル"></label>
        <label class="field"><span>種類</span><select v-model="e.kind"><option v-for="k in NotifierKind.all" :value="k">{{ NotifierKind.label(k, AppNames.nameOf) }}</option></select></label>
        <div v-if="isStui(e).v && stuiOn.v" class="row modes">
          <label class="row small"><input type="radio" name="mode" value="channel" :checked='e.mode.v == "channel"' @change='e.mode := "channel"'> チャンネルを選ぶ（{{ AppNames.of("stui") }} と連携）</label>
          <label class="row small"><input type="radio" name="mode" value="url" :checked='e.mode.v == "url"' @change='e.mode := "url"'> 受信 Webhook の URL を入れる</label>
        </div>
        <template v-if="channelMode(e).v">
          <template v-if="connected(s).v">
            <label class="field"><span>通知するチャンネル</span>
              <select v-model="e.channelId">
                <option value="">（選ぶ）</option>
                <option v-for="o in channelOptions(s, e)" :value="o._1">{{ o._2 }}</option>
              </select>
            </label>
            <div v-if="canHook(s, e).v" class="hook-modes">
              <label class="row small"><input type="radio" name="via" :checked="!e.hook.v" @change="e.hook := false"> あなたの連携（トークン）で投稿する</label>
              <label class="row small"><input type="radio" name="via" :checked="e.hook.v" @change="e.hook := true"> 受信 Webhook を作って送る（あなたがチャンネルの管理者なので選べます）</label>
            </div>
            <p v-if="canHook(s, e).v && e.hook.v" class="muted small">
              {{ if (sameHook(e).v) "前に作った受信 Webhook で送っています。" else "保存すると、このチャンネルに受信 Webhook（" + AppNames.self.v + "）を作り、その URL で送ります。" }}
              作ったあとは、あなたが連携をやめても届きます（Webhook は {{ AppNames.of("stui") }} のチャンネルの設定で確かめられます）。
            </p>
            <p v-else class="muted small">{{ AppNames.of("stui") }} のボット「{{ AppNames.self }}」として、このチャンネルに投稿します。あなたが参加しているチャンネルだけを選べます。</p>
            <p v-if="otherOwner(e).v" class="muted small">この通知先は、ほかの人の {{ AppNames.of("stui") }} との連携で投稿しています。チャンネルを変えると、あなたの連携で投稿するようになります。</p>
          </template>
          <template v-else>
            <p class="small">チャンネルを選ぶには、{{ AppNames.of("stui") }} と連携してください。</p>
            <p><button type="button" @click="connect()">{{ AppNames.of("stui") }} と連携する</button></p>
            <p v-if="e.current.channelId.isDefined" class="muted small">今は {{ AppNames.of("stui") }} #{{ e.current.channelName }} に投稿しています。監視の選択などだけなら、連携しなくても変えられます。</p>
          </template>
          <p v-if="s.channelError.mapC(_.nonEmpty)" class="error">{{ s.channelError }}</p>
        </template>
        <template v-else>
          <label class="field"><span>{{ secretLabel(e) }}</span><input type="password" v-model="e.secret" autocomplete="off"></label>
          <p class="muted small">{{ secretHint(e) }}</p>
        </template>
        <template v-if="isSkereru(e)">
          <label class="field"><span>{{ AppNames.of("skereru") }} の URL</span><input type="url" v-model="e.baseUrl" placeholder="https://mail.example.com"></label>
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
.link-state { align-items: center; gap: .6rem; border: 1px solid var(--line); border-radius: 8px; padding: .5rem .8rem; }
.modes { gap: 1.2rem; flex-wrap: wrap; margin: -.2rem 0 .6rem; }
.hook-modes { display: flex; flex-direction: column; gap: .3rem; margin: .2rem 0 .4rem; }
.checks { max-height: 14rem; overflow: auto; border: 1px solid var(--line); border-radius: 8px; padding: .4rem .6rem; margin: .3rem 0 .6rem; }
</style>
