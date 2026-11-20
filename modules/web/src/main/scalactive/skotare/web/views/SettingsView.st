<script lang="scala">
import skotare.web.*
import skotare.shared.*
import skotare.web.parts.*

import org.scalajs.dom


// 設定: アイコン・名前（サーバー）、画面の色・プッシュ通知（この端末）、連携しているサービス（OAuth）、API トークン・MCP
case class State(name: RV[String] = RV(Store.me.v.map(_.name).getOrElse("")),
                 theme: RV[String] = RV(Theme.current.v))

def initState() = State()

def mounted(s: State): Unit = {
  pageTitle("設定")
  watch(s.theme)((_, v) => Theme.set(v))
}

def save(s: State): Unit =
  Toasts.report(Api.updateMe(UpdateMeReq(Some(s.name.v.trim), None))) { me =>
    Store.me := Some(me)
    Toasts.show("保存しました")
  }

// ---------------------------------------------------------------- アイコン

def meBrief = Store.me.mapC(_.map(_.brief.person))
def uploaded = Store.me.mapC(_.exists(_.avatarUploaded))

def avatarDone(me: MeDto, msg: String): Unit = {
  Store.me := Some(me)
  Toasts.show(msg)
}

// 中央を正方形に切り抜いて保存する（同じファイルを選び直せるよう、選んだ欄は空に戻る）
def pickAvatar(e: dom.Event): Unit = Images.pickAvatar(e)(Api.uploadAvatar)(me => avatarDone(me, "アイコンを保存しました"))

def removeAvatar(): Unit =
  Dialog.confirm("保存したアイコンを消しますか？（ログインの仕組みから受け取った画像か、名前の1文字目の表示に戻ります）", "消す") {
    Toasts.report(Api.deleteAvatar())(me => avatarDone(me, "アイコンを消しました"))
  }

</script>

<template>
  <div class="page narrow">
    <h1>設定</h1>
    <section class="panel">
      <h2>アカウント</h2>
      <div class="field"><span>アイコン</span>
        <div class="row avatar-row">
          <Avatar :user="meBrief.v" :size="64" />
          <label class="btn">画像を選ぶ<input type="file" accept="image/png,image/jpeg,image/gif,image/webp" class="hidden-file" @change="pickAvatar(event)"></label>
          <button v-if="uploaded" type="button" class="ghost" @click="removeAvatar()">消す</button>
        </div>
        <span class="muted small">中央を正方形に切り抜いて保存します。保存しなければ、ログインの仕組み（IdP）の画像を使います。</span>
      </div>
      <form @submit.prevent="save(s)">
        <label class="field"><span>表示名</span><input type="text" v-model="s.name" maxlength="100"></label>
        <button type="submit" class="primary">保存</button>
      </form>
    </section>
    <section class="panel">
      <h2>表示（この端末）</h2>
      <label class="field"><span>画面の色</span>
        <select v-model="s.theme"><option v-for="t in Theme.options" :value="t._1">{{ t._2 }}</option></select></label>
    </section>
    <section class="panel">
      <h2>プッシュ通知</h2>
      <p class="small">受け取ると、画面を閉じていても、監視が停止・復旧したときにこの端末に届きます。</p>
      <p><PushToggle /></p>
    </section>
    <ConnectedApps />
    <ApiTokenSettings mcpName="skotare" curlPath="/api/monitors" writeLabel="読み書き（監視・通知先・ステータスページの変更もできる）" about="監視の様子を聞いたり監視を作ったりできます。" tools="全体の様子、監視の一覧・詳細・試す・作成・変更・一時停止・再開・今すぐ確かめる・削除、障害の一覧、ステータスページの一覧、任意の REST API の呼び出し。" />
  </div>
</template>

<style scoped>
.avatar-row { align-items: center; gap: .8rem; }
.hidden-file { display: none; }
</style>
