<script lang="scala">
import skotare.web.*
import skotare.web.parts.*
import skotare.shared.*

import org.scalajs.dom
import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// ステータスページ: ゲスト（ログインしない人）に監視の状態を見せるページ（/s/{名前}）. 監視の中身（URL・ホスト）は出さない
case class Edit(id: Option[UUID], slug: RV[String], title: RV[String], description: RV[String], notice: RV[String], public: RV[Boolean],
                items: RV[List[PageItem]], error: RV[String] = RV(""))

case class State(list: RV[List[StatusPageDto]] = RV(Nil), edit: RV[Option[Edit]] = RV(None))

def initState() = State()

def load(s: State): Unit = Store.report(Api.pages())(s.list := _)

def mounted(s: State): Unit = { pageTitle("ステータスページ"); load(s) }

def open(s: State, p: Option[StatusPageDto]): Unit = s.edit := Some(p match {
  case Some(p) => Edit(Some(p.id), RV(p.slug), RV(p.title), RV(p.description), RV(p.notice), RV(p.public), RV(p.items))
  case None    => Edit(None, RV(""), RV(""), RV(""), RV(""), RV(true), RV(Store.monitors.v.map(m => PageItem(m.id, None))))
})

def save(s: State, e: Edit): Unit =
  Api.savePage(e.id, StatusPageReq(e.slug.v.trim, e.title.v.trim, e.description.v, e.notice.v, e.public.v, e.items.v)).onComplete {
    case Success(_) => Store.toast("保存しました"); s.edit := None; load(s)
    case Failure(x) => e.error := x.getMessage
  }

def remove(s: State, p: StatusPageDto): Unit = Dialog.confirm(s"ステータスページ「${p.title}」を削除しますか？（URL は見られなくなります）", "削除する") {
  Store.report(Api.deletePage(p.id)) { _ => Store.toast("削除しました"); load(s) }
}

def url(p: StatusPageDto): String = s"${dom.window.location.origin}/s/${p.slug}"
def toggle(e: Edit, id: UUID): Unit =
  e.items := (if (e.items.v.exists(_.monitorId == id)) e.items.v.filterNot(_.monitorId == id) else e.items.v :+ PageItem(id, None))
def on(e: Edit, id: UUID) = e.items.mapC(_.exists(_.monitorId == id))
/** ページでの並び（上・下へ）. */
def move(e: Edit, id: UUID, d: Int): Unit = {
  val xs = e.items.v
  val i = xs.indexWhere(_.monitorId == id)
  val j = i + d
  if (i >= 0 && j >= 0 && j < xs.size) e.items := xs.updated(i, xs(j)).updated(j, xs(i))
}
def rename(e: Edit, id: UUID, v: String): Unit =
  e.items := e.items.v.map(x => if (x.monitorId == id) x.copy(displayName = Option(v.trim).filter(_.nonEmpty)) else x)
def chosen(e: Edit) = e.items.mapC(_.flatMap(i => Store.monitor(i.monitorId).map(m => (i, m))))
def others(e: Edit) = computed(Store.monitors.v.filterNot(m => e.items.v.exists(_.monitorId == m.id)))
def edits(s: State) = s.edit.mapC(_.toList)
def empty(s: State) = s.list.mapC(_.isEmpty)
</script>

<template>
  <div class="page">
    <div class="row head"><h1 class="grow">ステータスページ</h1><button class="primary" @click="open(s, None)">＋ ページを作る</button></div>
    <p class="muted small">ログインしない人（お客さま・利用者）に、サービスが動いているかを見せるページです。監視の URL・ホストは見せず、名前・状態・稼働率・障害の時間だけを出します。</p>
    <p v-if="empty(s)" class="panel muted">まだページがありません。</p>
    <div v-for="p in s.list" class="panel page-row">
      <div class="grow">
        <b>{{ p.title }}</b> <span v-if="!p.public" class="muted small">（ログインした人だけ）</span>
        <div class="small"><a :href='s"/s/${p.slug}"' target="_blank" rel="noopener">{{ url(p) }}</a> · 監視 {{ p.items.size }} 件</div>
      </div>
      <button class="small" @click='Dom.copy(url(p), "URL をコピーしました")'>URL をコピー</button>
      <button class="small" @click="open(s, Some(p))">編集</button>
      <button class="small ghost danger" @click="remove(s, p)">削除</button>
    </div>
    <Modal v-for="e in edits(s)" :onClose="_ => s.edit := None" :wide="true">
      <h2>{{ if (e.id.isDefined) "ステータスページの編集" else "ステータスページを作る" }}</h2>
      <form @submit.prevent="save(s, e)">
        <label class="field"><span>題名</span><input type="text" v-model="e.title" maxlength="100" placeholder="Example サービスの稼働状況"></label>
        <label class="field"><span>URL の名前（/s/ の後. 小文字の英数字とハイフン）</span><input type="text" v-model="e.slug" placeholder="example" pattern="[a-z0-9][a-z0-9\-]{1,40}"></label>
        <label class="field"><span>説明（任意）</span><input type="text" v-model="e.description"></label>
        <label class="field"><span>お知らせ（任意. メンテナンスの予定など. ページの上に出す）</span><textarea v-model="e.notice" rows="2"></textarea></label>
        <label class="row"><input type="checkbox" v-model="e.public"> ログインしない人にも見せる（外すとログインした人だけ）</label>
        <h3>載せる監視</h3>
        <div class="items">
          <div v-for="im in chosen(e)" class="row item">
            <span class="grow small">{{ im._2.name }}</span>
            <input type="text" class="name" :value="im._1.displayName.mkString" placeholder="ページでの名前（任意）" @change="rename(e, im._1.monitorId, Dom.value(event))">
            <button type="button" class="small ghost" title="上へ" @click="move(e, im._1.monitorId, -1)">↑</button>
            <button type="button" class="small ghost" title="下へ" @click="move(e, im._1.monitorId, 1)">↓</button>
            <button type="button" class="small ghost" title="外す" @click="toggle(e, im._1.monitorId)">×</button>
          </div>
        </div>
        <div class="row add">
          <span class="small muted">加える:</span>
          <button v-for="m in others(e)" type="button" class="small" @click="toggle(e, m.id)">＋ {{ m.name }}</button>
        </div>
        <p v-if="e.error.mapC(_.nonEmpty)" class="error">{{ e.error }}</p>
        <div class="actions"><button type="button" @click="s.edit := None">やめる</button><button type="submit" class="primary">保存</button></div>
      </form>
    </Modal>
  </div>
</template>

<style scoped>
.head { align-items: center; }
.page-row { display: flex; align-items: center; gap: .5rem; flex-wrap: wrap; }
.items { border: 1px solid var(--line); border-radius: 8px; padding: .3rem .5rem; margin-bottom: .5rem; }
.item { align-items: center; padding: .2rem 0; }
.item .name { width: 14rem; }
.add { flex-wrap: wrap; gap: .3rem; align-items: center; }
</style>
