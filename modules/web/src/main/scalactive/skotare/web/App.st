<script lang="scala">
import io.github.ondeoma.scalactive.components.routes.RouterComponent
import org.scalajs.dom
import skotare.web.parts.*
import skotare.web.views.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

// 画面全体の枠: ヘッダー（ナビゲーション・メニュー）と、ルートごとの画面. ステータスページ（/s/…）はゲストにも見せるので枠を出さない
case class State(ready: RV[Boolean] = RV(false), menu: RV[Boolean] = RV(false), nav: RV[Boolean] = RV(false))

def initState() = State()

val routes: PathPatternRoutes = List(
  ("/", _ => DashboardView()),
  ("/monitors/new", _ => MonitorEditView(monitorId = None)),
  ("/monitors/:id", r => MonitorView(monitorId = UUID.fromString(r.pathname.groups("id")))),
  ("/monitors/:id/edit", r => MonitorEditView(monitorId = Some(UUID.fromString(r.pathname.groups("id"))))),
  ("/incidents", _ => IncidentsView()),
  ("/notifiers", _ => NotifiersView()),
  ("/pages", _ => PagesView()),
  ("/s/:slug", r => StatusPageView(slug = js.URIUtils.decodeURIComponent(r.pathname.groups("slug")))),
  ("/login", _ => LoginView()),
  ("/settings", _ => SettingsView()),
)

def isStatusPage: Boolean = dom.window.location.pathname.startsWith("/s/")

def mounted(s: State): Unit = {
  Fallback.install(routes)
  Api.clientConfig().foreach(c => Store.config := Some(c))
  val path = dom.window.location.pathname
  if (path.startsWith("/login") || isStatusPage) s.ready := true
  else Toasts.report(Api.me()) { me =>
    Store.me := Some(me)
    AppNames.set(me.appNames)
    Observe.setUser(me.id.toString)
    Store.start()
    s.ready := true
  }
}

// ログアウトするときは、この端末のプッシュ通知もやめる（ログアウトした端末に監視の様子を送らない）
def logout(): Unit = Toasts.report(Pwa.disable().recover { case _ => "off" }.flatMap(_ => Api.logout()))(url => dom.window.location.href = url)

def loggedIn = computed(Store.me.v.isDefined && !Router.href.v.contains("/s/"))
def meBrief = Store.me.mapC(_.map(_.brief.person))
def userName = Store.me.mapC(_.map(_.name).getOrElse(""))
def userEmail = Store.me.mapC(_.flatMap(_.email).getOrElse(""))
def loadingClass = Store.loading.mapC(l => if (l) "progress on" else "progress")
def downCount = Store.summary.mapC(_.map(_.down).getOrElse(0))
def hasDown = downCount.mapC(_ > 0)

val nav: List[(String, String)] = List("/" -> "監視", "/incidents" -> "障害", "/pages" -> "ステータスページ", "/notifiers" -> "通知先")
def navClass(p: String) = Router.href.mapC { h =>
  val path = new dom.URL(h).pathname
  if (path == p || (p == "/" && path.startsWith("/monitors")) || (p != "/" && path.startsWith(p))) "nav active" else "nav"
}
</script>

<template>
  <header v-if="loggedIn" class="top">
    <button class="ghost icon only-sp" title="メニュー" @click="s.nav := !s.nav.v">☰</button>
    <RouterLink to="/" class="logo"><span class="brand"><img :src='AppNames.logo("/logo.svg").v' alt="" height="32" class="brand-logo"><span class="hide-sp">{{ AppNames.self }}</span></span></RouterLink>
    <nav class="navs hide-sp">
      <RouterLink v-for="n in nav" :to="n._1" :class="navClass(n._1)">{{ n._2 }}<span v-if='n._1 == "/incidents" && hasDown.v' class="badge">{{ downCount }}</span></RouterLink>
    </nav>
    <div class="spacer"></div>
    <div class="me">
      <button class="ghost me-btn" @click="s.menu := !s.menu.v"><Avatar :user="meBrief.v" :size="30" /></button>
      <div v-if="s.menu" class="menu-bg" @click="s.menu := false"></div>
      <div v-if="s.menu" class="menu" @click="s.menu := false">
        <div class="who"><b>{{ userName }}</b><span class="muted small">{{ userEmail }}</span></div>
        <RouterLink to="/settings">設定</RouterLink>
        <a href="#" @click.prevent="logout()">ログアウト</a>
      </div>
    </div>
    <div :class="loadingClass"></div>
  </header>
  <div v-if="s.nav" class="sp-nav only-sp" @click="s.nav := false">
    <RouterLink v-for="n in nav" :to="n._1" :class="navClass(n._1)">{{ n._2 }}</RouterLink>
  </div>
  <main class="main">
    <RouterComponent v-if="s.ready" :routes="routes" :reloadOnQueryChange="false" />
  </main>
  <DialogHost />
  <ToastHost />
</template>

<style scoped>
.top { position: sticky; top: 0; z-index: 100; height: var(--header); display: flex; align-items: center; gap: .5rem; padding: 0 .8rem;
  background: var(--bg); border-bottom: 1px solid var(--line); }
.brand { font-weight: 700; font-size: 1.25rem; color: var(--fg); display: inline-flex; align-items: center; gap: .5rem; white-space: nowrap; margin-right: 1rem; }
.brand img { display: block; width: auto; }
.icon { font-size: 1.2rem; padding: .3rem .55rem; }
.spacer { flex: 1; }
.me { position: relative; }
.me-btn { padding: .15rem; border-radius: 999px; }
.menu { position: absolute; right: 0; top: 2.8rem; background: var(--surface); border: 1px solid var(--line); border-radius: 10px; box-shadow: var(--shadow);
  min-width: 220px; padding: .3rem; display: flex; flex-direction: column; z-index: 200; }
.menu-bg { position: fixed; inset: 0; z-index: 199; }
.who { display: flex; flex-direction: column; padding: .5rem .7rem; border-bottom: 1px solid var(--line); margin-bottom: .3rem; }
.progress { position: absolute; left: 0; bottom: -1px; height: 2px; width: 0; background: var(--accent); opacity: 0; transition: opacity .2s; }
.progress.on { width: 100%; opacity: .7; animation: prog 1.2s ease-in-out infinite; }
@keyframes prog { from { transform: scaleX(0); transform-origin: left; } to { transform: scaleX(1); transform-origin: left; } }
.sp-nav { position: fixed; z-index: 150; top: var(--header); left: 0; right: 0; background: var(--surface); border-bottom: 1px solid var(--line);
  display: flex; flex-direction: column; padding: .4rem; box-shadow: var(--shadow); }
</style>
