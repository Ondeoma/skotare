<script lang="scala">
import skotare.web.*
import skotare.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// 監視の作成・編集. 種類（HTTP(S)・TCP・DNS・PING）ごとに入れる項目が変わる. 「試す」で保存せずに確かめられる
case class Props(monitorId: Option[UUID])

case class State(name: RV[String] = RV(""), kind: RV[String] = RV(Kind.Http), description: RV[String] = RV(""), enabled: RV[Boolean] = RV(true),
                 interval: RV[String] = RV("60"), timeout: RV[String] = RV("10000"), retries: RV[String] = RV("1"),
                 canary: RV[Boolean] = RV(false), confirm: RV[String] = RV("0"),
                 url: RV[String] = RV("https://"), method: RV[String] = RV("GET"), headers: RV[String] = RV(""), body: RV[String] = RV(""),
                 expectStatus: RV[String] = RV("200-299"), bodyContains: RV[String] = RV(""), bodyNotContains: RV[String] = RV(""),
                 bodyRegex: RV[String] = RV(""), followRedirects: RV[Boolean] = RV(true), certDays: RV[String] = RV("0"),
                 host: RV[String] = RV(""), port: RV[String] = RV("443"), recordType: RV[String] = RV("A"), dnsServer: RV[String] = RV(""),
                 expect: RV[String] = RV(""), test: RV[Option[ProbeResultDto]] = RV(None), testing: RV[Boolean] = RV(false),
                 error: RV[String] = RV(""), advanced: RV[Boolean] = RV(false))

def initState() = State()

def mounted(s: State, props: Props): Unit = {
  pageTitle(if (props.monitorId.isDefined) "監視の編集" else "監視を追加")
  props.monitorId.foreach(id => Store.report(Api.monitor(id))(d => fill(s, d.monitor)))
}

def fill(s: State, m: MonitorDto): Unit = {
  val c = m.config
  s.name := m.name; s.kind := m.kind; s.description := m.description; s.enabled := m.enabled
  s.interval := m.intervalSec.toString; s.timeout := m.timeoutMs.toString; s.retries := m.retries.toString
  s.canary := m.canary; s.confirm := m.confirmRegions.toString
  s.url := c.url; s.method := c.method; s.headers := c.headers.map(h => s"${h.name}: ${h.value}").mkString("\n"); s.body := c.body
  s.expectStatus := c.expectStatus; s.bodyContains := c.bodyContains; s.bodyNotContains := c.bodyNotContains; s.bodyRegex := c.bodyRegex
  s.followRedirects := c.followRedirects; s.certDays := c.certExpiryDays.toString
  s.host := c.host; s.port := (if (c.port > 0) c.port.toString else "443"); s.recordType := c.recordType; s.dnsServer := c.dnsServer; s.expect := c.expect
  s.advanced := c.headers.nonEmpty || c.body.nonEmpty || c.bodyRegex.nonEmpty || c.bodyNotContains.nonEmpty || !c.followRedirects
}

/** ヘッダーは1行に「名前: 値」. */
def headers(s: State): List[KeyValue] = s.headers.v.split('\n').toList.map(_.trim).filter(_.nonEmpty).map { l =>
  val i = l.indexOf(':')
  if (i < 0) KeyValue(l, "") else KeyValue(l.take(i).trim, l.drop(i + 1).trim)
}

def req(s: State): MonitorReq = MonitorReq(s.name.v.trim, s.kind.v,
  MonitorConfig(s.url.v.trim, s.method.v, headers(s), s.body.v, s.expectStatus.v, s.bodyContains.v, s.bodyNotContains.v, s.bodyRegex.v,
    s.followRedirects.v, s.certDays.v.toIntOption.getOrElse(0), s.host.v.trim, s.port.v.toIntOption.getOrElse(0), s.recordType.v,
    s.dnsServer.v.trim, s.expect.v.trim),
  s.interval.v.toIntOption.getOrElse(60), s.timeout.v.toIntOption.getOrElse(10000), s.retries.v.toIntOption.getOrElse(1), s.enabled.v, s.description.v,
  s.canary.v, s.confirm.v.toIntOption.getOrElse(0))

/** 名前が空なら、相手から付ける（試すときにも名前が要るため）. */
def withName(s: State): MonitorReq = {
  val r = req(s)
  if (r.name.nonEmpty) r else r.copy(name = r.config.target(r.kind).replace("GET ", "").take(100))
}

def check(s: State): Option[String] = MonitorCheck.validate(s.kind.v, req(s).config).left.toOption

def runTest(s: State): Unit = check(s) match {
  case Some(e) => s.error := e
  case None =>
    s.error := ""; s.testing := true; s.test := None
    Api.testMonitor(withName(s)).onComplete { r =>
      s.testing := false
      r match {
        case Success(t) => s.test := Some(t)
        case Failure(e) => s.error := e.getMessage
      }
    }
}

def save(s: State, props: Props): Unit = check(s) match {
  case Some(e) => s.error := e
  case None =>
    s.error := ""
    Api.saveMonitor(props.monitorId, withName(s)).onComplete {
      case Success(m) => Store.toast("保存しました"); Store.refresh(); Store.go(s"/monitors/${m.id}")
      case Failure(e) => s.error := e.getMessage
    }
}

def setKind(s: State, k: String): Unit = {
  s.kind := k; s.test := None
  if (k == Kind.Tcp && s.port.v.isEmpty) s.port := "443"
  // URL を入れてからほかの種類にしたら、ホスト名を引き継ぐ
  if (k != Kind.Http && s.host.v.isEmpty) s.host := s.url.v.replaceFirst("^https?://", "").takeWhile(c => c != '/' && c != ':' && c != '?')
}
def is(s: State, k: String) = s.kind.mapC(_ == k)
def kindClass(s: State, k: String) = s.kind.mapC(c => if (c == k) "seg active" else "seg")
def hasBody(s: State) = s.method.mapC(m => m != "GET" && m != "HEAD")
def testClass(t: ProbeResultDto): String = if (t.ok) "panel result ok" else "panel result ng"
def testText(t: ProbeResultDto): String = (if (t.ok) "✓ 成功" else "✗ 失敗") + t.latencyMs.fold("")(ms => s"（${ms}ms）") + ": " + t.message
def tests(s: State) = s.test.mapC(_.toList)
def hostLabel(s: State) = s.kind.mapC(k => if (k == Kind.Dns) "引く名前（ドメイン）" else "ホスト名か IP アドレス")

val intervals = List("20" -> "20秒", "30" -> "30秒", "60" -> "1分", "120" -> "2分", "300" -> "5分", "600" -> "10分", "1800" -> "30分", "3600" -> "1時間")
val confirms: List[(String, String)] = ConfirmRegions.choices.map((n, l) => n.toString -> l)
</script>

<template>
  <div class="page narrow">
    <p class="small"><a href="/" @click.prevent='Store.go("/")'>← 監視</a></p>
    <h1>{{ if (props.monitorId.isDefined) "監視の編集" else "監視を追加" }}</h1>
    <form class="panel" @submit.prevent="save(s, props)">
      <div class="field"><span>種類</span>
        <div class="segs">
          <button v-for="k in Kind.all" type="button" :class="kindClass(s, k)" @click="setKind(s, k)">{{ Kind.label(k) }}</button>
        </div>
      </div>
      <label class="field"><span>名前（省くと相手の名前）</span><input type="text" v-model="s.name" maxlength="100" placeholder="会社のサイト"></label>

      <template v-if="is(s, Kind.Http)">
        <div class="row">
          <label class="field"><span>メソッド</span><select v-model="s.method"><option v-for="m in MonitorCheck.Methods" :value="m">{{ m }}</option></select></label>
          <label class="field grow"><span>URL</span><input type="url" v-model="s.url" placeholder="https://example.com/health" required></label>
        </div>
        <div class="row">
          <label class="field"><span>期待する応答コード</span><input type="text" v-model="s.expectStatus" placeholder="200-299,301"></label>
          <label class="field grow"><span>本文に含まれるべき文字（任意）</span><input type="text" v-model="s.bodyContains" placeholder="ok"></label>
        </div>
        <label class="field"><span>証明書の期限の警告（残りの日数がこれを切ったら失敗. 0 で見ない）</span><input type="number" v-model="s.certDays" min="0" max="365"></label>
        <p><a href="#" class="small" @click.prevent="s.advanced := !s.advanced.v">{{ if (s.advanced.v) "▾ 詳しい設定を閉じる" else "▸ 詳しい設定（ヘッダー・本文・正規表現・リダイレクト）" }}</a></p>
        <template v-if="s.advanced">
          <label class="field"><span>本文に含まれてはいけない文字</span><input type="text" v-model="s.bodyNotContains" placeholder="error"></label>
          <label class="field"><span>本文が合うべき正規表現</span><input type="text" v-model="s.bodyRegex" placeholder="&quot;status&quot;:\s*&quot;ok&quot;"></label>
          <label class="field"><span>送るヘッダー（1行に「名前: 値」）</span><textarea v-model="s.headers" rows="3" placeholder="Authorization: Bearer …"></textarea></label>
          <label v-if="hasBody(s)" class="field"><span>送る本文</span><textarea v-model="s.body" rows="4"></textarea></label>
          <label class="row"><input type="checkbox" v-model="s.followRedirects"> リダイレクトをたどる（たどらないなら 3xx が応答コードになる）</label>
        </template>
      </template>

      <template v-else>
        <div class="row">
          <label class="field grow"><span>{{ hostLabel(s) }}</span><input type="text" v-model="s.host" placeholder="example.com" required></label>
          <label v-if="is(s, Kind.Tcp)" class="field"><span>ポート</span><input type="number" v-model="s.port" min="1" max="65535"></label>
          <label v-if="is(s, Kind.Dns)" class="field"><span>レコード</span><select v-model="s.recordType"><option v-for="t in MonitorCheck.RecordTypes" :value="t">{{ t }}</option></select></label>
        </div>
        <template v-if="is(s, Kind.Dns)">
          <div class="row">
            <label class="field grow"><span>答えに含まれるべき値（任意）</span><input type="text" v-model="s.expect" placeholder="203.0.113.10"></label>
            <label class="field grow"><span>DNS サーバー（任意. 省くとシステムの設定）</span><input type="text" v-model="s.dnsServer" placeholder="1.1.1.1"></label>
          </div>
        </template>
      </template>

      <div class="row">
        <label class="field"><span>確認の間隔</span><select v-model="s.interval"><option v-for="i in intervals" :value="i._1">{{ i._2 }}</option></select></label>
        <label class="field"><span>タイムアウト（ミリ秒）</span><input type="number" v-model="s.timeout" min="500" max="60000" step="500"></label>
        <label class="field"><span>停止と判断するまでの再試行</span><input type="number" v-model="s.retries" min="0" max="10"></label>
        <label class="field"><span>停止と判断する拠点の数</span><select v-model="s.confirm"><option v-for="c in confirms" :value="c._1">{{ c._2 }}</option></select></label>
      </div>
      <label class="row"><input type="checkbox" v-model="s.canary"> 基準点にする（拠点そのものが外へ届くかの判定に使う. 過半数の基準点に届かない拠点は、{{ AppNames.self }} 側の異常の疑いとして判定から外す）</label>
      <label class="field"><span>説明（任意）</span><input type="text" v-model="s.description" maxlength="1000"></label>
      <label class="row"><input type="checkbox" v-model="s.enabled"> 確かめる（外すと一時停止）</label>

      <div v-for="t in tests(s)" :class="testClass(t)">{{ testText(t) }}</div>
      <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
      <div class="row actions">
        <button type="button" @click="runTest(s)" :disabled="s.testing.v">{{ if (s.testing.v) "確かめています…" else "試す" }}</button>
        <span class="grow"></span>
        <button type="submit" class="primary">保存</button>
      </div>
    </form>
  </div>
</template>

<style scoped>
.segs { display: inline-flex; width: fit-content; border: 1px solid var(--line); border-radius: 999px; overflow: hidden; }
.seg { border: none; border-radius: 0; padding: .4rem 1rem; background: transparent; }
.seg + .seg { border-left: 1px solid var(--line); }
.seg.active { background: var(--accent-weak); color: var(--accent); font-weight: 600; }
.row > .field { margin-bottom: .6rem; }
.result { margin: .6rem 0; }
.result.ok { border-color: var(--up); }
.result.ng { border-color: var(--down); }
.actions { margin-top: .8rem; align-items: center; }
</style>
