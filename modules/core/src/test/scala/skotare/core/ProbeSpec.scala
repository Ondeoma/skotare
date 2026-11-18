package skotare.core

import cats.effect.*
import skotare.shared.*

import java.time.Instant
import scala.concurrent.duration.*

class ProbeSpec extends munit.CatsEffectSuite:

  private val now = Instant.parse("2026-11-17T00:00:00Z")
  private def http(f: MonitorConfig => MonitorConfig) = f(MonitorConfig(url = "https://example.com"))

  test("期待する応答コードの書き方") {
    assertEquals(StatusMatcher.parse("200-299,301"), Right(List(200 -> 299, 301 -> 301)))
    assert(StatusMatcher.parse("").isLeft)
    assert(StatusMatcher.parse("299-200").isLeft)
    assert(StatusMatcher.parse("700").isLeft)
    assert(StatusMatcher.matches(List(200 -> 299, 301 -> 301), 301))
    assert(!StatusMatcher.matches(List(200 -> 299), 302))
  }

  test("HTTP の応答を、コード・本文・正規表現・証明書の期限で確かめる") {
    assert(Probe.checkHttp(http(identity), 200, "", None, now).isRight)
    assertEquals(Probe.checkHttp(http(identity), 500, "", None, now), Left("応答コード 500（期待 200-299）"))
    assert(Probe.checkHttp(http(_.copy(bodyContains = "ok")), 200, "status: ok", None, now).isRight)
    assert(Probe.checkHttp(http(_.copy(bodyContains = "ok")), 200, "ng", None, now).isLeft)
    assert(Probe.checkHttp(http(_.copy(bodyNotContains = "error")), 200, "fatal error", None, now).isLeft)
    assert(Probe.checkHttp(http(_.copy(bodyRegex = "\"status\":\\s*\"ok\"")), 200, """{"status": "ok"}""", None, now).isRight)
    assert(Probe.checkHttp(http(_.copy(bodyRegex = "(")), 200, "", None, now).isLeft)
    val cert = Some(now.plusSeconds(5L * 86400))
    assert(Probe.checkHttp(http(_.copy(certExpiryDays = 14)), 200, "", cert, now).left.exists(_.contains("残り 5 日")))
    assert(Probe.checkHttp(http(_.copy(certExpiryDays = 3)), 200, "", cert, now).isRight)
    assert(Probe.checkHttp(http(_.copy(certExpiryDays = 3)), 200, "", Some(now.minusSeconds(3600)), now).left.exists(_.contains("切れて")))
  }

  test("本文の正規表現: 時間がかかりすぎる・深くなりすぎるものは失敗にする（スレッドを占めない）") {
    val body = "a" * 60000 + "!"
    val t0 = System.nanoTime()
    assert(Probe.regexFind("(a+)+$", body).left.exists(_.contains("時間がかかりすぎ")))
    assert((System.nanoTime() - t0) / 1000000 < 3000, "時間切れで抜ける")
    assert(Probe.regexFind("(a|b)*c", body).isLeft)
    assertEquals(Probe.regexFind("a{3}!", body), Right(true))
    assert(Probe.regexFind("(", body).left.exists(_.contains("正しくありません")))
  }

  test("一覧・通知・ログに出す相手から、URL の利用者名・パスワードとクエリを除く") {
    assertEquals(MonitorConfig.shownUrl("https://user:pass@example.com/health?token=abc"), "https://example.com/health?…")
    assertEquals(MonitorConfig.shownUrl("https://example.com/a@b"), "https://example.com/a@b")
    assertEquals(MonitorConfig(url = "http://u@h:8080/x#f").target(Kind.Http), "GET http://h:8080/x?…")
  }

  test("DNS の答えを期待する値と照らし合わせる（末尾の . と大文字・小文字は見ない）") {
    val c = MonitorConfig(host = "example.com", recordType = "MX", expect = "Mail.Example.com")
    assert(Probe.checkDns(c, List("10 mail.example.com.")).isRight)
    assert(Probe.checkDns(c, List("10 other.example.net.")).isLeft)
    assert(Probe.checkDns(c.copy(expect = ""), Nil).isLeft)
  }

  test("入力の確かめ: ホスト名にコマンドを混ぜられない") {
    assert(MonitorCheck.validate(Kind.Ping, MonitorConfig(host = "example.com")).isRight)
    assert(MonitorCheck.validate(Kind.Ping, MonitorConfig(host = "2001:db8::1")).isRight)
    assert(MonitorCheck.validate(Kind.Ping, MonitorConfig(host = "-c 1000 example.com")).isLeft)
    assert(MonitorCheck.validate(Kind.Ping, MonitorConfig(host = "example.com;reboot")).isLeft)
    assert(MonitorCheck.validate(Kind.Http, MonitorConfig(url = "https://x", headers = List(KeyValue("X-A", "1\r\nEvil: 1")))).isLeft)
    assertEquals(MonitorCheck.validate(Kind.Dns, MonitorConfig(host = "example.com", recordType = "aaaa")).map(_.recordType), Right("AAAA"))
  }

  test("TCP: 開いているポートは成功、閉じているポートは失敗") {
    Resource.make(IO.blocking(new java.net.ServerSocket(0)))(s => IO.blocking(s.close())).use { s =>
      for
        ok <- Probe.run(Kind.Tcp, MonitorConfig(host = "127.0.0.1", port = s.getLocalPort), 2.seconds)
        _   = assert(ok.ok, ok.message)
        ng <- Probe.run(Kind.Tcp, MonitorConfig(host = "127.0.0.1", port = 1), 2.seconds)
        _   = assert(!ng.ok)
      yield ()
    }
  }

  test("API の要求は、省いた項目を既定の値で読む") {
    val r = io.circe.parser.decode[MonitorReq]("""{"name": "x", "kind": "tcp", "config": {"host": "h", "port": 1}}""")
    assertEquals(r.map(x => (x.intervalSec, x.timeoutMs, x.retries, x.enabled, x.config.expectStatus)), Right((60, 10000, 1, true, "200-299")))
    assertEquals(io.circe.parser.decode[StatusPageReq]("""{"slug": "a1", "title": "t"}""").map(_.public), Right(true))
  }
