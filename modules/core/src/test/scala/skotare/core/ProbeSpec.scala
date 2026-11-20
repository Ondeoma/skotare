package skotare.core

import cats.effect.*
import skotare.shared.*

import java.time.Instant
import scala.concurrent.duration.*

class ProbeSpec extends munit.CatsEffectSuite {

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
    assert(Probe.checkHttp(
      http(_.copy(bodyRegex = "\"status\":\\s*\"ok\"")),
      200,
      """{"status": "ok"}""",
      None,
      now
    ).isRight)
    assert(Probe.checkHttp(http(_.copy(bodyRegex = "(")), 200, "", None, now).isLeft)
    val cert = Some(now.plusSeconds(5L * 86400))
    assert(Probe.checkHttp(http(_.copy(certExpiryDays = 14)), 200, "", cert, now).left.exists(_.contains("残り 5 日")))
    assert(Probe.checkHttp(http(_.copy(certExpiryDays = 3)), 200, "", cert, now).isRight)
    assert(Probe.checkHttp(
      http(_.copy(certExpiryDays = 3)),
      200,
      "",
      Some(now.minusSeconds(3600)),
      now
    ).left.exists(_.contains("切れて")))
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
    assertEquals(
      MonitorConfig.shownUrl("https://user:pass@example.com/health?token=abc"),
      "https://example.com/health?…"
    )
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
    assert(MonitorCheck.validate(
      Kind.Http,
      MonitorConfig(url = "https://x", headers = List(KeyValue("X-A", "1\r\nEvil: 1")))
    ).isLeft)
    assertEquals(
      MonitorCheck.validate(Kind.Dns, MonitorConfig(host = "example.com", recordType = "aaaa")).map(_.recordType),
      Right("AAAA")
    )
  }

  /** 手元（127.0.0.1）だけ許す（PROBE_ALLOW_INTERNAL=127.0.0.1 に当たる）. */
  private val loopback = sbirds.net.NetGuard.Policy.allowList(List("127.0.0.1")).toOption.get

  test("TCP: 開いているポートは成功、閉じているポートは失敗") {
    Resource.make(IO.blocking(new java.net.ServerSocket(0)))(s => IO.blocking(s.close())).use { s =>
      for {
        ok <- Probe.run(Kind.Tcp, MonitorConfig(host = "127.0.0.1", port = s.getLocalPort), 2.seconds, loopback)
        _ = assert(ok.ok, ok.message)
        ng <- Probe.run(Kind.Tcp, MonitorConfig(host = "127.0.0.1", port = 1), 2.seconds, loopback)
        _ = assert(!ng.ok)
      } yield ()
    }
  }

  test("内部のアドレスは、許したものだけ監視できる（HTTP・TCP・PING・DNS のサーバー）") {
    Resource.make(IO.blocking(new java.net.ServerSocket(0)))(s => IO.blocking(s.close())).use { s =>
      val port = s.getLocalPort
      def refused(r: ProbeResult) = assert(!r.ok && r.message.contains("内部のアドレス"), r.message)
      for {
        // 既定（許可リストなし）では、ループバック・プライベート・メタデータのどれも断る（つなぎに行かない）
        tcp <- Probe.run(Kind.Tcp, MonitorConfig(host = "127.0.0.1", port = port), 2.seconds)
        http <- Probe.run(Kind.Http, MonitorConfig(url = s"http://127.0.0.1:$port/"), 2.seconds)
        meta <- Probe.run(Kind.Http, MonitorConfig(url = "http://169.254.169.254/latest/meta-data"), 2.seconds)
        v6 <- Probe.run(Kind.Http, MonitorConfig(url = s"http://[::1]:$port/"), 2.seconds)
        ping <- Probe.run(Kind.Ping, MonitorConfig(host = "10.0.0.1"), 2.seconds)
        dns <- Probe.run(Kind.Dns, MonitorConfig(host = "example.com", dnsServer = "192.168.0.1"), 2.seconds)
        // 許可リストにあっても、ほかの内部のアドレスは断る
        other <- Probe.run(Kind.Tcp, MonitorConfig(host = "127.0.0.2", port = port), 2.seconds, loopback)
      } yield List(tcp, http, meta, v6, ping, dns, other).foreach(refused)
    }
  }

  /** 手元の HTTP サーバー（/ok は 200、/to?u=… はその URL へ 302、/loop は自分へ 302）. */
  private def httpServer: Resource[IO, Int] =
    Resource.make(IO.blocking {
      val srv = com.sun.net.httpserver.HttpServer.create(new java.net.InetSocketAddress("127.0.0.1", 0), 0)
      def reply(x: com.sun.net.httpserver.HttpExchange, code: Int, location: Option[String]): Unit = {
        location.foreach(l => x.getResponseHeaders.add("Location", l))
        val body = "ok".getBytes
        x.sendResponseHeaders(code, body.length.toLong)
        x.getResponseBody.write(body)
        x.close()
      }
      srv.createContext("/ok", x => reply(x, 200, None))
      srv.createContext("/to", x => reply(x, 302, Some(x.getRequestURI.getRawQuery.stripPrefix("u="))))
      srv.createContext("/loop", x => reply(x, 302, Some("/loop")))
      srv.start()
      srv
    })(s => IO.blocking(s.stop(0))).map(_.getAddress.getPort)

  test("HTTP: リダイレクトは行き先を確かめながらたどり、内部のアドレスへはたどらない") {
    httpServer.use { port =>
      val base = s"http://127.0.0.1:$port"
      def run(c: MonitorConfig) = Probe.run(Kind.Http, c, 3.seconds, loopback)
      for {
        ok <- run(MonitorConfig(url = s"$base/ok", bodyContains = "ok"))
        _ = assert(ok.ok, ok.message)
        // 許した相手へのリダイレクトはたどる（相対の Location も）
        followed <- run(MonitorConfig(url = s"$base/to?u=/ok"))
        _ = assert(followed.ok, followed.message)
        // たどらない設定なら 302 のまま
        stay <- run(MonitorConfig(url = s"$base/to?u=/ok", followRedirects = false, expectStatus = "302"))
        _ = assert(stay.ok, stay.message)
        // 許していない内部のアドレスへのリダイレクトは断る（メタデータ・ほかのループバック）
        meta <- run(MonitorConfig(url = s"$base/to?u=http://169.254.169.254/latest/meta-data"))
        _ = assert(!meta.ok && meta.message.contains("内部のアドレス（169.254.169.254）"), meta.message)
        lo <- run(MonitorConfig(url = s"$base/to?u=http://127.0.0.2:$port/ok"))
        _ = assert(!lo.ok && lo.message.contains("内部のアドレス"), lo.message)
        // http・https 以外へはたどらない（302 のまま）
        file <- run(MonitorConfig(url = s"$base/to?u=file:///etc/passwd", expectStatus = "302"))
        _ = assert(file.ok, file.message)
        loop <- run(MonitorConfig(url = s"$base/loop"))
        _ = assert(!loop.ok && loop.message.contains("リダイレクトが多すぎます"), loop.message)
      } yield ()
    }
  }

  test("API の要求は、省いた項目を既定の値で読む") {
    val r = io.circe.parser.decode[MonitorReq]("""{"name": "x", "kind": "tcp", "config": {"host": "h", "port": 1}}""")
    assertEquals(
      r.map(x => (x.intervalSec, x.timeoutMs, x.retries, x.enabled, x.config.expectStatus)),
      Right((60, 10000, 1, true, "200-299"))
    )
    assertEquals(io.circe.parser.decode[StatusPageReq]("""{"slug": "a1", "title": "t"}""").map(_.public), Right(true))
  }
}
