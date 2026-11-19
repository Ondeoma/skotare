package skotare.core

import cats.effect.*
import skotare.shared.*

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.{InetAddress, InetSocketAddress, Socket, URI}
import java.security.cert.X509Certificate
import java.time.{Duration as JDuration, Instant}
import java.util.Hashtable
import javax.naming.directory.InitialDirContext
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** 1回の確認の結果. latencyMs は応答までの時間（取れなければ None）. */
final case class ProbeResult(ok: Boolean, latencyMs: Option[Int], message: String)

/** 監視の確かめ方（HTTP(S)・TCP・DNS・PING）. どれも timeout を超えたら失敗にする. */
object Probe {

  val UserAgent = "skotare/1.0 (uptime monitor)"

  /** 本文はこの大きさまで読んで確かめる. */
  val MaxBody = 1024 * 1024

  /** 正規表現で照らし合わせる本文の上限と、照合にかけてよい時間（破滅的なバックトラックでスレッドを占めないように）. */
  val MaxRegexBody = 64 * 1024
  val RegexBudget: FiniteDuration = 500.millis

  def run(kind: String, cfg: MonitorConfig, timeout: FiniteDuration): IO[ProbeResult] = {
    val io = kind match {
      case Kind.Http => http(cfg, timeout)
      case Kind.Tcp => tcp(cfg.host, cfg.port, timeout)
      case Kind.Dns => dns(cfg, timeout)
      case Kind.Ping => ping(cfg.host, timeout)
      case k => IO.pure(ProbeResult(false, None, s"未対応の種類: $k"))
    }
    // 各方式の時間切れに加えて、全体にも上限を付ける（固まったままにしない）
    io.timeoutTo(timeout + 2.seconds, IO.pure(ProbeResult(false, None, s"${timeout.toMillis}ms 以内に応答がありません")))
      .handleError(e => ProbeResult(false, None, describe(e)))
  }

  /** 例外を、画面に出す短い説明にする. */
  def describe(e: Throwable): String = e match {
    case _: java.net.http.HttpTimeoutException | _: java.net.SocketTimeoutException => "時間内に応答がありません"
    case _: java.net.UnknownHostException => s"ホストが見つかりません（${e.getMessage}）"
    case _: java.net.ConnectException => s"接続できません（${Option(e.getMessage).getOrElse("接続を拒否されました")}）"
    case _: javax.net.ssl.SSLException => s"TLS のエラー: ${e.getMessage}"
    case _: javax.naming.NameNotFoundException => "名前が見つかりません（NXDOMAIN）"
    case _ => Option(
        e.getMessage
      ).filter(_.nonEmpty).map(m => s"${e.getClass.getSimpleName}: $m").getOrElse(e.getClass.getSimpleName)
  }

  private def elapsed(start: Long): Int = ((System.nanoTime() - start) / 1000000L).toInt

  // ------------------------------------------------------------------ HTTP(S)

  private lazy val follow =
    HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).connectTimeout(JDuration.ofSeconds(30)).build()
  private lazy val noFollow =
    HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NEVER).connectTimeout(JDuration.ofSeconds(30)).build()

  def http(c: MonitorConfig, timeout: FiniteDuration): IO[ProbeResult] =
    IO.defer {
      val publisher =
        if c.body.isEmpty then HttpRequest.BodyPublishers.noBody() else HttpRequest.BodyPublishers.ofString(c.body)
      val b = HttpRequest.newBuilder(URI.create(c.url)).timeout(JDuration.ofMillis(timeout.toMillis)).method(
        c.method,
        publisher
      )
        .header("User-Agent", UserAgent)
      c.headers.foreach(h => b.setHeader(h.name, h.value))
      val req = b.build()
      val start = System.nanoTime()
      IO.fromCompletableFuture(IO((if c.followRedirects then follow else noFollow).sendAsync(
        req,
        HttpResponse.BodyHandlers.ofInputStream()
      ))).flatMap { res =>
        // 本文は上限まで読む（大きな本文で記憶を使い切らない）
        // 本文の読み込みと照らし合わせ（正規表現を含む）は中断できるスレッドで行い、中断したら接続を閉じる
        // （少しずつしか返さない相手でも、時間切れのあとにスレッドと接続を残さない）. 本文を見ない設定なら読まずに閉じる
        val in = res.body()
        val needBody = c.bodyContains.nonEmpty || c.bodyNotContains.nonEmpty || c.bodyRegex.nonEmpty
        IO.interruptible {
          val bytes = try (if needBody then in.readNBytes(MaxBody) else Array.emptyByteArray)
          finally in.close()
          val ms = elapsed(start)
          val body = new String(bytes, java.nio.charset.StandardCharsets.UTF_8)
          val cert = res.sslSession().toScala.flatMap(s =>
            Try(s.getPeerCertificates.head.asInstanceOf[X509Certificate].getNotAfter.toInstant).toOption
          )
          checkHttp(c, res.statusCode(), body, cert, Instant.now()).fold(
            m => ProbeResult(false, Some(ms), m),
            m => ProbeResult(true, Some(ms), m)
          )
        }.onCancel(IO(in.close()).voidError)
      }
    }

  /** 応答を設定と照らし合わせる（Right は成功の説明、Left は失敗の理由）. */
  def checkHttp(c: MonitorConfig,
                code: Int,
                body: String,
                certNotAfter: Option[Instant],
                now: Instant): Either[String, String] = {
    val ranges = StatusMatcher.parse(c.expectStatus).getOrElse(List(200 -> 299))
    val certDays = certNotAfter.map(t => JDuration.between(now, t).toDays)
    for {
      _ <- Either.cond(StatusMatcher.matches(ranges, code), (), s"応答コード $code（期待 ${c.expectStatus}）")
      _ <- Either.cond(
        c.bodyContains.isEmpty || body.contains(c.bodyContains),
        (),
        s"本文に「${c.bodyContains.take(80)}」がありません"
      )
      _ <- Either.cond(
        c.bodyNotContains.isEmpty || !body.contains(c.bodyNotContains),
        (),
        s"本文に「${c.bodyNotContains.take(80)}」があります"
      )
      _ <- if c.bodyRegex.isEmpty then Right(())
      else
        regexFind(c.bodyRegex, body).flatMap(found =>
          Either.cond(found, (), s"本文が正規表現 /${c.bodyRegex.take(80)}/ に合いません")
        )
      _ <- (certNotAfter, certDays, c.certExpiryDays) match {
        case (Some(t), _, n) if n > 0 && t.isBefore(now) => Left("証明書の期限が切れています")
        case (_, Some(d), n) if n > 0 && d < n => Left(s"証明書の期限まで残り ${d} 日です（${n} 日を切りました）")
        case _ => Right(())
      }
    } yield s"$code" + certDays.filter(_ => c.certExpiryDays > 0).fold("")(d => s"（証明書の残り ${d} 日）")
  }

  /** 照合の時間切れ（スタックトレースは作らない）. */
  private final class RegexTimeout extends RuntimeException("regex timeout", null, false, false)

  /** 文字を読むたびに期限を確かめる（java.util.regex は中断できないため、期限を過ぎたら例外で抜ける）. */
  private final class Deadline(s: CharSequence, until: Long) extends CharSequence {
    def length: Int = s.length
    def charAt(i: Int): Char = {
      if System.nanoTime() > until then throw new RegexTimeout
      s.charAt(i)
    }
    def subSequence(a: Int, b: Int): CharSequence = new Deadline(s.subSequence(a, b), until)
    override def toString: String = s.toString
  }

  /** 本文（先頭 MaxRegexBody 文字）に正規表現が見つかるか. 時間がかかりすぎる・深くなりすぎる正規表現は失敗にする. */
  def regexFind(re: String, body: String): Either[String, Boolean] =
    try Right(java.util.regex.Pattern.compile(re).matcher(new Deadline(
        body.take(MaxRegexBody),
        System.nanoTime() + RegexBudget.toNanos
      )).find())
    catch {
      case _: java.util.regex.PatternSyntaxException => Left("本文の正規表現が正しくありません")
      case _: RegexTimeout => Left("本文の正規表現の照合に時間がかかりすぎます（正規表現を見直してください）")
      case _: StackOverflowError => Left("本文の正規表現の照合が深くなりすぎました（正規表現を見直してください）")
    }

  // ------------------------------------------------------------------ TCP

  def tcp(host: String, port: Int, timeout: FiniteDuration): IO[ProbeResult] =
    IO.interruptible {
      val start = System.nanoTime()
      val s = new Socket()
      try {
        s.connect(new InetSocketAddress(host, port), timeout.toMillis.toInt)
        ProbeResult(true, Some(elapsed(start)), s"$host:$port に接続できました")
      } finally s.close()
    }

  // ------------------------------------------------------------------ DNS

  def dns(c: MonitorConfig, timeout: FiniteDuration): IO[ProbeResult] =
    IO.interruptible {
      val env = new Hashtable[String, String]()
      env.put("java.naming.factory.initial", "com.sun.jndi.dns.DnsContextFactory")
      env.put("java.naming.provider.url", if c.dnsServer.isEmpty then "dns:" else s"dns://${c.dnsServer}")
      // 1回目を timeout の 1/3 にし、2回目は倍になるので、合わせて timeout に収める
      env.put("com.sun.jndi.dns.timeout.initial", math.max(timeout.toMillis / 3, 300).toString)
      env.put("com.sun.jndi.dns.timeout.retries", "2")
      val start = System.nanoTime()
      val ctx = new InitialDirContext(env)
      try {
        val attr = Option(ctx.getAttributes(c.host, Array(c.recordType)).get(c.recordType))
        val answers = attr.toList.flatMap(a => a.getAll.asScala.map(_.toString).toList)
        val ms = Some(elapsed(start))
        checkDns(c, answers).fold(m => ProbeResult(false, ms, m), m => ProbeResult(true, ms, m))
      } finally ctx.close()
    }

  /** 答えを期待する値と照らし合わせる（末尾の「.」と大文字・小文字の違いは見ない）. */
  def checkDns(c: MonitorConfig, answers: List[String]): Either[String, String] = {
    val norm = (s: String) => s.trim.stripSuffix(".").toLowerCase
    val shown = answers.take(5).mkString(", ")
    if answers.isEmpty then Left(s"${c.recordType} レコードがありません")
    else if c.expect.nonEmpty && !answers.exists(a => norm(a).contains(norm(c.expect))) then
      Left(s"答えに「${c.expect}」がありません（$shown）")
    else Right(shown)
  }

  // ------------------------------------------------------------------ PING

  private val timeRe = "time[=<]\\s*([0-9.]+)\\s*ms".r

  /**
   * ping コマンドを1回だけ送る（ICMP は特権が要るため OS のコマンドに任せる）. ホスト名は MonitorCheck.host で確かめたもの.
   * ping コマンドがなければ InetAddress.isReachable（ICMP が使えなければ TCP の echo）で代わりに確かめる.
   */
  def ping(host: String, timeout: FiniteDuration): IO[ProbeResult] = {
    val mac = sys.props.get("os.name").exists(_.toLowerCase.contains("mac"))
    // -W は macOS ではミリ秒、Linux（iputils・busybox）では秒
    val wait = if mac then timeout.toMillis.toString else math.max(1L, timeout.toSeconds).toString
    val cmd = List("ping", "-c", "1", "-W", wait, host)
    IO.interruptible {
      val start = System.nanoTime()
      Try(new ProcessBuilder(cmd*).redirectErrorStream(true).start()).toOption match {
        case Some(p) =>
          // 中断されても、ping のプロセスを残さない
          val (out, code) =
            try (new String(p.getInputStream.readNBytes(64 * 1024), "UTF-8"), p.waitFor())
            finally p.destroyForcibly()
          val ms = timeRe.findFirstMatchIn(out).flatMap(m => m.group(1).toDoubleOption).map(d => math.round(d).toInt)
          if code == 0 then ProbeResult(true, ms.orElse(Some(elapsed(start))), s"$host から応答がありました")
          else
            ProbeResult(
              false,
              None,
              out.linesIterator.map(_.trim).filter(_.nonEmpty).toList.lastOption.map(l =>
                s"応答がありません（${l.take(160)}）"
              ).getOrElse("応答がありません")
            )
        case None =>
          val ok = InetAddress.getByName(host).isReachable(timeout.toMillis.toInt)
          ProbeResult(ok, Option.when(ok)(elapsed(start)), if ok then s"$host から応答がありました" else "応答がありません")
      }
    }
  }

  extension [A](o: java.util.Optional[A]) {
    private def toScala: Option[A] = if o.isPresent then Some(o.get) else None
  }
}
