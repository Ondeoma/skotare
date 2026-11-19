package skotare.api

import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.Authorization
import skotare.auth.SecretBox
import skotare.db.*
import sbirds.observe.{Metrics, Telemetry}
import skotare.shared.*

import java.time.Instant
import scala.concurrent.duration.*

object Senders {

  /** 送れなかった理由（短く. 相手の応答の本文や内部の詳細は含めない）. */
  def describe(e: Throwable): String = e match {
    case _: java.util.concurrent.TimeoutException => "時間内に応答がありません"
    case _: java.net.ConnectException | _: java.nio.channels.UnresolvedAddressException => "接続できません"
    case _: IllegalArgumentException | _: IllegalStateException => Option(e.getMessage).getOrElse("設定が正しくありません")
    case _ => Option(e.getMessage).filter(_.startsWith("HTTP ")).getOrElse(e.getClass.getSimpleName)
  }
}

/** 通知先へ送る（stui のチャンネル・stui の受信 Webhook・skereru の送信 API・汎用の Webhook）. */
final class Senders(http: Client[IO], box: Option[SecretBox], val publicUrl: String, stui: Option[StuiLink] = None) {

  private def secretOf(n: NotifierRow): Either[String, String] =
    if n.secret.isEmpty then Left("秘密の値（URL・APIキー）が設定されていません")
    else
      box.toRight(
        "APP_SECRET が設定されていないため、通知先の秘密の値を読めません"
      ).flatMap(_.decrypt(n.secret).toRight("秘密の値を読めません（APP_SECRET を変えましたか？）"))

  private def post(uri: String, body: Json, auth: Option[String] = None): IO[Unit] =
    IO.fromEither(Uri.fromString(uri).left.map(_ => new IllegalArgumentException("URL が正しくありません"))).flatMap { u =>
      val req = Request[IO](Method.POST, u).withEntity(body)
      http.run(auth.fold(req)(t => req.putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, t))))).use { r =>
        if r.status.isSuccess then IO.unit
        // 相手の本文は返さない（内部のホストの応答を、画面・記録へ持ち出させない）
        else IO.raiseError(new RuntimeException(s"HTTP ${r.status.code}"))
      }.timeout(20.seconds)
    }

  /** 秘密の値（URL・APIキー）で送るもの. */
  private def withSecret(n: NotifierRow, payload: Json): IO[Unit] =
    IO.fromEither(secretOf(n).left.map(new IllegalStateException(_))).flatMap { secret =>
      n.kind match {
        // stui: チャンネルの受信 Webhook（本文は Markdown）
        case NotifierKind.Stui =>
          post(
            secret,
            Json.obj(
              "text" -> Messages.body(payload).asJson,
              "username" -> "skotare".asJson,
              "icon_emoji" -> Messages.emoji(payload).asJson
            )
          )
        // skereru: メールを送る（APIキーの send のスコープ）
        case NotifierKind.Skereru =>
          val text = Messages.plain(payload)
          post(
            s"${n.config.baseUrl.stripSuffix("/")}/api/send",
            Json.obj(
              "from" -> n.config.from.asJson,
              "to" -> n.config.to.asJson,
              "cc" -> Json.arr(),
              "bcc" -> Json.arr(),
              "subject" -> s"[skotare] ${Messages.subject(payload)}".asJson,
              "text" -> text.asJson,
              "html" -> Json.Null,
              "uploadIds" -> Json.arr(),
              "inReplyTo" -> Json.Null,
              "forwardOf" -> Json.Null,
              "draftId" -> Json.Null
            ),
            Some(secret)
          )
        case _ => post(secret, payload)
      }
    }

  def send(n: NotifierRow, payload: Json): IO[Unit] =
    Telemetry.span("notify.send", "kind" -> n.kind)(
      // stui のチャンネル: 作った人のトークンで、ボット「skotare」として投稿する（本文は Markdown）
      if n.kind == NotifierKind.Stui && n.config.stuiChannel then
        stui.fold(IO.raiseError[Unit](new IllegalStateException("stui との連携が設定されていません")))(_.post(
          n,
          Messages.body(payload)
        ))
      else withSecret(n, payload)
    ) <* Metrics.count("skotare.notifications.sent", "kind" -> n.kind)

  /** 試しに送る本文. */
  def testPayload(n: NotifierRow): Json = Json.obj(
    "event" -> "test".asJson,
    "monitor" -> Json.obj("name" -> n.name.asJson, "target" -> "-".asJson),
    "message" -> "skotare からのテストの通知です".asJson,
    "at" -> Instant.now().toString.asJson,
    "url" -> publicUrl.asJson
  )
}
