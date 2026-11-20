package skotare.app

import sbirds.http.Env.*
import sbirds.db.*
import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import skotare.api.*
import sbirds.auth.OidcConfig
import sbirds.net.NetGuard

import scala.concurrent.duration.*

/** 設定は環境変数から読む. */
object Config {
  private def hostOf(url: String): Option[String] =
    scala.util.Try(java.net.URI.create(url).getHost).toOption.flatMap(Option(_)).map(_.toLowerCase)

  /** ノードの名前（既定はホスト名. コンテナではコンテナの ID になる）. */
  private def hostName: String = scala.util.Try(java.net.InetAddress.getLocalHost.getHostName).getOrElse("node")

  def load: IO[AppConfig] = IO.fromEither(
    (for {
      dbUrl <- required("DATABASE_URL")
      dbUser <- required("DATABASE_USER")
      dbPass <- required("DATABASE_PASSWORD")
      pub <- required("PUBLIC_URL").map(_.stripSuffix("/"))
      // 開発用のログインは名前だけでだれにでもなれるので、手元（localhost）で開くときだけ（sbirds.http.Env.devLogin）
      devLogin <- devLogin(pub)
      oidc <- (opt("OIDC_ISSUER"), opt("OIDC_CLIENT_ID"), opt("OIDC_CLIENT_SECRET")) match {
        // グループを絞らないと IdP のだれでもログインでき、監視を通して内部のネットワークを調べられる. 明示したときだけ許す
        case (Some(_), Some(_), Some(_)) if list("OIDC_ALLOWED_GROUPS").isEmpty && !enabled("OIDC_ALLOW_ANY_USER") =>
          Left("OIDC_ALLOWED_GROUPS でログインできるグループを絞ってください（IdP のだれでもログインしてよいときは OIDC_ALLOW_ANY_USER=true）")
        case (Some(i), Some(c), Some(s)) =>
          Right(Some(OidcConfig(
            i,
            c,
            s,
            s"$pub/auth/callback",
            opt("OIDC_GROUPS_CLAIM").getOrElse("groups"),
            Some(list("OIDC_SCOPES")).filter(_.nonEmpty).getOrElse(List("openid", "profile", "email")),
            list("OIDC_ALLOWED_GROUPS").toSet
          )))
        case (None, None, None) if devLogin => Right(None)
        case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      }
      rate <- sbirds.http.RateLimitConfig.fromEnv()
      secret <- opt("APP_SECRET") match {
        case Some(s) if s.length < 16 => Left("APP_SECRET は16文字以上にしてください")
        case s => Right(s)
      }
      conc <- int("CHECK_CONCURRENCY").getOrElse(16) match {
        case n if n >= 1 && n <= 256 => Right(n)
        case _ => Left("CHECK_CONCURRENCY は 1〜256 にしてください")
      }
      // 拠点の名前（同じ拠点のノードは同じ名前にする. 例: tokyo・osaka・東京）
      // 待ち受けるアドレス（既定 0.0.0.0. 開発用のログインのときは 127.0.0.1）
      bind <- opt("BIND_HOST").fold[Either[String, com.comcast.ip4s.Host]](Right(if devLogin then ipv4"127.0.0.1"
      else ipv4"0.0.0.0"))(h =>
        com.comcast.ip4s.Host.fromString(h).toRight("BIND_HOST は IP アドレスかホスト名で書いてください")
      )
      // OAUTH_CLIENTS: skotare を利用者の権限で使うサービス（scopes は省くと read だけ. 形は sbirds.oauth.OAuthClient.parse）
      clients <- sbirds.oauth.OAuthClient.fromEnv(
        sbirds.oauth.OAuthClient.Rules(duplicateError = Some("OAUTH_CLIENTS の id が重複しています"))
      )
      // 内部のアドレス（ループバック・プライベート・リンクローカルなど）のうち、監視してよいもの・通知を送ってよいもの
      // （ホスト名・IP アドレス・CIDR をカンマか空白で区切る. 既定は空で、外のアドレスだけ）
      probe <- NetGuard.Policy.allowList(list("PROBE_ALLOW_INTERNAL")).left.map(e => s"PROBE_ALLOW_INTERNAL: $e")
      // stui との連携先（STUI_URL・STUI_INTERNAL_URL）は管理者が決めた相手なので、通知の宛先として許す
      notify <- NetGuard.Policy.allowList(
        list("NOTIFY_ALLOW_INTERNAL") ++ List("STUI_URL", "STUI_INTERNAL_URL").flatMap(opt).flatMap(hostOf)
      ).left.map(e => s"NOTIFY_ALLOW_INTERNAL: $e")
      region <- opt("REGION").getOrElse("default") match {
        case r if r.length <= 40 && !r.exists(c => c.isControl || c == '/') => Right(r)
        case _ => Left("REGION は40文字までで、制御文字と / を含めないでください")
      }
    } yield AppConfig(
      port = int("HTTP_PORT").getOrElse(8080),
      // 接続の数は、同時に確かめる数より多くする（確認の記録と API・裏の処理で取り合って待たないように）
      db = DbConfig(dbUrl, dbUser, dbPass, int("DATABASE_POOL_SIZE").getOrElse(conc + 6)),
      oidc = oidc,
      api = ApiConfig(
        cookieSecure = pub.startsWith("https://"),
        publicUrl = pub,
        sessionTtl = int("SESSION_DAYS").getOrElse(14).days,
        devLogin = devLogin,
        oauthClients = clients,
        // STUI_URL・STUI_CLIENT_ID・STUI_CLIENT_SECRET（STUI_INTERNAL_URL はサーバーから呼ぶ URL）. 通知先に stui のチャンネルを選べるようにする
        stui = (opt("STUI_URL"), opt("STUI_CLIENT_ID"), opt("STUI_CLIENT_SECRET")).mapN((u, id, sec) =>
          LinkService("stui", u.stripSuffix("/"), id, sec, opt("STUI_INTERNAL_URL").map(_.stripSuffix("/")))
        ),
        rateLimit =
          rate
      ),
      staticDir = opt("STATIC_DIR"),
      appSecret = secret,
      node = opt("NODE_NAME").getOrElse(hostName).take(100),
      concurrency = conc,
      region = region,
      probePolicy = probe,
      notifyPolicy = notify,
      bindHost = bind
    )).left.map(new IllegalArgumentException(_))
  )
}
