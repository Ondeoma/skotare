package skotare.app

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import skotare.api.*
import sbirds.auth.OidcConfig
import skotare.db.*

import scala.concurrent.duration.*

/** 設定は環境変数から読む. */
object Config {
  private def opt(name: String): Option[String] = sys.env.get(name).map(_.trim).filter(_.nonEmpty)
  private def int(name: String): Option[Int] = opt(name).flatMap(_.toIntOption)
  private def enabled(name: String): Boolean = opt(name).exists(_.equalsIgnoreCase("true"))
  private def required(name: String): Either[String, String] = opt(name).toRight(s"環境変数 $name を設定してください")
  private def list(name: String): List[String] =
    opt(name).toList.flatMap(_.split(Array(',', ' ')).map(_.trim).filter(_.nonEmpty))

  private def local(url: String): Boolean =
    scala.util.Try(java.net.URI.create(url).getHost).toOption.flatMap(Option(_)).map(_.toLowerCase)
      .exists(h => Set("localhost", "127.0.0.1", "[::1]").contains(h) || h.endsWith(".localhost"))

  /** ノードの名前（既定はホスト名. コンテナではコンテナの ID になる）. */
  private def hostName: String = scala.util.Try(java.net.InetAddress.getLocalHost.getHostName).getOrElse("node")

  /**
   * OAUTH_CLIENTS: skotare を利用者の権限で使うサービス（OAuth のクライアント）.
   * [{"id":"skakapo","secret":"…（16文字以上）","redirectUri":"https://…/api/connections/callback","name":"skakapo","scopes":["read","write"]}]
   * （scopes は省くと read だけ. redirectUri は https://（手元なら http://localhost も可））
   */
  private def oauthClients: Either[String, List[OAuthClient]] =
    opt("OAUTH_CLIENTS").fold(Right(Nil)) { s =>
      io.circe.parser.parse(s).flatMap(_.as[List[io.circe.Json]]).left.map(_ =>
        "OAUTH_CLIENTS は JSON の配列で指定してください"
      ).flatMap(_.traverse { j =>
        val c = j.hcursor
        (c.get[String]("id"), c.get[String]("secret"), c.get[String]("redirectUri")).tupled.toOption
          .filter((id, sec, uri) =>
            id.nonEmpty && sec.length >= 16 && (uri.startsWith("https://") || uri.startsWith("http://localhost"))
          )
          .map((id, sec, uri) =>
            OAuthClient(
              id,
              sec,
              uri,
              c.get[String]("name").getOrElse(id),
              c.get[List[String]]("scopes").toOption.map(_.filter(Set(
                "read",
                "write"
              ))).filter(_.nonEmpty).getOrElse(List("read"))
            )
          )
          .toRight("OAUTH_CLIENTS の各要素には id・secret（16文字以上）・redirectUri（https://）を指定してください")
      }).flatMap(cs => Either.cond(cs.map(_.id).distinct.size == cs.size, cs, "OAUTH_CLIENTS の id が重複しています"))
    }

  def load: IO[AppConfig] = IO.fromEither(
    (for {
      dbUrl <- required("DATABASE_URL")
      dbUser <- required("DATABASE_USER")
      dbPass <- required("DATABASE_PASSWORD")
      pub <- required("PUBLIC_URL").map(_.stripSuffix("/"))
      devLogin = enabled("DEV_LOGIN")
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
        // 開発用のログインは名前だけでだれにでもなれるので、手元（localhost）で開くときだけ
        case (None, None, None) if devLogin && !local(pub) => Left("DEV_LOGIN は PUBLIC_URL が localhost のときだけ使えます")
        case (None, None, None) if devLogin => Right(None)
        case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      }
      proxies <- list("TRUSTED_PROXIES").traverse(s =>
        sbirds.http.RateLimit.proxy(s).toRight(s"TRUSTED_PROXIES の「$s」は IP か CIDR で書いてください")
      )
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
      clients <- oauthClients
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
          RateLimitConfig(int("RATE_LIMIT_AUTH").getOrElse(20), int("RATE_LIMIT_PUBLIC").getOrElse(120), proxies)
      ),
      staticDir = opt("STATIC_DIR"),
      appSecret = secret,
      node = opt("NODE_NAME").getOrElse(hostName).take(100),
      concurrency = conc,
      region = region,
      bindHost = bind
    )).left.map(new IllegalArgumentException(_))
  )
}
