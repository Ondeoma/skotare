package skotare.api

import cats.effect.*
import com.comcast.ip4s.{Cidr, IpAddress}
import org.http4s.*
import skotare.core.ErrorCode

import scala.concurrent.duration.FiniteDuration

/**
 * IP ごとの回数の制限（1分あたり. 0 なら制限しない）. ログインしなくても使える入口だけに掛ける.
 * auth はログイン（/auth/…）と OAuth の認可（/oauth/…）、public はステータスページ（/api/public/…）.
 * trustedProxies はリバースプロキシの IP か CIDR. そこから来た要求だけ X-Forwarded-For を信じる.
 */
final case class RateLimitConfig(
  authPerMinute: Int = 20,
  publicPerMinute: Int = 120,
  trustedProxies: List[Cidr[IpAddress]] = Nil)

/** 数え方は sbirds-lib の RateLimit. ここでは、どの入口をどのカウンタで数えるかだけを決める. */
object RateLimit {

  def apply(
    routes: HttpRoutes[IO],
    cfg: RateLimitConfig,
    clock: IO[FiniteDuration] = IO.realTime
  ): HttpRoutes[IO] = {
    val auth = sbirds.http.RateLimit.counter(cfg.authPerMinute, clock).toList
    val pub = sbirds.http.RateLimit.counter(cfg.publicPerMinute, clock).toList
    sbirds.http.RateLimit(routes, cfg.trustedProxies, ErrorCode.TooManyRequests) { req =>
      req.pathInfo.segments.map(_.decoded()).toList match {
        case ("auth" | "oauth") :: _ => auth
        case "api" :: "public" :: _ => pub
        case _ => Nil
      }
    }
  }
}
