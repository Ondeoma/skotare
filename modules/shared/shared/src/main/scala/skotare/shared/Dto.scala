package skotare.shared

import io.circe.*
import io.circe.derivation.{Configuration, ConfiguredDecoder}

import java.util.UUID

// バックエンド(JVM)と画面(Scala.js)の両方で使う API の型。
// 日時は ISO 8601 の UTC の文字列（2026-11-15T01:00:00Z）. 画面(Scala.js)で java.time を使わないため、文字列で持つ。

/** 省いた項目は既定の値にする（API・MCP から必要な項目だけ送れるように）. */
given Configuration = Configuration.default.withDefaults

/** stui は stui との連携（通知先にチャンネルを選ぶ）を設定しているか. */
final case class ClientConfigDto(devLogin: Boolean, oidc: Boolean, version: String, stui: Boolean = false)
  derives Encoder.AsObject, ConfiguredDecoder

final case class UserBrief(id: UUID, name: String, email: Option[String], avatarUrl: Option[String])
  derives Encoder.AsObject, Decoder

/**
 * avatarUploaded は、自分で保存したアイコンがあるか（なければ avatarUrl は IdP の画像）.
 * appNames は sbirds のアプリの表示名（slug → 名前. 自身は "_self"）. Cookie のセッションで /api/me を読んだときだけ入る.
 */
final case class MeDto(id: UUID,
                       name: String,
                       email: Option[String],
                       avatarUrl: Option[String],
                       timezone: String,
                       avatarUploaded: Boolean,
                       appNames: Map[String, String] = Map.empty)
  derives Encoder.AsObject, Decoder {
  def brief: UserBrief = UserBrief(id, name, email, avatarUrl)
}

/** 保存できるアイコン（画像の種類と大きさの上限）. SVG はスクリプトを含められるので受け付けない. */
object AvatarImage {
  val MaxBytes = 1024 * 1024

  /** 先頭のバイトから画像の種類を決める（PNG・JPEG・GIF・WebP 以外は None）. */
  def detect(b: Array[Byte]): Option[String] = {
    def at(i: Int, xs: Int*): Boolean = b.length >= i + xs.size &&
      xs.zipWithIndex.forall((x, j) => (b(i + j) & 0xff) == x)
    if (at(0, 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)) Some("image/png")
    else if (at(0, 0xff, 0xd8, 0xff)) Some("image/jpeg")
    else if (at(0, 0x47, 0x49, 0x46, 0x38)) Some("image/gif")
    else if (at(0, 0x52, 0x49, 0x46, 0x46) && at(8, 0x57, 0x45, 0x42, 0x50)) Some("image/webp")
    else None
  }
}

final case class UpdateMeReq(name: Option[String], timezone: Option[String]) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 監視

/** 監視の種類. */
object Kind {
  val Http = "http"
  val Tcp = "tcp"
  val Dns = "dns"
  val Ping = "ping"
  val all: List[String] = List(Http, Tcp, Dns, Ping)
  def label(k: String): String = k match {
    case Http => "HTTP(S)"
    case Tcp => "TCP"
    case Dns => "DNS"
    case Ping => "PING"
    case _ => k
  }
}

/** 監視の状態. */
object MonStatus {
  val Pending = "pending"
  val Up = "up"
  val Down = "down"
  val Paused = "paused"
  def label(s: String): String = s match {
    case Up => "正常"
    case Down => "停止"
    case Paused => "一時停止"
    case _ => "未確認"
  }
}

final case class KeyValue(name: String, value: String) derives Encoder.AsObject, ConfiguredDecoder

/**
 * 監視の設定（種類ごとに使う項目が違う. 画面のフォームで扱いやすいよう1つの形にまとめる）.
 *  - http: url・method・headers・body・expectStatus（例 200-299,301）・bodyContains・bodyNotContains・bodyRegex・followRedirects・
 *    certExpiryDays（証明書の残りがこの日数を切ったら失敗. 0 なら見ない）
 *  - tcp: host・port
 *  - dns: host（引く名前）・recordType（A・AAAA・CNAME・MX・TXT・NS）・dnsServer（空ならシステムの設定）・expect（答えに含まれるべき値. 空なら引けるかだけ）
 *  - ping: host
 */
final case class MonitorConfig(url: String = "",
                               method: String = "GET",
                               headers: List[KeyValue] = Nil,
                               body: String = "",
                               expectStatus: String = "200-299",
                               bodyContains: String = "",
                               bodyNotContains: String = "",
                               bodyRegex: String = "",
                               followRedirects: Boolean = true,
                               certExpiryDays: Int = 0,
                               host: String = "",
                               port: Int = 0,
                               recordType: String = "A",
                               dnsServer: String = "",
                               expect: String = "") derives Encoder.AsObject, ConfiguredDecoder {

  /**
   * 一覧・通知・ログに出す、確かめる相手. URL の利用者名・パスワードとクエリ（トークンなどを含みうる）は出さない.
   */
  def target(kind: String): String = kind match {
    case Kind.Http => s"$method ${MonitorConfig.shownUrl(url)}"
    case Kind.Tcp => s"$host:$port"
    case Kind.Dns => s"$recordType $host" + (if (dnsServer.isEmpty) "" else s" @$dnsServer")
    case _ => host
  }
}

/** HTTP の期待する応答コード（「200-299,301」の形）. */
object StatusMatcher {
  def parse(s: String): Either[String, List[(Int, Int)]] = {
    val parts = s.split(',').map(_.trim).filter(_.nonEmpty).toList
    val ranges = parts.map { p =>
      p.split('-').map(_.trim).toList match {
        case a :: Nil => a.toIntOption.map(x => (x, x))
        case a :: b :: Nil => for { x <- a.toIntOption; y <- b.toIntOption } yield (x, y)
        case _ => None
      }
    }
    if (parts.isEmpty) Left("期待する応答コードを入れてください（例 200-299）")
    else if (
      ranges.exists(r =>
        r.forall((a, b) => a < 100 || b > 599 || a > b)
      )
    ) Left("応答コードは 100〜599 の数か範囲（例 200-299,301）で書いてください")
    else Right(ranges.flatten)
  }
  def matches(ranges: List[(Int, Int)], code: Int): Boolean = ranges.exists((a, b) => code >= a && code <= b)
}

/** 監視の入力の確かめ（画面とサーバーで同じものを使う）. */
object MonitorCheck {
  val Methods = List("GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "OPTIONS")
  val RecordTypes = List("A", "AAAA", "CNAME", "MX", "TXT", "NS")
  private val hostRe = "^[A-Za-z0-9]([A-Za-z0-9.-]{0,251}[A-Za-z0-9])?$|^\\[?[0-9A-Fa-f:]+\\]?$".r
  private val headerRe = "^[!#$%&'*+.^_`|~0-9A-Za-z-]+$".r

  def host(h: String): Either[String, String] = {
    val t = h.trim
    if (t.isEmpty) Left("ホスト名を入れてください") else if (hostRe.matches(t)) Right(t) else Left(s"ホスト名が正しくありません: $t")
  }

  def validate(kind: String, c: MonitorConfig): Either[String, MonitorConfig] = kind match {
    case Kind.Http =>
      val url = c.url.trim
      if (!(url.startsWith("http://") || url.startsWith("https://")) || url.length > 2000)
        Left("URL は http:// か https:// で始めてください")
      else if (!Methods.contains(c.method.toUpperCase)) Left(s"メソッドは ${Methods.mkString("・")} のどれかです")
      else if (c.headers.size > 30 || c.headers.exists(h => h.value.length > 4000)) Left("ヘッダーは30件、値は4000文字までです")
      else if (c.headers.exists(h => !headerRe.matches(h.name.trim) || h.value.exists(ch => ch == '\r' || ch == '\n')))
        Left("ヘッダーの名前・値が正しくありません")
      else if (c.certExpiryDays < 0 || c.certExpiryDays > 365) Left("証明書の残りの日数は 0〜365 です")
      else if (c.certExpiryDays > 0 && !url.startsWith("https://")) Left("証明書の期限を確かめるのは https:// の URL のときだけです")
      else if (c.body.length > 65536) Left("送る本文は 64KB までです")
      else if (c.bodyRegex.length > 200 || c.bodyContains.length > 1000 || c.bodyNotContains.length > 1000)
        Left("本文の正規表現は200文字、本文に含む・含まない文字は1000文字までです")
      else StatusMatcher.parse(c.expectStatus).map(_ =>
        c.copy(
          url = url,
          method = c.method.toUpperCase,
          headers = c.headers.map(h => KeyValue(h.name.trim, h.value)).filter(_.name.nonEmpty),
          expectStatus = c.expectStatus.replace(" ", "")
        )
      )
    case Kind.Tcp =>
      host(c.host).flatMap(h =>
        if (c.port < 1 || c.port > 65535) Left("ポートは 1〜65535 です") else Right(MonitorConfig(host = h, port = c.port))
      )
    case Kind.Dns =>
      for {
        h <- host(c.host)
        _ <- if (RecordTypes.contains(c.recordType.toUpperCase)) Right(())
        else Left(s"レコードの種類は ${RecordTypes.mkString("・")} のどれかです")
        s <- if (c.dnsServer.trim.isEmpty) Right("") else host(c.dnsServer)
      } yield MonitorConfig(host = h, recordType = c.recordType.toUpperCase, dnsServer = s, expect = c.expect.trim)
    case Kind.Ping => host(c.host).map(h => MonitorConfig(host = h))
    case _ => Left(s"監視の種類は ${Kind.all.mkString("・")} のどれかです")
  }
}

final case class MonitorReq(name: String,
                            kind: String,
                            config: MonitorConfig,
                            intervalSec: Int = 60,
                            timeoutMs: Int = 10000,
                            retries: Int = 1,
                            enabled: Boolean = true,
                            description: String = "",
                            canary: Boolean = false,
                            confirmRegions: Int = 0) derives Encoder.AsObject, ConfiguredDecoder

object MonitorConfig {

  /** 保存した設定（JSON）から、確かめる相手を出す. */
  def targetOf(config: Json, kind: String): String = config.as[MonitorConfig].map(_.target(kind)).getOrElse("")

  /** URL から利用者名・パスワード（user:pass@）とクエリ・フラグメントを除く. */
  def shownUrl(url: String): String = {
    val noQuery = url.takeWhile(c => c != '?' && c != '#')
    val scheme = noQuery.indexOf("://")
    if (scheme < 0) noQuery
    else {
      val rest = noQuery.substring(scheme + 3)
      val hostEnd = rest.indexOf('/') match { case -1 => rest.length; case n => n }
      val at = rest.lastIndexOf('@', hostEnd - 1)
      noQuery.substring(0, scheme + 3) + (if (at >= 0) rest.substring(at + 1) else rest) +
        (if (noQuery.length < url.length) "?…" else "")
    }
  }
}

/** 停止と判断するのに要る拠点の数（MonitorReq.confirmRegions）. 0 は過半数、All はすべて. */
object ConfirmRegions {
  val Majority = 0
  val All = 20
  val choices: List[(Int, String)] =
    List(Majority -> "過半数の拠点（おすすめ）", 1 -> "1つの拠点", 2 -> "2つの拠点", 3 -> "3つの拠点", All -> "すべての拠点")

  /** 説明. regions は今の拠点の数（それ以上の数は「すべて」と同じ）. */
  def label(n: Int, regions: Int): String =
    if (n <= Majority) "過半数の拠点" else if (n >= All || (regions > 0 && n >= regions)) "すべての拠点" else s"${n}つの拠点"
}

/**
 * 1つの拠点から見た監視の状態. counted は、その拠点の票を今の判定に数えているか（拠点が疑わしい・止まっているときは false）.
 */
final case class RegionStateDto(region: String,
                                status: String,
                                since: String,
                                lastCheckedAt: Option[String],
                                latencyMs: Option[Int],
                                message: Option[String],
                                counted: Boolean) derives Encoder.AsObject, Decoder

/**
 * recent は最近の判定（古い順. 一覧の小さな帯）. uptime24h・uptime30d は稼働率（%）.
 * canary は基準点（拠点の健全性の判定に使う）か、confirmRegions は停止と判断するのに要る拠点の数（0 は過半数）. regions は拠点ごとの状態.
 */
final case class MonitorDto(id: UUID,
                            name: String,
                            kind: String,
                            config: MonitorConfig,
                            target: String,
                            intervalSec: Int,
                            timeoutMs: Int,
                            retries: Int,
                            enabled: Boolean,
                            description: String,
                            status: String,
                            statusSince: String,
                            lastCheckedAt: Option[String],
                            lastLatencyMs: Option[Int],
                            lastMessage: Option[String],
                            uptime24h: Option[Double],
                            uptime30d: Option[Double],
                            recent: List[Boolean],
                            canary: Boolean = false,
                            confirmRegions: Int = 0,
                            regions: List[RegionStateDto] = Nil)
  derives Encoder.AsObject, Decoder {

  /** 変えるときの要求（今の設定のまま）. */
  def toReq: MonitorReq =
    MonitorReq(name, kind, config, intervalSec, timeoutMs, retries, enabled, description, canary, confirmRegions)
}

final case class CheckDto(at: String,
                          ok: Boolean,
                          latencyMs: Option[Int],
                          message: String,
                          node: String,
                          region: String = "") derives Encoder.AsObject, Decoder

final case class IncidentDto(id: UUID,
                             monitorId: UUID,
                             monitorName: String,
                             startedAt: String,
                             resolvedAt: Option[String],
                             cause: String)
  derives Encoder.AsObject, Decoder

/** 日ごとの稼働率（%. その日に確認がなければ None）. day は yyyy-MM-dd（UTC）. */
final case class DayBar(day: String, uptime: Option[Double]) derives Encoder.AsObject, Decoder

final case class MonitorDetailDto(monitor: MonitorDto,
                                  checks: List[CheckDto],
                                  incidents: List[IncidentDto],
                                  days: List[DayBar])
  derives Encoder.AsObject, Decoder

/** 今すぐ確かめた結果（記録はしない. 設定を試すため）. */
final case class ProbeResultDto(ok: Boolean, latencyMs: Option[Int], message: String) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 通知先

/** 通知先の種類. stui は連携して選んだチャンネル（または受信 Webhook の URL）、skereru は API キーでメールを送る、webhook は任意の URL に JSON を POST する. */
object NotifierKind {
  val Stui = "stui"
  val Skereru = "skereru"
  val Webhook = "webhook"
  val all: List[String] = List(Stui, Skereru, Webhook)

  /** name はサービスの表示名（画面では sruru に登録した名前を渡す）. */
  def label(k: String, name: String => String = identity): String = k match {
    case Stui => s"${name(Stui)}（チャット）"
    case Skereru => s"${name(Skereru)}（メール）"
    case _ => "Webhook"
  }

  /** 秘密の値（暗号化して保存し、画面には返さない）の説明. */
  def secretLabel(k: String, name: String => String = identity): String = k match {
    case Stui => s"${name(Stui)} の受信 Webhook の URL"
    case Skereru => s"${name(Skereru)} の APIキー（send のスコープ）"
    case _ => "送り先の URL"
  }
}

/**
 * 秘密でない設定. skereru の baseUrl（skereru の URL）・from（差出人）・to（宛先）.
 * stui でチャンネルを選んだときは channelId・channelName（stui のチャンネル）. 送り方は2つ（どちらもサーバーが決める）:
 *  - connectedBy: その人の stui のトークンで、ボット「skotare」として投稿する
 *  - hookId: チャンネルの管理者が、連携して作った受信 Webhook（その URL を秘密の値として保存し、そこへ送る）
 * channelId がなければ、入力した秘密の値（受信 Webhook の URL）へ送る.
 */
final case class NotifierConfig(baseUrl: String = "",
                                from: String = "",
                                to: List[String] = Nil,
                                channelId: Option[UUID] = None,
                                channelName: String = "",
                                connectedBy: Option[UUID] = None,
                                hookId: Option[UUID] = None) derives Encoder.AsObject, ConfiguredDecoder {

  /** トークンで投稿する（受信 Webhook を作っていない）チャンネル. */
  def stuiChannel: Boolean = channelId.isDefined && hookId.isEmpty

  /** 連携して作った受信 Webhook で送るチャンネル. */
  def stuiHook: Boolean = channelId.isDefined && hookId.isDefined
}

/**
 * secret は変えるときだけ送る（None なら今のまま）.
 * hook は stui のチャンネル（config.channelId）を、受信 Webhook を作って送るか（自分がチャンネルの管理者のときだけ. 前と同じチャンネルなら作り直さない）.
 */
final case class NotifierReq(name: String,
                             kind: String,
                             config: NotifierConfig,
                             secret: Option[String],
                             allMonitors: Boolean = true,
                             monitorIds: List[UUID] = Nil,
                             enabled: Boolean = true,
                             hook: Boolean = false) derives Encoder.AsObject, ConfiguredDecoder

final case class NotifierDto(id: UUID,
                             name: String,
                             kind: String,
                             config: NotifierConfig,
                             hasSecret: Boolean,
                             allMonitors: Boolean,
                             monitorIds: List[UUID],
                             enabled: Boolean) derives Encoder.AsObject, Decoder

/** 外部のサービスとの連携（stui）. connected は自分が連携しているか. */
final case class IntegrationDto(provider: String, name: String, url: String, connected: Boolean)
  derives Encoder.AsObject, Decoder

/** 通知先に選べる stui のチャンネル（自分が参加していて、アーカイブしていないもの）. kind は public・private. admin は自分がチャンネルの管理者か（受信 Webhook を作れる）. */
final case class StuiChannelDto(id: UUID, name: String, kind: String, admin: Boolean = false) derives Encoder.AsObject,
    ConfiguredDecoder

// ------------------------------------------------------------------ ステータスページ

final case class PageItem(monitorId: UUID, displayName: Option[String]) derives Encoder.AsObject, Decoder

/** slug は /s/{slug} の部分. public が false ならログインした人だけが見られる. */
final case class StatusPageReq(slug: String,
                               title: String,
                               description: String = "",
                               notice: String = "",
                               public: Boolean = true,
                               items: List[PageItem] = Nil) derives Encoder.AsObject, ConfiguredDecoder

final case class StatusPageDto(id: UUID,
                               slug: String,
                               title: String,
                               description: String,
                               notice: String,
                               public: Boolean,
                               items: List[PageItem])
  derives Encoder.AsObject, Decoder

object Slug {
  private val re = "^[a-z0-9][a-z0-9-]{1,40}$".r
  def valid(s: String): Either[String, String] =
    Either.cond(re.matches(s.trim), s.trim, "URL の名前は小文字の英数字とハイフン（2〜41文字）にしてください")
}

/** ゲストに見せる1つの監視（監視の中身（URL・ホスト）は出さない）. */
final case class PublicItem(name: String, status: String, uptime90d: Option[Double], days: List[DayBar])
  derives Encoder.AsObject, Decoder

final case class PublicIncident(name: String, startedAt: String, resolvedAt: Option[String]) derives Encoder.AsObject,
    Decoder

/** overall: up（すべて正常）/ partial（一部で障害）/ down（すべて停止）/ pending. */
final case class PublicStatusDto(title: String,
                                 description: String,
                                 notice: String,
                                 overall: String,
                                 updatedAt: String,
                                 items: List[PublicItem],
                                 incidents: List[PublicIncident]) derives Encoder.AsObject, Decoder

object Overall {
  def of(statuses: List[String]): String = {
    val live = statuses.filterNot(s => s == MonStatus.Paused || s == MonStatus.Pending)
    if (live.isEmpty) MonStatus.Pending
    else if (live.forall(_ == MonStatus.Up)) MonStatus.Up
    else if (live.forall(_ == MonStatus.Down)) MonStatus.Down
    else "partial"
  }
  def label(o: String): String = o match {
    case MonStatus.Up => "すべて正常に動いています"
    case MonStatus.Down => "障害が発生しています"
    case "partial" => "一部で障害が発生しています"
    case _ => "確認しています"
  }
}

// ------------------------------------------------------------------ ノード（冗長構成）

final case class NodeDto(name: String,
                         version: String,
                         startedAt: String,
                         seenAt: String,
                         alive: Boolean,
                         region: String = "")
  derives Encoder.AsObject, Decoder

/** 拠点の健全性. ok（健全）/ suspect（skotare 側の異常の疑い. 票を数えない）/ offline（動いているノードがない）. */
object RegionHealth {
  val Ok = "ok"
  val Suspect = "suspect"
  val Offline = "offline"
  def label(h: String): String = h match {
    case Ok => "健全"
    case Suspect => "異常の疑い"
    case Offline => "停止"
    case _ => h
  }
}

/** 拠点. nodes は動いているノードの数、reason は疑わしい・止まっている理由. */
final case class RegionDto(name: String, health: String, reason: String, since: String, nodes: Int)
  derives Encoder.AsObject, Decoder

final case class SummaryDto(up: Int,
                            down: Int,
                            paused: Int,
                            pending: Int,
                            openIncidents: Int,
                            nodes: List[NodeDto],
                            regions: List[RegionDto] = Nil) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ API トークン

/** API トークン. scope は read（参照だけ）か write（変更も）. 日時は UTC の ISO 8601. */
object TokenScope {
  val Read = "read"
  val Write = "write"
  val all: List[String] = List(Read, Write)
  def label(s: String): String = if (s == Write) "読み書き" else "参照のみ"
}

final case class ApiTokenDto(id: UUID,
                             name: String,
                             prefix: String,
                             scope: String,
                             createdAt: String,
                             expiresAt: Option[String],
                             lastUsedAt: Option[String]) derives Encoder.AsObject, Decoder

final case class ApiTokenReq(name: String, scope: String, expiresInDays: Option[Int]) derives Encoder.AsObject, Decoder

/** 作ったときだけ token（秘密の値）を返す. */
final case class ApiTokenCreated(info: ApiTokenDto, token: String) derives Encoder.AsObject, Decoder

/** OAuth で連携しているサービス（クライアント）. scope は read か write（write は読み書き）. lastUsedAt は最後に API を呼んだ日時. */
final case class OAuthGrantDto(clientId: String,
                               name: String,
                               scope: String,
                               grantedAt: String,
                               lastUsedAt: Option[String])
  derives Encoder.AsObject, Decoder
