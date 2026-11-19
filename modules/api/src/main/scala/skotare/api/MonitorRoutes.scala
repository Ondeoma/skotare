package skotare.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skotare.auth.SecretBox
import skotare.core.Probe
import skotare.db.*
import skotare.observe.{Metrics, Telemetry}
import skotare.shared.*

import java.time.{Instant, LocalDate, ZoneOffset}
import java.util.UUID
import scala.concurrent.duration.*

/** 監視・障害・通知先・ステータスページ（管理）と、ゲストに見せるステータスページ. */
final class MonitorRoutes(monitors: MonitorStore, notifiers: NotifierStore, pages: PageStore, nodes: NodeStore, senders: Senders,
    box: Option[SecretBox], val sessions: SessionAuth, announcer: Announcer, stui: Option[StuiLink] = None) extends RouteSupport:

  // ------------------------------------------------------------------ 入力の確かめ

  private def validMonitor(r: MonitorReq): Either[String, MonitorReq] =
    for
      name <- Check.name(r.name)
      _    <- Either.cond(r.intervalSec >= 20 && r.intervalSec <= 86400, (), "確認の間隔は20秒〜1日です")
      _    <- Either.cond(r.timeoutMs >= 500 && r.timeoutMs <= 60000, (), "タイムアウトは500ミリ秒〜60秒です")
      _    <- Either.cond(r.retries >= 0 && r.retries <= 10, (), "停止と判断するまでの再試行は0〜10回です")
      _    <- Either.cond(r.confirmRegions >= 0 && r.confirmRegions <= 20, (), "停止と判断する拠点の数は0（過半数）〜20です")
      _    <- Check.text(r.description, "説明", 1000)
      cfg  <- MonitorCheck.validate(r.kind, r.config)
      _    <- if cfg.bodyRegex.isEmpty then Right(())
              else scala.util.Try(java.util.regex.Pattern.compile(cfg.bodyRegex)).toEither.left.map(_ => "本文の正規表現が正しくありません")
    yield r.copy(name = name, config = cfg)

  private def httpUrl(s: String, what: String): Either[String, String] =
    val t = s.trim
    Either.cond((t.startsWith("https://") || t.startsWith("http://")) && t.length <= 2000 && !t.exists(_.isWhitespace), t, s"$what は http(s):// の URL で入れてください")

  /**
   * 通知先の入力を確かめ、秘密の値を暗号化する（None なら今のまま）. current は変える前の通知先（作るときは None）.
   * 種類や skereru の URL を変えるときは、秘密の値を入れ直してもらう（今の API キー・URL を別の送り先へ送らせないため）.
   */
  private def validNotifier(r: NotifierReq, current: Option[NotifierRow]): Either[String, (NotifierReq, Option[String])] =
    val secret = r.secret.map(_.trim).filter(_.nonEmpty)
    for
      name <- Check.name(r.name)
      _    <- Either.cond(r.monitorIds.size <= 1000, (), "選べる監視は1000件までです")
      _    <- Either.cond(NotifierKind.all.contains(r.kind), (), s"種類は ${NotifierKind.all.mkString("・")} のどれかです")
      // stui のチャンネル: トークンで投稿するなら秘密の値は使わない（前の受信 Webhook の URL は消す）. 投稿する人・作る Webhook は prepare で決める
      stuiCh = r.kind == NotifierKind.Stui && r.config.channelId.isDefined
      res  <- if stuiCh then Right((NotifierConfig(channelId = r.config.channelId), Option.unless(r.hook)(""))) else secretNotifier(r, current, secret)
    yield (r.copy(name = name, config = res._1, secret = None), res._2)

  /** 秘密の値（受信 Webhook の URL・skereru の API キー・Webhook の URL）で送る通知先. */
  private def secretNotifier(r: NotifierReq, current: Option[NotifierRow], secret: Option[String]): Either[String, (NotifierConfig, Option[String])] =
    for
      _    <- if r.kind == NotifierKind.Skereru then secret.traverse_(s => Either.cond(s.length <= 500, (), "APIキーが長すぎます"))
              else secret.traverse_(httpUrl(_, NotifierKind.secretLabel(r.kind)))
      cfg  <- if r.kind != NotifierKind.Skereru then Right(NotifierConfig())
              else for
                base <- httpUrl(r.config.baseUrl, "skereru の URL")
                from <- Either.cond(r.config.from.contains("@"), r.config.from.trim, "差出人のメールアドレスを入れてください")
                to   = r.config.to.map(_.trim).filter(_.nonEmpty).distinct
                _    <- Either.cond(to.nonEmpty && to.size <= 50 && to.forall(_.contains("@")), (), "宛先のメールアドレスを入れてください（50件まで）")
              yield NotifierConfig(base.stripSuffix("/"), from, to)
      keep  = current.exists(c => c.kind == r.kind && c.config.baseUrl == cfg.baseUrl && c.secret.nonEmpty)
      _    <- Either.cond(secret.isDefined || keep, (),
                if current.isEmpty then s"${NotifierKind.secretLabel(r.kind)}を入れてください"
                else s"種類や送り先を変えるときは、${NotifierKind.secretLabel(r.kind)}も入れ直してください")
      enc  <- secret.traverse(s => box.toRight("通知先の秘密の値を保存するには APP_SECRET（16文字以上）を設定してください").map(_.encrypt(s)))
    yield (cfg, enc)

  /** 通知先の入力を確かめ、stui のチャンネルなら投稿する人・受信 Webhook を決める. (保存する内容, 秘密の値（None なら今のまま）). */
  private def prepare(u: User, current: Option[NotifierRow], r: NotifierReq): IO[Either[String, (NotifierReq, Option[String])]] =
    validNotifier(r, current) match
      case Left(e) => IO.pure(Left(e))
      case Right((n, enc)) => n.config.channelId match
        case None             => IO.pure(Right((n, enc)))
        case Some(ch) if r.hook => stuiHook(u, current, n, ch)
        case Some(_)          => stuiOwner(u, current, n).map(_.map(_ -> enc))

  /**
   * stui のチャンネルに、受信 Webhook を作って送る通知先. 前と同じチャンネルの Webhook なら、そのまま使う.
   * 作れるのは、変えた人がそのチャンネルの管理者のときだけ（stui も確かめる）. 作った URL は秘密の値として暗号化して保存する.
   */
  private def stuiHook(u: User, current: Option[NotifierRow], n: NotifierReq, ch: UUID): IO[Either[String, (NotifierReq, Option[String])]] =
    current.filter(c => c.kind == NotifierKind.Stui && c.config.stuiHook && c.config.channelId.contains(ch) && c.secret.nonEmpty) match
      case Some(c) => IO.pure(Right((n.copy(config = c.config), None)))
      case None =>
        (stui, box) match
          case (None, _) => IO.pure(Left("stui との連携が設定されていません"))
          case (_, None) => IO.pure(Left("通知先の秘密の値を保存するには APP_SECRET（16文字以上）を設定してください"))
          case (Some(s), Some(b)) =>
            s.channels(u.id).flatMap {
              case Left(e) => IO.pure(Left(e))
              case Right(cs) => cs.find(_.id == ch) match
                case None => IO.pure(Left("そのチャンネルは選べません（stui で参加しているチャンネルから選んでください）"))
                case Some(c) if !c.admin => IO.pure(Left("受信 Webhook を作れるのは、チャンネルの管理者だけです"))
                case Some(c) => s.createHook(u.id, ch, n.name).map(_.map((hookId, url) =>
                    (n.copy(config = NotifierConfig(channelId = Some(ch), channelName = c.name, hookId = Some(hookId))), Some(b.encrypt(url)))))
            }

  /** 前に作った受信 Webhook を、使わなくなったら消す（消した人がチャンネルの管理者のときだけ消せる. 消せなくても続ける）. */
  private def dropHook(u: User, before: Option[NotifierRow], after: Option[NotifierConfig]): IO[Unit] =
    (stui, before.flatMap(_.config.hookId).filterNot(h => after.exists(_.hookId.contains(h)))).tupled.traverse_((s, h) => s.deleteHook(u.id, h))

  /**
   * stui のチャンネルを選んだ通知先の、投稿する人（connectedBy）とチャンネルの名前を決める.
   * 前と同じチャンネルで、前に選んだ人がまだ連携していれば、その人のまま（ほかの人が監視の選択だけを変えられるように）.
   * そうでなければ、変えた人が連携していて、そのチャンネルに参加している必要がある（その人のトークンで投稿する）.
   */
  private def stuiOwner(u: User, current: Option[NotifierRow], n: NotifierReq): IO[Either[String, NotifierReq]] =
    n.config.channelId match
      case None => IO.pure(Right(n))
      case Some(ch) =>
        val prev = current.filter(c => c.kind == NotifierKind.Stui && c.config.channelId.contains(ch)).flatMap(c => c.config.connectedBy.map(_ -> c.config))
        stui.fold(IO.pure(false))(s => prev.fold(IO.pure(false))(p => s.connected(p._1))).flatMap {
          case true => IO.pure(Right(n.copy(config = prev.get._2)))
          case false =>
            stui.fold(IO.pure(Left("stui との連携が設定されていません"): Either[String, List[StuiChannelDto]]))(_.channels(u.id)).map(_.flatMap(cs =>
              cs.find(_.id == ch).toRight("そのチャンネルは選べません（stui で参加しているチャンネルから選んでください）")
                .map(c => n.copy(config = NotifierConfig(channelId = Some(c.id), channelName = c.name, connectedBy = Some(u.id))))))
        }

  private def validPage(r: StatusPageReq): Either[String, StatusPageReq] =
    for
      slug  <- Slug.valid(r.slug)
      title <- Check.name(r.title, "題名")
      _     <- Check.text(r.description, "説明", 2000)
      _     <- Check.text(r.notice, "お知らせ", 2000)
      _     <- Either.cond(r.items.size <= 200, (), "監視は200件までです")
      _     <- r.items.flatMap(_.displayName).traverse_(n => Check.text(n, "ページでの名前", 100))
    yield r.copy(slug = slug, title = title)

  // ------------------------------------------------------------------ ゲストに見せるページ

  /**
   * ゲストに見せる内容は、ページごとに30秒だけ覚えておく（ログインなしで呼べるので、何度呼ばれても DB に負担をかけない）.
   * ノードごとに覚えるので、ノードによって最大30秒ずれる. ページを変えたら忘れる.
   */
  private val publicCache = new TtlCache[UUID, PublicStatusDto](30000L)

  /**
   * 監視の一覧は、だれが見ても同じなので5秒だけ覚えておく（画面の定期の読み直しや、API・MCP から何度呼ばれても重い集計を繰り返さない）.
   * 監視を変えたら忘れる.
   */
  private val listCache = new TtlCache[Unit, List[MonitorDto]](5000L)
  private def cachedList: IO[List[MonitorDto]] = listCache.get(())(monitors.list)
  private def changedMonitors[A](io: IO[A]): IO[A] = io <* listCache.clear

  /** 通知先の試しの送信は、通知先ごとに10秒に1回まで（宛先へ大量に送らせない）. */
  private val lastTest = Ref.unsafe[IO, Map[UUID, Long]](Map.empty)
  private def testAllowed(id: UUID): IO[Boolean] =
    IO.realTime.map(_.toMillis).flatMap(now => lastTest.modify { m =>
      val ok = m.get(id).forall(now - _ >= 10000L)
      (if ok then m.filter((_, t) => now - t < 10000L) + (id -> now) else m, ok)
    })

  /** ゲストに見せる内容（監視の中身（URL・ホスト）は出さない. 障害は14日分と、続いているもの）. */
  private def publicStatus(p: StatusPageDto): IO[PublicStatusDto] =
    val ids = p.items.map(_.monitorId)
    for
      now  <- IO.realTimeInstant
      today = LocalDate.now(ZoneOffset.UTC)
      ms   <- monitors.statuses(ids)
      days <- monitors.days(ids, monitors.Days, today)
      ups  <- monitors.uptimeOver(ids, monitors.Days, today)
      inc  <- monitors.incidentsOf(ids, now.minusSeconds(14L * 86400), 200)
      names = p.items.flatMap(i => ms.get(i.monitorId).map(m => i.monitorId -> i.displayName.getOrElse(m._1))).toMap
      items = p.items.filter(i => ms.contains(i.monitorId)).map(i =>
                PublicItem(names(i.monitorId), ms(i.monitorId)._2, ups.get(i.monitorId), days.getOrElse(i.monitorId, Nil)))
      recent = inc.filter(i => names.contains(i.monitorId)).map(i => PublicIncident(names(i.monitorId), i.startedAt, i.resolvedAt))
    yield PublicStatusDto(p.title, p.description, p.notice, Overall.of(items.map(_.status)), now.toString, items, recent)

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // ログインしない人も見られる（公開しないページはログインした人だけ）
    case req @ GET -> Root / "api" / "public" / "status" / slug =>
      pages.bySlug(slug).flatMap {
        case Some(p) if p.public => Metrics.count("skotare.status_page.views", "page" -> p.slug) *> publicCache.get(p.id)(publicStatus(p)).flatMap(Ok(_))
        case Some(p) => sessions.fromRequest(req).flatMap(_.fold(err(Status.NotFound, "ページが見つかりません"))(_ => publicStatus(p).flatMap(Ok(_))))
        case None    => err(Status.NotFound, "ページが見つかりません")
      }

    case req @ GET -> Root / "api" / "summary" => authed(req)(_ => (nodes.list, nodes.regions, monitors.counts).flatMapN { (ns, rs, c) =>
      Ok(SummaryDto(c._1, c._2, c._3, c._4, c._5, ns, rs))
    })
    case req @ GET -> Root / "api" / "nodes"   => authed(req)(_ => nodes.list.flatMap(Ok(_)))
    case req @ GET -> Root / "api" / "regions" => authed(req)(_ => nodes.regions.flatMap(Ok(_)))

    // ------------------------------------------------------------------ 監視

    case req @ GET -> Root / "api" / "monitors" => authed(req)(_ => cachedList.flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "monitors" =>
      authed(req)(u => body[MonitorReq](req)(r => valid(validMonitor(r))(m =>
        changedMonitors(monitors.create(m, u.id)).flatTap(_ => Metrics.count("skotare.monitors.created", "kind" -> m.kind)).flatMap(Created(_)))))

    // 保存せずに今すぐ確かめる（設定を試す）
    case req @ POST -> Root / "api" / "monitors" / "test" =>
      authed(req)(_ => body[MonitorReq](req)(r => valid(validMonitor(r))(m =>
        Telemetry.span("monitor.test", "monitor.kind" -> m.kind)(Probe.run(m.kind, m.config, m.timeoutMs.millis))
          .flatMap(p => Ok(ProbeResultDto(p.ok, p.latencyMs, p.message))))))

    case req @ GET -> Root / "api" / "monitors" / UuidVar(id) =>
      authed(req)(_ => monitors.detail(id).flatMap(_.fold(err(Status.NotFound, "監視が見つかりません"))(Ok(_))))

    case req @ PUT -> Root / "api" / "monitors" / UuidVar(id) =>
      authed(req)(_ => body[MonitorReq](req)(r => valid(validMonitor(r))(m =>
        changedMonitors(monitors.update(id, m, senders.publicUrl)).flatTap(_.traverse_(_._2.traverse_(announcer.transition))).flatMap(_.fold(err(Status.NotFound, "監視が見つかりません"))(x => Ok(x._1))))))

    case req @ DELETE -> Root / "api" / "monitors" / UuidVar(id) =>
      authed(req)(_ => changedMonitors(monitors.delete(id)).ifM(NoContent(), err(Status.NotFound, "監視が見つかりません")))

    case req @ POST -> Root / "api" / "monitors" / UuidVar(id) / "check" =>
      authed(req)(_ => monitors.checkSoon(id).ifM(Accepted(), err(Status.NotFound, "監視が見つからないか、止めています")))

    case req @ GET -> Root / "api" / "incidents" =>
      authed(req)(_ => monitors.incidents(param(req, "monitor").flatMap(s => scala.util.Try(UUID.fromString(s)).toOption),
        param(req, "open").contains("true"), param(req, "limit").flatMap(_.toIntOption).map(_.max(1).min(500)).getOrElse(100)).flatMap(Ok(_)))

    // ------------------------------------------------------------------ 通知先

    case req @ GET -> Root / "api" / "notifiers" => authed(req)(_ => notifiers.list.flatMap(ns => Ok(ns.map(_.dto))))

    case req @ POST -> Root / "api" / "notifiers" =>
      authed(req)(u => body[NotifierReq](req)(r => prepare(u, None, r).flatMap(valid(_)((n, s) =>
        notifiers.save(None, n, s).flatMap(x => Created(x.get.dto))))))

    case req @ PUT -> Root / "api" / "notifiers" / UuidVar(id) =>
      authed(req)(u => notifiers.find(id).flatMap {
        case None => err(Status.NotFound, "通知先が見つかりません")
        case cur  => body[NotifierReq](req)(r => prepare(u, cur, r).flatMap(valid(_)((n, s) =>
          notifiers.save(Some(id), n, s).flatMap(_.fold(err(Status.NotFound, "通知先が見つかりません"))(x => dropHook(u, cur, Some(x.config)) *> Ok(x.dto))))))
      })

    case req @ DELETE -> Root / "api" / "notifiers" / UuidVar(id) =>
      authed(req)(u => notifiers.find(id).flatMap(cur => notifiers.delete(id).ifM(dropHook(u, cur, None) *> NoContent(), err(Status.NotFound, "通知先が見つかりません"))))

    // 試しに送る（行列を通さず、その場で送って結果を返す）
    case req @ POST -> Root / "api" / "notifiers" / UuidVar(id) / "test" =>
      authed(req)(_ => testAllowed(id).ifM(notifiers.find(id).flatMap {
        case None => err(Status.NotFound, "通知先が見つかりません")
        case Some(n) => senders.send(n, senders.testPayload(n)).attempt.flatMap {
            case Right(_) => NoContent()
            case Left(e)  => err(Status.BadGateway, s"送れませんでした: ${Senders.describe(e)}")
          }
      }, err(Status.TooManyRequests, "試しの送信は10秒に1回までです")))

    // ------------------------------------------------------------------ ステータスページ

    case req @ GET -> Root / "api" / "pages" => authed(req)(_ => pages.list.flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "pages" =>
      authed(req)(_ => body[StatusPageReq](req)(r => valid(validPage(r))(p =>
        pages.save(None, p).flatMap(_.fold(err(Status.Conflict, _), x => Created(x.get))))))

    case req @ PUT -> Root / "api" / "pages" / UuidVar(id) =>
      authed(req)(_ => body[StatusPageReq](req)(r => valid(validPage(r))(p =>
        (pages.save(Some(id), p) <* publicCache.forget(id)).flatMap(_.fold(err(Status.Conflict, _), _.fold(err(Status.NotFound, "ページが見つかりません"))(Ok(_)))))))

    case req @ DELETE -> Root / "api" / "pages" / UuidVar(id) =>
      authed(req)(_ => (pages.delete(id) <* publicCache.forget(id)).ifM(NoContent(), err(Status.NotFound, "ページが見つかりません")))
  }
