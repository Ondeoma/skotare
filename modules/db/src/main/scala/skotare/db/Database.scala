package skotare.db

import cats.effect.*
import com.zaxxer.hikari.HikariConfig
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import org.flywaydb.core.Flyway

final case class DbConfig(url: String, user: String, password: String, maxPoolSize: Int = 10)

object Database:

  def migrate(cfg: DbConfig): IO[Int] =
    IO.blocking(Flyway.configure().dataSource(cfg.url, cfg.user, cfg.password).load().migrate().migrationsExecuted)

  def transactor(cfg: DbConfig): Resource[IO, HikariTransactor[IO]] =
    val hc = new HikariConfig()
    hc.setJdbcUrl(cfg.url)
    hc.setUsername(cfg.user)
    hc.setPassword(cfg.password)
    hc.setMaximumPoolSize(cfg.maxPoolSize)
    hc.setDriverClassName("org.postgresql.Driver")
    hc.setConnectionTimeout(5000)
    hc.setValidationTimeout(3000)
    hc.setKeepaliveTime(60000)
    hc.setMaxLifetime(600000)
    // SQL ごとにトレースの子のスパンと所要時間のメトリクスを残す（skorimako）
    HikariTransactor.fromHikariConfig[IO](hc, Some(skotare.observe.DbTelemetry.logHandler))

/** 各メソッドを1トランザクションで実行するストアの共通部分. */
abstract class TxStore(xa: Transactor[IO]):
  protected def tx[A](c: ConnectionIO[A]): IO[A] = c.transact(xa)

/** SQL の部品. */
object Sql:
  /** set a = 1, b = 2（空のリストは渡さないこと）. */
  def set(fs: List[Fragment]): Fragment = fr"set" ++ fs.reduce(_ ++ fr"," ++ _)

  /** ノードが止まったとみなすまでの、心拍のない時間. */
  val AliveWindow: Fragment = fr"now() - interval '90 seconds'"

  /** 拠点の行（mr）が、確かめる時刻を間隔（監視 m）の3倍・最低2分より過ぎている（確認が回っていない. 古い票として数えない）. */
  val stale: Fragment = fr"mr.next_check_at < now() - make_interval(secs => greatest(m.interval_sec * 3, 120))"

  /** 拠点で動いているノードの数（region は拠点の名前の式）. */
  def aliveNodes(region: Fragment): Fragment =
    fr"(select count(*)::int from node n where n.region =" ++ region ++ fr"and n.seen_at >" ++ AliveWindow ++ fr")"

  /** 部分一致の LIKE のパターン（% と _ はそのままの文字として探す）. */
  def like(q: String): String = "%" + q.trim.replace("\\", "\\\\").replace("%", "\\%").replace("_", "\\_") + "%"
