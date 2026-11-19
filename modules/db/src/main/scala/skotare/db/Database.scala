package skotare.db

import cats.effect.*
import com.zaxxer.hikari.HikariConfig
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import org.flywaydb.core.Flyway

final case class DbConfig(url: String, user: String, password: String, maxPoolSize: Int = 10)

object Database {

  def migrate(cfg: DbConfig): IO[Int] =
    IO.blocking(Flyway.configure().dataSource(cfg.url, cfg.user, cfg.password).load().migrate().migrationsExecuted)

  def transactor(cfg: DbConfig): Resource[IO, HikariTransactor[IO]] = {
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
    HikariTransactor.fromHikariConfig[IO](hc, Some(sbirds.observe.DbTelemetry.logHandler))
  }
}

/** 各メソッドを1トランザクションで実行するストアの共通部分. */
abstract class TxStore(xa: Transactor[IO]) {
  protected def tx[A](c: ConnectionIO[A]): IO[A] = c.transact(xa)
}
