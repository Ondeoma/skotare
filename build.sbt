ThisBuild / scalaVersion := "3.3.7"
ThisBuild / organization := "io.github.ondeoma"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / licenses := List("MIT" -> url("https://opensource.org/licenses/MIT"))
ThisBuild / homepage := Some(url("https://github.com/Ondeoma/skotare"))
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:imports", "-no-indent")

val http4sV = "1.0.0-M49"
val circeV = "0.14.16"
val doobieV = "1.0.0-RC12"
val ceV = "3.7.1"
val munitV = "1.3.6"
val flywayV = "11.3.0"
val otel4sV = "1.2.0"
val otelV = "1.68.0"
val otelInstV = "2.33.1-alpha"

// sbirds シリーズの共通ライブラリ（GitHub のリポジトリを直接参照する. 手元の sbirds-lib で試すときは SBIRDS_LIB=../sbirds-lib）
lazy val sbirdsLib = sys.env
  .get("SBIRDS_LIB")
  .map(p => file(p).getCanonicalFile.toURI)
  .getOrElse(uri("https://github.com/Ondeoma/sbirds-lib.git#v0.4.0"))
lazy val libSharedJVM = ProjectRef(sbirdsLib, "sharedJVM")
lazy val libSharedJS = ProjectRef(sbirdsLib, "sharedJS")
lazy val libObserve = ProjectRef(sbirdsLib, "observe")
lazy val libAuth = ProjectRef(sbirdsLib, "auth")
lazy val libHttp = ProjectRef(sbirdsLib, "http")
lazy val libMcp = ProjectRef(sbirdsLib, "mcp")
lazy val libPush = ProjectRef(sbirdsLib, "push")
lazy val libWeb = ProjectRef(sbirdsLib, "web")
lazy val libLinks = ProjectRef(sbirdsLib, "links")
lazy val libDb = ProjectRef(sbirdsLib, "db")
lazy val libOauth = ProjectRef(sbirdsLib, "oauth")
lazy val libDevidp = ProjectRef(sbirdsLib, "devidp")
lazy val libNet = ProjectRef(sbirdsLib, "net")

// http4s のサーバー側（ルートの DSL と JSON）
val http4sServer = Seq(
  "org.http4s" %% "http4s-dsl" % http4sV,
  "org.http4s" %% "http4s-circe" % http4sV,
  "org.http4s" %% "http4s-server" % http4sV
)

lazy val commonTest = Seq(
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % munitV % Test,
    "org.typelevel" %% "munit-cats-effect" % "2.2.1" % Test
  ),
  Test / fork := true
)

// バックエンドと画面(Scala.js)で共有する API の型
lazy val shared = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/shared"))
  .jvmConfigure(_.dependsOn(libSharedJVM))
  .jsConfigure(_.dependsOn(libSharedJS))
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % circeV,
      "io.circe" %%% "circe-parser" % circeV
    )
  )

// 画面: Scalactive の SPA（frontend/ の Vite から読み込む）
lazy val web = (project in file("modules/web"))
  .enablePlugins(ScalaJSPlugin, ScalactivePlugin)
  .dependsOn(shared.js, libWeb)
  .settings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "io.github.ondeoma" %%% "scalactive" % "0.7.0"
    )
  )

// 監視の確かめ方（HTTP(S)・TCP・DNS・PING. JDK の機能で行う. 宛先の確認は sbirds-lib の net）
lazy val core = (project in file("modules/core"))
  .dependsOn(shared.jvm, libNet)
  .settings(commonTest)
  .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % ceV)

lazy val db = (project in file("modules/db"))
  .dependsOn(core, libObserve, libDb, libOauth)
  .settings(commonTest)
  .settings(
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-core" % doobieV,
      "org.tpolecat" %% "doobie-hikari" % doobieV,
      "org.tpolecat" %% "doobie-postgres" % doobieV,
      "org.tpolecat" %% "doobie-postgres-circe" % doobieV,
      "org.typelevel" %% "cats-effect" % ceV,
      "org.postgresql" % "postgresql" % "42.7.14",
      "org.flywaydb" % "flyway-core" % flywayV,
      "org.flywaydb" % "flyway-database-postgresql" % flywayV % Runtime
    )
  )

// サービス固有の鍵（API トークンの接頭辞・秘密の値の暗号化の salt）. OIDC などは sbirds-lib の auth
lazy val auth = (project in file("modules/auth"))
  .dependsOn(libAuth)
  .settings(commonTest)

// 開発・テスト用の簡易 OIDC プロバイダ（本番では使わない. 中身は sbirds-lib の devidp）
lazy val devidp = (project in file("modules/devidp"))
  .dependsOn(libDevidp)
  .settings(
    Compile / mainClass := Some("skotare.devidp.DevIdpMain"),
    fork := true
  )

lazy val api = (project in file("modules/api"))
  .dependsOn(core, db, auth, shared.jvm, libObserve, libHttp, libMcp, libPush, libLinks, libDevidp % "test->compile", libOauth)
  .settings(commonTest)
  .settings(
    libraryDependencies ++= http4sServer
  )

lazy val app = (project in file("modules/app"))
  .dependsOn(api)
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-server" % http4sV,
      "org.http4s" %% "http4s-ember-client" % http4sV,
      "ch.qos.logback" % "logback-classic" % "1.5.19",
      "org.typelevel" %% "log4cats-slf4j" % "2.8.0"
    ),
    Compile / mainClass := Some("skotare.app.Main"),
    fork := true
  )

lazy val root = (project in file("."))
  .aggregate(shared.jvm, shared.js, web, core, db, auth, devidp, api, app)
  .settings(name := "skotare", publish / skip := true)
