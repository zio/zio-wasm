import BuildHelper._

inThisBuild(
  List(
    organization := "com.ziverge"
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check")

addCommandAlias(
  "testJVM",
  ";zioParserJVM/test; calibanParser/test"
)

val zioVersion       = "2.0.9"
val zioNioVersion    = "2.0.1"
val zioParserVersion = "0.1.8+36-628c70b0+20230305-0911-SNAPSHOT"

lazy val root =
  (project in file("."))
    .settings(stdSettings("zio-wasm"))
    .settings(dottySettings)
    .settings(
      scalaVersion       := ScalaDotty,
      crossScalaVersions := Nil,
      libraryDependencies ++= Seq(
        "dev.zio" %% "zio"          % zioVersion,
        "dev.zio" %% "zio-nio"      % zioNioVersion,
        "dev.zio" %% "zio-parser"   % zioParserVersion,
        "dev.zio" %% "zio-streams"  % zioVersion,
        "dev.zio" %% "zio-test"     % zioVersion % Test,
        "dev.zio" %% "zio-test-sbt" % zioVersion % Test
      ),
      testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
    )
