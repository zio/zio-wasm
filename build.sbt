import BuildHelper._

inThisBuild(
  List(
    organization  := "dev.zio",
    homepage      := Some(url("https://zio.dev/zio-wasm/")),
    licenses      := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers    := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      ),
      Developer(
        "vigoo",
        "Daniel Vigovszky",
        "daniel.vigovszky@gmail.com",
        url("https://vigoo.github.io/")
      )
    ),
    resolvers +=
      "Sonatype OSS Snapshots 01" at "https://s01.oss.sonatype.org/content/repositories/snapshots",
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    resolvers +=
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check")

val zioVersion        = "2.0.10"
val zioNioVersion     = "2.0.1"
val zioParserVersion  = "0.1.9"
val zioPreludeVersion = "1.0.0-RC18"

lazy val root = (project in file("."))
  .settings(
    name               := "zio-wasm",
    publish / skip     := true,
    crossScalaVersions := Nil
  )
  .aggregate(zioWasm, docs)

lazy val zioWasm =
  (project in file("zio-wasm"))
    .settings(stdSettings("zio-wasm"))
    .settings(dottySettings)
    .settings(
      scalaVersion       := ScalaDotty,
      crossScalaVersions := Nil,
      libraryDependencies ++= Seq(
        "dev.zio" %% "zio"          % zioVersion,
        "dev.zio" %% "zio-nio"      % zioNioVersion,
        "dev.zio" %% "zio-parser"   % zioParserVersion,
        "dev.zio" %% "zio-prelude"  % zioPreludeVersion,
        "dev.zio" %% "zio-streams"  % zioVersion,
        "dev.zio" %% "zio-test"     % zioVersion % Test,
        "dev.zio" %% "zio-test-sbt" % zioVersion % Test
      ),
      testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
    )

lazy val docs = project
  .in(file("zio-wasm-docs"))
  .settings(stdSettings("zio-wasm"))
  .settings(macroDefinitionSettings)
  .settings(
    scalaVersion                               := ScalaDotty,
    publish / skip                             := true,
    moduleName                                 := "zio-wasm-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    projectName                                := "ZIO WASM",
    mainModuleName                             := (zioWasm / moduleName).value,
    projectStage                               := ProjectStage.Development,
    docsPublishBranch                          := "main",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(zioWasm)
  )
  .dependsOn(zioWasm)
  .enablePlugins(WebsitePlugin)
