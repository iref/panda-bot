import scoverage.ScoverageKeys._

lazy val buildSettings = Seq(
  organization := "me.janferko",
  version := "1.0",
  scalaVersion := "2.12.2",
  crossScalaVersions := Seq("2.12.2", "2.11.11")
)

lazy val compilerOptions = Seq(
  "-encoding", "UTF-8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Yrangepos",
  "-Ywarn-numeric-widen"
)

lazy val repositories = Seq(
  "typesafe" at "http://repo.typesafe.com/typesafe/releases/",
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

lazy val dependencies = Seq(
  "org.typelevel" %% "cats-core"       % "0.9.0",
  "io.reactivex"  %% "rxscala"         % "0.26.5",
  "com.lihaoyi"   %% "fastparse"       % "0.4.3",
  "org.scalatest" %% "scalatest"       % "3.0.3" % "test",
  "org.mockito"   %  "mockito-all"     % "1.9.5" % "test"
)

lazy val commonSettings = Seq(
  resolvers ++= repositories,
  libraryDependencies ++= dependencies,
  scapegoatVersion := "1.3.0",
  scalacOptions ++= compilerOptions,
  scalacOptions in (Compile, console) ~= (_ filterNot (_ == "-Ywarn-unused-imports")),
  scalacOptions in (Compile, console) += "-Yrepl-class-based"
)

lazy val scoverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := true,
  coverageHighlighting := scalaBinaryVersion.value != "2.11"
)

lazy val settings = commonSettings ++ buildSettings

lazy val pandaBot = project.in(file("."))
  .settings(moduleName := "PandaBot")
  .settings(settings)
  .aggregate(core, examples)
  .dependsOn(core, examples)

lazy val core = project.in(file("core"))
  .settings(moduleName := "core")
  .settings(settings: _*)
  .settings(scoverageSettings: _*)

lazy val examples = project.in(file("examples"))
  .settings(moduleName := "examples")
  .settings(settings: _*)
  .dependsOn(core)
  .enablePlugins(JavaAppPackaging)
