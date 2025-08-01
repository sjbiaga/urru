ThisBuild / scalaVersion := "3.7.1"

Global / resolvers += "scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/"

val scala2Opts = Seq("-feature", "-language:implicitConversions", "-deprecation", "-Ytasty-reader")
val scala3Opts = Seq("-feature", "-language:implicitConversions", "-indent", "-Xwiki-syntax", "-Xmax-inlines", "128", "-new-syntax")

// val scala2Opts = Seq("-feature", "-language:implicitConversions", "-explaintypes", "-deprecation", "-Ytasty-reader")
// val scala3Opts = Seq("-feature", "-language:implicitConversions", "-explain-types", "-indent", "-new-syntax")

lazy val root = (project in file("."))
  .aggregate(`game-flow`, `game-fold`, `game-fill`)
  .settings(
    name := "urru",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "2.0",
    maxErrors := 5,
    scalaVersion := "3.7.1",
    javacOptions ++= Seq("-source", "16"),
    crossScalaVersions ++= Seq("2.13.16", "3.7.1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
  )

lazy val base = (project in file("base"))
  .settings(
    name := "base",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "2.0",
    maxErrors := 5,
    scalaVersion := "3.7.1",
    javacOptions ++= Seq("-source", "16"),
    crossScalaVersions ++= Seq("2.13.16", "3.7.1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
  )

lazy val grid = (project in file("grid"))
  .dependsOn(base)
  .settings(
    name := "grid",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "2.0",
    maxErrors := 5,
    scalaVersion := "3.7.1",
    javacOptions ++= Seq("-source", "16"),
    crossScalaVersions ++= Seq("2.13.16", "3.7.1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
  )

lazy val game = (project in file("game"))
  .dependsOn(grid)
  .settings(
    name := "game",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "2.0",
    maxErrors := 5,
    scalaVersion := "3.7.1",
    javacOptions ++= Seq("-source", "16"),
    crossScalaVersions ++= Seq("2.13.16", "3.7.1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
  )

lazy val `game-flow` = (project in file("game/flow"))
  .dependsOn(game)
  .enablePlugins(DockerPlugin)
  .settings(
    name := "game.flow",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "2.0",
    maxErrors := 5,
    scalaVersion := "3.7.1",
    javacOptions ++= Seq("-source", "16"),
    crossScalaVersions ++= Seq("2.13.16", "3.7.1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
  )

lazy val `game-fold` = (project in file("game/fold"))
  .dependsOn(game)
  .enablePlugins(DockerPlugin)
  .settings(
    name := "game.fold",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "2.0",
    maxErrors := 5,
    scalaVersion := "3.7.1",
    javacOptions ++= Seq("-source", "16"),
    crossScalaVersions ++= Seq("2.13.16", "3.7.1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
  )

lazy val `game-fill` = (project in file("game/fill"))
  .dependsOn(game)
  .enablePlugins(DockerPlugin)
  .settings(
    name := "game.fill",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "2.0",
    maxErrors := 5,
    scalaVersion := "3.7.1",
    javacOptions ++= Seq("-source", "16"),
    crossScalaVersions ++= Seq("2.13.16", "3.7.1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
  )

Compile / unmanagedSourceDirectories / excludeFilter := "sΠ"

// ThisBuild / evictionErrorLevel := Level.Info

Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / onChangedBuildSource := ReloadOnSourceChanges
//Global / onChangedBuildSource := IgnoreSourceChanges
