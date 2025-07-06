import sbt._

object Dependencies {
  lazy val breeze = "org.scalanlp" %% "breeze" % "2.1.0"
  lazy val scaffeine = "com.github.blemale" %% "scaffeine" % "5.3.0"
  lazy val catseffect = "org.typelevel" %% "cats-effect" % "3.7-4972921"
  lazy val parsercombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  lazy val scalameta = "org.scalameta" %% "scalameta" % "4.13.8"
  lazy val munit = "org.scalameta" %% "munit" % "1.1.1"
}
