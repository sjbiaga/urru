libraryDependencies ++= Seq(
//  "org.wvlet.airframe" % "airframe_3" % "24.11.0",

  "co.fs2" %% "fs2-io" % "3.12.0",

  "io.github.greenleafoss" %% "green-leaf-mongo" % "0.1.16.1",
  "io.spray" %% "spray-json" % "1.3.6",
  "org.mongodb.scala" %% "mongo-scala-driver" % "5.5.1" cross CrossVersion.for3Use2_13,

  "io.circe" %% "circe-core" % "0.14.13",
  "io.circe" %% "circe-generic" % "0.14.13",
  "io.circe" %% "circe-parser" % "0.14.13",

  "org.http4s" %% "http4s-circe" % "1.0.0-M44",

  "com.github.fd4s" %% "fs2-kafka" % "3.7.0",
  "org.apache.kafka" % "kafka-clients" % "4.0.0",

  "com.github.pureconfig" %% "pureconfig-core" % "0.17.9",

  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)
