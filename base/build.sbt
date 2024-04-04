libraryDependencies ++= Seq(
//  "org.wvlet.airframe" % "airframe_3" % "24.3.0",

  "io.github.greenleafoss" %% "green-leaf-mongo" % "0.1.16.1",
  "io.spray" %% "spray-json" % "1.3.6",
  "org.mongodb.scala" %% "mongo-scala-driver" % "5.0.0" cross CrossVersion.for3Use2_13,

  "org.scalatest" %% "scalatest" % "3.2.18" % Test
)
