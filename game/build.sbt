libraryDependencies ++= Seq(
//  "org.wvlet.airframe" % "airframe_2.13" % "24.11.0",

  "io.github.greenleafoss" %% "green-leaf-mongo" % "0.1.16.1",
  "io.spray" %% "spray-json" % "1.3.6",
  "org.mongodb.scala" %% "mongo-scala-driver" % "5.5.1" cross CrossVersion.for3Use2_13,

  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)
