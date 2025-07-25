libraryDependencies ++= Seq(
//  "org.wvlet.airframe" % "airframe_2.13" % "24.11.0",

  "co.fs2" %% "fs2-io" % "3.12.0",

  "org.scalafx" %% "scalafx" % "24.0.0-R35",

  "org.http4s" %% "http4s-ember-client" % "1.0.0-M44",
  "org.http4s" %% "http4s-circe" % "1.0.0-M44",
  "org.http4s" %% "http4s-dsl" % "1.0.0-M44",

  "io.circe" %% "circe-core" % "0.14.13",
  "io.circe" %% "circe-generic" % "0.14.13",
  "io.circe" %% "circe-parser" % "0.14.13",

  "org.apache.kafka" % "kafka-clients" % "4.0.0",

  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

Compile / mainClass := Some("urru.game.fill.ui.scalafx.Main")
Compile / run / fork := true

//https://stackoverflow.com/questions/25144484/sbt-assembly-deduplication-found-error
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}

//https://github.com/marcuslonnberg/sbt-docker/blob/master/examples/sbt-assembly/build.sbt
docker / dockerfile := {
  // The assembly task generates a fat JAR file
  val artifact: File = assembly.value
  val artifactTargetPath = s"/app/${artifact.name}"

  //https://stackoverflow.com/questions/28676006/add-copy-files-with-sbt-native-packagers-docker-support
  val dockerFiles = {
    val resources = (Runtime / unmanagedResources).value
    val dockerFilesDir = new File(resources.find(_.getPath.contains("/docker")).get.getPath.replaceAll("docker/.*", "docker"))
    resources.filter(_.getPath.contains("/docker/")).map(r => dockerFilesDir.toURI.relativize(r.toURI).getPath -> r).toMap
  }

  new Dockerfile {
    from("openjdk:17-oracle")
    add(artifact, artifactTargetPath)

    add(dockerFiles("fill-2-168.txt"), "/txt/fill-2-168.txt")
    add(dockerFiles("fill-2-5.txt"), "/txt/fill-2-5.txt")
    add(dockerFiles("fill-3-12.txt"), "/txt/fill-3-12.txt")
    add(dockerFiles("fill-4-14.txt"), "/txt/fill-4-14.txt")
    add(dockerFiles("fill-7-1.txt"), "/txt/fill-7-1.txt")
    add(dockerFiles("fill-7-1.txt"), "/txt/fill-7-19.txt")
    add(dockerFiles("fill-jp-pb-3-125.txt"), "/txt/fill-jp-pb-3-125.txt")
    add(dockerFiles("fill-jp-pb-3-160.txt"), "/txt/fill-jp-pb-3-160.txt")
    add(dockerFiles("fill-btp-d-20240409-3.txt"), "/txt/fill-btp-d-20240409-3.txt")
    add(dockerFiles("fill.txt"), "/txt/fill.txt")

    entryPoint("java", "-jar", artifactTargetPath)
    workDir("/txt")
  }
}
