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

Compile / mainClass := Some("urru.game.fold.ui.scalafx.Main")
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

    add(dockerFiles("fold-lv-1170.txt"), "/txt/fold-lv-1170.txt")
    add(dockerFiles("fold-lv-1171.txt"), "/txt/fold-lv-1171.txt")
    add(dockerFiles("fold-lv-1279-multi.txt"), "/txt/fold-lv-1279-multi.txt")
    add(dockerFiles("fold-lv-1279.txt"), "/txt/fold-lv-1279.txt")
    add(dockerFiles("fold-lv-6.txt"), "/txt/fold-lv-6.txt")
    add(dockerFiles("fold-lv-7.txt"), "/txt/fold-lv-7.txt")
    add(dockerFiles("fold-lv-9.txt"), "/txt/fold-lv-9.txt")
    add(dockerFiles("fold.txt"), "/txt/fold.txt")

    entryPoint("java", "-jar", artifactTargetPath)
    workDir("/txt")
  }
}
