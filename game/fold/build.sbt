libraryDependencies ++= Seq(
//  "org.wvlet.airframe" % "airframe_2.13" % "24.3.0",

  "co.fs2" %% "fs2-io" % "3.10.2",

  "com.googlecode.lanterna" % "lanterna" % "3.1.2",
  "org.scalafx" %% "scalafx" % "22.0.0-R33",

  "org.scalatest" %% "scalatest" % "3.2.18" % Test
)

// Compile / mainClass := Some("urru.game.fold.ui.lanterna.Main") // run / fork := false
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
