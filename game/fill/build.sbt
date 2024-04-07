libraryDependencies ++= Seq(
//  "org.wvlet.airframe" % "airframe_2.13" % "24.3.0",

  "co.fs2" %% "fs2-io" % "3.10.2",

  "com.googlecode.lanterna" % "lanterna" % "3.1.2",
  "org.scalafx" %% "scalafx" % "22.0.0-R33",

  "org.scalatest" %% "scalatest" % "3.2.18" % Test
)

// Compile / mainClass := Some("urru.game.fill.ui.lanterna.Main") // run / fork := false
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

    add(dockerFiles("fill-0-0.txt"), "/txt/fill-0-0.txt")
    add(dockerFiles("fill-2-168.txt"), "/txt/fill-2-168.txt")
    add(dockerFiles("fill-2-5.txt"), "/txt/fill-2-5.txt")
    add(dockerFiles("fill-3-12.txt"), "/txt/fill-3-12.txt")
    add(dockerFiles("fill-4-14.txt"), "/txt/fill-4-14.txt")
    add(dockerFiles("fill-7-1.txt"), "/txt/fill-7-1.txt")
    add(dockerFiles("fill-7-1.txt"), "/txt/fill-7-19.txt")
    add(dockerFiles("fill-jp-pb-3-125.txt"), "/txt/fill-jp-pb-3-125.txt")
    add(dockerFiles("fill.txt"), "/txt/fill.txt")

    entryPoint("java", "-jar", artifactTargetPath)
    workDir("/txt")
  }
}
