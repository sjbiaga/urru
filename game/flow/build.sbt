libraryDependencies ++= Seq(
//  "org.wvlet.airframe" % "airframe_2.13" % "24.3.0",

  "co.fs2" %% "fs2-io" % "3.10.2",

  "com.googlecode.lanterna" % "lanterna" % "3.1.2",
  "org.scalafx" %% "scalafx" % "21.0.0-R32",

  "org.scalatest" %% "scalatest" % "3.2.18" % Test
)

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

    add(dockerFiles("flow-br-1.txt"), "/txt/flow-br-1.txt")
    add(dockerFiles("flow-br-2.txt"), "/txt/flow-br-2.txt")
    add(dockerFiles("flow-br-3.txt"), "/txt/flow-br-3.txt")
    add(dockerFiles("flow-br-4.txt"), "/txt/flow-br-4.txt")
    add(dockerFiles("flow-cl-1.txt"), "/txt/flow-cl-1.txt")
    add(dockerFiles("flow-cl-2.txt"), "/txt/flow-cl-2.txt")
    add(dockerFiles("flow-cl-3.txt"), "/txt/flow-cl-3.txt")
    add(dockerFiles("flow-cl-4.txt"), "/txt/flow-cl-4.txt")
    add(dockerFiles("flow-cl-5.txt"), "/txt/flow-cl-5.txt")
    add(dockerFiles("flow-cl-6.txt"), "/txt/flow-cl-6.txt")
    add(dockerFiles("flow-cl-7.txt"), "/txt/flow-cl-7.txt")
    add(dockerFiles("flow-cl-wc-2.txt"), "/txt/flow-cl-wc-2.txt")
    add(dockerFiles("flow-cl-wc-3.txt"), "/txt/flow-cl-wc-3.txt")
    add(dockerFiles("flow-weekly-htb-1.txt"), "/txt/flow-weekly-htb-1.txt")
    add(dockerFiles("flow-weekly-p-11.txt"), "/txt/flow-weekly-p-11.txt")
    add(dockerFiles("flow.txt"), "/txt/flow.txt")

    entryPoint("java", "-jar", artifactTargetPath)
    workDir("/txt")
  }
}
