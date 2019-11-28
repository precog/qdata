libraryDependencies += "org.kohsuke" % "github-api" % "1.101" exclude("org.jenkins-ci", "annotation-indexer")

disablePlugins(TravisCiPlugin)

scalacOptions --= Seq(
  "-Ywarn-unused:imports",
  "-Yinduction-heuristics",
  "-Ykind-polymorphism",
  "-Xstrict-patmat-analysis")

// sbt/sbt#2572
scalacOptions in (Compile, console) --= Seq(
  "-Yno-imports",
  "-Ywarn-unused:imports")
