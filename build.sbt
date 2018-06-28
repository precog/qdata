import scala.Predef._

import qdata.project.Dependencies

import java.lang.{Integer, String, Throwable}
import scala.{Boolean, List, Predef, None, Some, StringContext, sys, Unit}, Predef.{any2ArrowAssoc, assert, augmentString}
import scala.collection.Seq
import scala.collection.immutable.Map

import sbt._, Keys._
import sbt.std.Transform.DummyTaskMap
import sbt.TestFrameworks.Specs2
import sbtrelease._, ReleaseStateTransformations._, Utilities._
import slamdata.SbtSlamData.transferPublishAndTagResources

val BothScopes = "test->test;compile->compile"

// Exclusive execution settings
lazy val ExclusiveTests = config("exclusive") extend Test

val ExclusiveTest = Tags.Tag("exclusive-test")

def exclusiveTasks(tasks: Scoped*) =
  tasks.flatMap(inTask(_)(tags := Seq((ExclusiveTest, 1))))

lazy val buildSettings = commonBuildSettings ++ Seq(
  organization := "com.slamdata",
  scalaOrganization := "org.scala-lang",
  scalacOptions --= Seq(
    "-Yliteral-types",
    "-Xstrict-patmat-analysis",
    "-Yinduction-heuristics",
    "-Ykind-polymorphism",
    "-Ybackend:GenBCode"),
  initialize := {
    val version = sys.props("java.specification.version")
    assert(
      Integer.parseInt(version.split("\\.")(1)) >= 8,
      "Java 8 or above required, found " + version)
  },

  scalacOptions += "-target:jvm-1.8",

  // NB: -Xlint triggers issues that need to be fixed
  scalacOptions --= Seq("-Xlint"),
  // NB: Some warts are disabled in specific projects. Here’s why:
  //   • AsInstanceOf   – wartremover/wartremover#266
  //   • others         – simply need to be reviewed & fixed
  wartremoverWarnings in (Compile, compile) --= Seq(
    Wart.Any,                   // - see wartremover/wartremover#263
    Wart.PublicInference,       // - creates many compile errors when enabled - needs to be enabled incrementally
    Wart.ImplicitParameter,     // - creates many compile errors when enabled - needs to be enabled incrementally
    Wart.ImplicitConversion,    // - see mpilquist/simulacrum#35
    Wart.Nothing),              // - see wartremover/wartremover#263
  // Normal tests exclude those tagged in Specs2 with 'exclusive'.
  testOptions in Test := Seq(Tests.Argument(Specs2, "exclude", "exclusive", "showtimes")),
  // Exclusive tests include only those tagged with 'exclusive'.
  testOptions in ExclusiveTests := Seq(Tests.Argument(Specs2, "include", "exclusive", "showtimes")),

  logBuffered in Test := isTravisBuild.value,

  console := { (console in Test).value }) // console alias test:console

// In Travis, the processor count is reported as 32, but only ~2 cores are
// actually available to run.
concurrentRestrictions in Global := {
  val maxTasks = 2
  if (isTravisBuild.value)
    // Recreate the default rules with the task limit hard-coded:
    Seq(Tags.limitAll(maxTasks), Tags.limit(Tags.ForkedTestGroup, 1))
  else
    (concurrentRestrictions in Global).value
}

// Tasks tagged with `ExclusiveTest` should be run exclusively.
concurrentRestrictions in Global += Tags.exclusive(ExclusiveTest)

// copied from quasar
version in ThisBuild := {
  import scala.sys.process._

  val currentVersion = (version in ThisBuild).value
  if (!isTravisBuild.value)
    currentVersion + "-" + "git rev-parse HEAD".!!.substring(0, 7)
  else
    currentVersion
}

useGpg in Global := {
  val oldValue = (useGpg in Global).value
  !isTravisBuild.value || oldValue
}

pgpSecretRing in Global := pgpPublicRing.value   // workaround for sbt/sbt-pgp#126

lazy val publishSettings = commonPublishSettings ++ Seq(
  performMavenCentralSync := false,   // basically just ignores all the sonatype sync parts of things
  publishAsOSSProject := true,
  organizationName := "SlamData Inc.",
  organizationHomepage := Some(url("http://slamdata.com")),
  homepage := Some(url("https://github.com/slamdata/qdata")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/slamdata/qdata"),
      "scm:git@github.com:slamdata/qdata.git")),
  bintrayCredentialsFile := {
    val oldValue = bintrayCredentialsFile.value
    if (!isTravisBuild.value)
      Path.userHome / ".bintray" / ".credentials"
    else
      oldValue
  })

lazy val assemblySettings = Seq(
  test in assembly := {},

  assemblyExcludedJars in assembly := {
    val cp = (fullClasspath in assembly).value
    cp filter { attributedFile =>
      val file = attributedFile.data

      val excludeByName: Boolean = file.getName.matches("""scala-library-2\.12\.\d+\.jar""")
      val excludeByPath: Boolean = file.getPath.contains("org/typelevel")

      excludeByName && excludeByPath
    }
  }
)

// Build and publish a project, excluding its tests.
lazy val commonSettings = buildSettings ++ publishSettings ++ assemblySettings

// not doing this causes NoSuchMethodErrors when using coursier
lazy val excludeTypelevelScalaLibrary =
  Seq(excludeDependencies += "org.typelevel" % "scala-library")

// Include to also publish a project's tests
lazy val publishTestsSettings = Seq(
  publishArtifact in (Test, packageBin) := true)

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .settings(noPublishSettings)
  .settings(transferPublishAndTagResources)
  .settings(aggregate in assembly := false)
  .settings(excludeTypelevelScalaLibrary)
  .aggregate(core, time)
  .enablePlugins(AutomateHeaderPlugin)

lazy val core = project
  .in(file("core"))
  .settings(name := "qdata-core")
  .dependsOn(time % BothScopes)
  .settings(commonSettings)
  .settings(libraryDependencies ++= Dependencies.core)
  .settings(excludeTypelevelScalaLibrary)
  .enablePlugins(AutomateHeaderPlugin)

lazy val time = project
  .in(file("time"))
  .settings(name := "qdata-time")
  .settings(commonSettings)
  .settings(publishTestsSettings)
  .settings(libraryDependencies ++= Dependencies.time)
  .settings(excludeTypelevelScalaLibrary)
  .enablePlugins(AutomateHeaderPlugin)
