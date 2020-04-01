import scala.Predef._

import qdata.project.Dependencies

import java.lang.{Integer, String, Throwable}
import scala.{Boolean, List, Predef, None, Some, StringContext, sys, Unit}, Predef.{any2ArrowAssoc, assert, augmentString}
import scala.collection.Seq
import scala.collection.immutable.Map

import sbt._, Keys._
import sbt.std.Transform.DummyTaskMap
import sbt.TestFrameworks.Specs2

ThisBuild / githubRepository := "qdata"

ThisBuild / crossScalaVersions := Seq("2.12.10", "2.13.1")
ThisBuild / scalaVersion := "2.12.10"

// ThisBuild / githubWorkflowJavaVersions += "graalvm@20.0.0"

ThisBuild / scmInfo in ThisBuild := Some(ScmInfo(
  url("https://github.com/precog/qdata"),
  "scm:git@github.com:precog/qdata.git"))

val BothScopes = "test->test;compile->compile"

lazy val buildSettings = commonBuildSettings ++ Seq(
  organization := "com.precog",
  scalaOrganization := "org.scala-lang",
  scalacOptions --= Seq(
    "-Yliteral-types",
    "-Xstrict-patmat-analysis",
    "-Yinduction-heuristics",
    "-Ykind-polymorphism",
    "-Ybackend:GenBCode"),

  scalacOptions += "-target:jvm-1.8",

  // NB: -Xlint triggers issues that need to be fixed
  scalacOptions --= Seq("-Xlint"),

  logBuffered in Test := githubIsWorkflowBuild.value,

  console := { (console in Test).value }) // console alias test:console

// In Travis, the processor count is reported as 32, but only ~2 cores are
// actually available to run.
concurrentRestrictions in Global := {
  val maxTasks = 2
  if (githubIsWorkflowBuild.value)
    // Recreate the default rules with the task limit hard-coded:
    Seq(Tags.limitAll(maxTasks), Tags.limit(Tags.ForkedTestGroup, 1))
  else
    (concurrentRestrictions in Global).value
}

// copied from quasar
version in ThisBuild := {
  import scala.sys.process._

  val currentVersion = (version in ThisBuild).value
  if (!githubIsWorkflowBuild.value)
    currentVersion + "-" + "git rev-parse HEAD".!!.substring(0, 7)
  else
    currentVersion
}

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
lazy val commonSettings = buildSettings ++ assemblySettings

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
  .settings(aggregate in assembly := false)
  .settings(excludeTypelevelScalaLibrary)
  .aggregate(core, time, json, tectonic)

lazy val core = project
  .in(file("core"))
  .settings(name := "qdata-core")
  .dependsOn(time % BothScopes)
  .settings(commonSettings)
  .settings(publishTestsSettings)
  .settings(libraryDependencies ++= Dependencies.core)
  .settings(excludeTypelevelScalaLibrary)

lazy val time = project
  .in(file("time"))
  .settings(name := "qdata-time")
  .settings(commonSettings)
  .settings(publishTestsSettings)
  .settings(libraryDependencies ++= Dependencies.time)
  .settings(excludeTypelevelScalaLibrary)

lazy val json = project
  .in(file("json"))
  .settings(name := "qdata-json")
  .dependsOn(core % BothScopes)
  .settings(commonSettings)
  .settings(publishTestsSettings)
  .settings(libraryDependencies ++= Dependencies.json)
  .settings(excludeTypelevelScalaLibrary)

lazy val tectonic = project
  .in(file("tectonic"))
  .settings(name := "qdata-tectonic")
  .dependsOn(json, core % "test->test")
  .settings(commonSettings)
  .settings(publishTestsSettings)
  .settings(libraryDependencies ++= Dependencies.tectonic)
  .settings(libraryDependencies += "com.precog" %% "tectonic" % managedVersions.value("precog-tectonic"))
  .settings(excludeTypelevelScalaLibrary)
