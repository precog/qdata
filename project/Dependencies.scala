package qdata.project

import sbt._

object Dependencies {
  private val predefVersion     = "0.1.1"
  private val scalazVersion     = "7.2.30"
  private val scalacheckVersion = "1.14.3"
  private val scodecVersion     = "1.11.7"
  private val spireVersion      = "0.17.0-M1"
  private val specsVersion      = "4.8.1"
  private val jawnVersion       = "1.0.0"

  def core = Seq(
    "com.slamdata"  %% "slamdata-predef" % predefVersion,
    "org.scodec"    %% "scodec-core"     % scodecVersion,
    "org.typelevel" %% "spire"           % spireVersion
  )

  def time = Seq(
    "com.slamdata"   %% "slamdata-predef"   % predefVersion,
    "org.scalacheck" %% "scalacheck"        % scalacheckVersion % Test,
    "org.scalaz"     %% "scalaz-core"       % scalazVersion,
    "org.specs2"     %% "specs2-core"       % specsVersion      % Test,
    "org.specs2"     %% "specs2-scalacheck" % specsVersion      % Test
  )

  def json = Seq(
    "org.typelevel" %% "jawn-ast"  % jawnVersion,
    "org.typelevel" %% "jawn-util" % jawnVersion
  )

  def tectonic = Seq(
    "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.3"
  )
}
