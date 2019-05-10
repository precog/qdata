package qdata.project

import sbt._

object Dependencies {
  private val predefVersion     = "0.0.7"
  private val scalazVersion     = "7.2.23"
  private val scalacheckVersion = "1.14.0"
  private val scodecVersion     = "1.11.3"
  private val spireVersion      = "0.16.0"
  private val specsVersion      = "4.2.0"
  private val jawnVersion       = "0.14.1"
  private val tectonicVersion   = IO.read(file("./tectonic-version")).trim

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
    "com.slamdata" %% "tectonic" % tectonicVersion
  )
}
