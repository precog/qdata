package qdata.project

import sbt._

object Dependencies {
  private val predefVersion     = "0.0.7"
  private val scalazVersion     = "7.2.30"
  private val scalacheckVersion = "1.14.3"
  private val scodecVersion     = "1.11.4"
  private val spireVersion      = "0.16.2"
  private val specsVersion      = "4.8.3"
  private val jawnVersion       = "1.0.0"
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
