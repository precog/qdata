package qdata.project

import sbt._

object Dependencies {
  private val predefVersion     = "0.0.4"
  private val scalazVersion     = "7.2.23"
  private val scalacheckVersion = "1.14.0"
  private val scodecVersion     = "1.10.3"
  private val spireVersion      = "0.14.1"
  private val specsVersion      = "4.2.0"

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
}
