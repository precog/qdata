package qdata.project

import sbt._

object Dependencies {
  private val predefVersion = "0.0.4"
  private val spireVersion  = "0.14.1"

  def core = Seq(
    "com.slamdata"  %% "slamdata-predef" % predefVersion,
    "org.typelevel" %% "spire"           % spireVersion
  )
}
