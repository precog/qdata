resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.bintrayRepo("slamdata-inc", "maven-public")

addSbtPlugin("com.eed3si9n"    % "sbt-assembly"  % "0.14.10")
addSbtPlugin("com.eed3si9n"    % "sbt-buildinfo" % "0.9.0")
addSbtPlugin("io.get-coursier" % "sbt-coursier"  % "2.0.0-RC2-1")
addSbtPlugin("com.slamdata"    % "sbt-slamdata"  % "2.5.6")
