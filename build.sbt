name := "fancycase"

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalaVersion := "2.11.8"

organization := "com.fonmoney"

libraryDependencies <++= scalaVersion(version => Seq(
  "org.scala-lang" % "scala-reflect" % version,
  "org.scala-lang" % "scalap" % version,
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
))