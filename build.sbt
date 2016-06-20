name := "fancycase"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

lazy val fancycase = project in file(".") settings Seq(
	scalaVersion := "2.11.8",
	organization := "com.transpaygo"
)


libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
