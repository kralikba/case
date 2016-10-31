name := "fancycase"

addCompilerPlugin("org.scalamacros" % "paradise" % "3.0.0-M4" cross CrossVersion.full)

lazy val fancycase = project in file(".") settings Seq(
	scalaVersion := "2.11.8",
	organization := "com.fonmoney"
)

libraryDependencies += "org.scalameta" %% "scalameta" % "1.2.0"
