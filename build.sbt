name := "comonad2"

version := "0.1"

scalaVersion := "2.13.4"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

lazy val root = (project.in(file(".")).settings(commonSmlBuildSettings))
