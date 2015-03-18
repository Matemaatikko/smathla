import sbt._
import Keys._
import BuildProperties._

scalaVersion := "2.10.4"

name := projectNameProperty

version := propertyOption("project.version").getOrElse("1.0")

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xlint",
  "-language:_",
  "-target:jvm-1.6",
  "-encoding", "UTF-8"
)

libraryDependencies ++= Dependencies.dependencies

