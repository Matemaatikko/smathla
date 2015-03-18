import sbt._
import Keys._
import BuildProperties._

scalaVersion := "2.11.6"

name := projectNameProperty

version := propertyOption("project.version").getOrElse("1.0")

scalacOptions ++= Seq(
  "-Xlint",
  "-language:_",
  "-target:jvm-1.8",
  "-encoding", "UTF-8"
)


