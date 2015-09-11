import sbt._
import Keys._

scalaVersion := "2.11.7"

name := "smathla"

version := "1.0"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xlint",
  "-language:_",
  "-target:jvm-1.8",
  "-encoding", "UTF-8"
)
