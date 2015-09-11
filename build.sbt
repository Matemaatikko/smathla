import sbt._
import Keys._
import BuildProperties._

scalaVersion := "2.11.7"

name := projectNameProperty

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

libraryDependencies ++= Dependencies.dependencies

