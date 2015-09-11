import Dependencies._
import sbt.Keys._
import sbt._

object MyBuild extends Build{

  lazy val root = Project("root", file("."))
    .settings( scalaVersion := "2.11.7" )
    .settings( libraryDependencies ++= dependencies )

}