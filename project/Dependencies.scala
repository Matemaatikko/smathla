import sbt.Keys._
import sbt._

object Dependencies {

  resolvers += "Java" at "http://download.java.net/maven/2/"

  // Libraries
  object Version {
    val scalatest = "2.0"
    val mockito = "1.9.5"
    val scalaLogging = "2.1.2"
  }

  val scalatest = "org.scalatest" % "scalatest_2.10" % Version.scalatest
  val mockito = "org.mockito" % "mockito-all" % Version.mockito
  val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging-slf4j" % Version.scalaLogging

  // Project
  libraryDependencies ++= Seq(scalaLogging, scalatest % "test", mockito % "test")
}
