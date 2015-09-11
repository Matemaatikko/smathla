import sbt.Keys._
import sbt._

object Dependencies {

  resolvers += "Java" at "http://download.java.net/maven/2/"

  // Libraries
  object Version {
    val scalaTest = "2.2.4"
    val mockito = "1.9.5"
    val scalaLogging = "2.1.2"
  }

  val scalaTest = "org.scalatest" % "scalatest_2.11" % Version.scalaTest
  val mockito = "org.mockito" % "mockito-all" % Version.mockito
  val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging-slf4j" % Version.scalaLogging

  // Project
  val dependencies = Seq(scalaLogging, scalaTest, mockito % "test")
}
