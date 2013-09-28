name := "preamble"

version := "0.1-SNAPSHOT"

organization := "com.github.aloiscochard"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-unchecked", "-feature")

resolvers ++= Seq(
  "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

initialCommands in console := """
  import preamble._
  import view._
"""
