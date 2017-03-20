
organization := "org.kframework.kale"

name := "kale"

scalaVersion := "2.12.1"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "org.kframework.k" % "minikore" % "1.0-SNAPSHOT"
