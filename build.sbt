
organization := "org.kframework"

name := "kale"

scalaVersion := "2.12.2"


resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += Resolver.mavenLocal

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",

  "io.circe" %% "circe-core" % "0.8.0",
  "io.circe" %% "circe-parser" % "0.8.0",

  "org.kframework.k" %% "kore" % "0.6-SNAPSHOT"
)

lazy val installZ3 = taskKey[Unit]("Install Z3 theorem prover")

installZ3 := {
  "./installZ3.sh" !
}

(test in Test) := (test in Test).dependsOn(installZ3).value

// Your profile name of the sonatype account. The default is the same with the organization value
// sonatypeProfileName := "org.kframework.kale"
