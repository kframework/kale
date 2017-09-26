
organization := "org.kframework"

name := "kale"

scalaVersion := "2.12.2"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += Resolver.mavenLocal

lazy val kale = project.in(file(".")).dependsOn(kore)

lazy val kore = project in file("kore")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",

  "org.typelevel" %% "cats-core" % "1.0.0-MF",
  "org.typelevel" %% "kittens" % "1.0.0-RC0",

  "io.circe" %% "circe-core" % "0.9.0-M1",
  "io.circe" %% "circe-parser" % "0.9.0-M1"
)

lazy val installZ3 = taskKey[Unit]("Install Z3 theorem prover")

installZ3 := {
  "./installZ3.sh" !
}

(test in Test) := (test in Test).dependsOn(installZ3).value

// Your profile name of the sonatype account. The default is the same with the organization value
sonatypeProfileName := "org.kframework.kale"
