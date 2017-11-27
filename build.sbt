
organization := "org.kframework"

name := "kale"

scalaVersion := "2.12.4"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += Resolver.mavenLocal

lazy val kale = project.in(file(".")).dependsOn(kore)

lazy val kore = project in file("kore")

libraryDependencies ++= Seq(
  "org.typelevel" %% "discipline" % "0.7.+" % "test",
  "org.scalatest" %% "scalatest" % "3.0.+" % "test",

  "org.typelevel" %% "cats-core" % "1.0.+",
  "org.typelevel" %% "cats-laws" % "1.0.+" % "test",
  "org.typelevel" %% "kittens" % "1.0.+",

  "org.typelevel"  %% "squants"  % "1.3.0",

  "org.roaringbitmap" % "RoaringBitmap" % "0.6.+",

  "io.circe" %% "circe-core" % "0.9.+",
  "io.circe" %% "circe-parser" % "0.9.+"
)

lazy val installZ3 = taskKey[Unit]("Install Z3 theorem prover")

installZ3 := {
  "./installZ3.sh" !
}

(test in Test) := (test in Test).dependsOn(installZ3).value

// Your profile name of the sonatype account. The default is the same with the organization value
sonatypeProfileName := "org.kframework.kale"
