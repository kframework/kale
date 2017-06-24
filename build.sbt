
organization := "org.kframework"

name := "kale"

scalaVersion := "2.12.2"


resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

//resolvers += "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"

resolvers += Resolver.mavenLocal

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",

  "io.circe" %% "circe-core" % "0.7.0",
  "io.circe" %% "circe-generic" % "0.7.0",
  "io.circe" %% "circe-parser" % "0.7.0",

  "org.kframework.k" %% "kore" % "0.6-SNAPSHOT",

  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
)

lazy val installZ3 = taskKey[Unit]("Install Z3 theorem prover")

installZ3 := { "./installZ3.sh" !}

(test in Test) := (test in Test).dependsOn(installZ3).value




