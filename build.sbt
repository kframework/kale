
organization := "org.kframework"

name := "kale"

scalaVersion := "2.12.2"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",

  "io.circe" %% "circe-core" % "0.7.0",
  "io.circe" %% "circe-generic" % "0.7.0",
  "io.circe" %% "circe-parser" % "0.7.0",

  "org.kframework.k" %% "kore" % "0.5-SNAPSHOT"
)

//lazy val installZ3 = taskKey[Unit]("Install Z3 theorem prover")
//
//installZ3 := {
//  if(java.nio.file.Files.notExists(new File("z3").toPath())) {
//    println("Z3 not present, downloading...")
//    IO.unzipURL(new URL("https://github.com/Z3Prover/z3/releases/download/z3-4.5.0/z3-4.5.0-x64-osx-10.11.6.zip"), new File("z3"))
//    val f = new File("z3/z3-4.5.0-x64-osx-10.11.6/bin/z3")
//    f.setExecutable(true)
//    java.nio.file.Files.createDirectories(Path("z3/bin"))
//    java.nio.file.Files.createSymbolicLink(f.toPath, fn.toPath)
//  } else {
//    println("Path exists, no need to download.")
//  }
//}
//
//
//(test in Test) := (test in Test).dependsOn(installZ3).value




