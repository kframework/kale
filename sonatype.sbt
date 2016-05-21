// Your profile name of the sonatype account. The default is the same with the organization value
sonatypeProfileName := "org.kframework.kale"

// To sync with Maven central, you need to supply the following information:
pomExtra in Global := {
  <url>https://github.com/kframework/kale</url>
    <licenses>
      <license>
        <name>University of Illinois/NCSA Open Source License</name>
        <url>https://opensource.org/licenses/NCSA</url>
      </license>
    </licenses>
    <scm>
      <connection>https://github.com/kframework/kale.git</connection>
      <developerConnection>https://github.com/kframework/kale.git</developerConnection>
      <url>https://github.com/kframework/kale</url>
    </scm>
    <developers>
      <developer>
        <id>andreistefanescu</id>
        <name>Andrei Stefanescu</name>
        <url>http://fsl.cs.illinois.edu/index.php/Andrei_Stefanescu</url>
      </developer>
      <developer>
        <id>cosmin</id>
        <name>Cosmin Radoi</name>
        <url>http://cosmin.radoi.net</url>
      </developer>
    </developers>
}
