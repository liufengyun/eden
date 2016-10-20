name := "eden"
version := "0.1"
scalaVersion := "2.11.8"

libraryDependencies += "ch.epfl.lamp"  %% "dotty"     % "0.1-20160930-93d4c8c-NIGHTLY"
libraryDependencies += "org.scalameta" %% "scalameta" % "1.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

lazy val mavenPublishingSettings = Seq(
  credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
  publishMavenStyle := true,
  isSnapshot := version.value.contains("SNAPSHOT"),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) {
      Some("snapshots" at nexus + "content/repositories/snapshots")
    } else {
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  },
  homepage := Some(url("https://github.com/liufengyun/eden")),
  licenses += "BSD" -> url("https://github.com/liufengyun/eden/blob/master/LICENSE.md"),
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := {
    <url>https://github.com/liufengyun/eden</url>
    <scm>
      <url>http://github.com/liufengyun/eden</url>
      <connection>scm:git:git@github.com:liufengyun/eden.git</connection>
    </scm>
    <developers>
      <developer>
        <id>liufengyun</id>
        <name>Liu Fengyun</name>
        <url>http://fengy.me</url>
      </developer>
    </developers>
  }
)
