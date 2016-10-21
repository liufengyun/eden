lazy val edenSetting = Seq(
  name := "eden",
  version := "0.1",
  organization := "me.fengy",
  scalaVersion := "2.11.8",

  libraryDependencies += "ch.epfl.lamp"  %% "dotty"     % "0.1-20160930-93d4c8c-NIGHTLY",
  libraryDependencies += "org.scalameta" %% "scalameta" % "1.2.0",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test",

  credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),

  publishMavenStyle := true,

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) {
      Some("snapshots" at nexus + "content/repositories/snapshots")
    } else {
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  },

  licenses += "BSD" -> url("https://github.com/liufengyun/eden/blob/master/LICENSE.md"),

  publishArtifact in Test := false,

  pomIncludeRepository := { _ => false },

  pomExtra := (
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
  )
)

lazy val macrosSetting = Seq(
  name := "macros",
  version := "0.1",
  scalaVersion := "2.11.8",

  resolvers += Resolver.url(
    "scalameta-bintray",
    url("https://dl.bintray.com/scalameta/maven")
  )(Resolver.ivyStylePatterns),

  libraryDependencies += "org.scalameta" %% "scalameta" % "1.2.0",
  addCompilerPlugin("org.scalameta" % "paradise_2.11.8" % "3.0.0.100")
)

val dottyVersion = "0.1-SNAPSHOT"

lazy val usageSetting = Seq(
  scalacOptions ++= Seq("-Xprint:frontend"),

  // Dotty version
  scalaVersion := dottyVersion,
  scalaOrganization := "me.fengy",

  // Dotty is compatible with Scala 2.11, as such you can use 2.11
  // binaries. However, when publishing - this version number should be set
  // to 0.1 (the dotty version number)
  scalaBinaryVersion := "2.11",

  // Don't import the stdlib for "scalaBinaryVersion"
  autoScalaLibrary := false,

  // Add resolver for Sonatype Snapshots
  resolvers += Resolver.sonatypeRepo("snapshots"),

  libraryDependencies ++= Seq(
    // Dotty depends on stdlib 2.11.5, best use that too (0.1-SNAPSHOT is
    // actually 2.11.5, published under ch.epfl.lamp)
    "ch.epfl.lamp" % "scala-library_2.11" % "0.1-SNAPSHOT",
    // Compiler on tool path
    "me.fengy" % "dotty_2.11" % dottyVersion % "scala-tool"
  ),

  // Bridge which allows REPL and compilation via dotty
  scalaCompilerBridgeSource := ("ch.epfl.lamp" % "dotty-bridge" % "0.1.1-20160906-75f4400-NIGHTLY" % "component").sources()
)

lazy val eden = (project in file(".")).
  settings(edenSetting: _*)

lazy val macros = (project in file("macros")).
  settings(macrosSetting: _*)

lazy val usage = (project in file("usage")).
  settings(usageSetting: _*).
  dependsOn(macros)

