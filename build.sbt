name               := "Negatum"
version            := "0.1.0-SNAPSHOT"
organization       := "de.sciss"
scalaVersion       := "2.11.8"
crossScalaVersions := Seq("2.11.8", "2.10.6")
description        := "Genetic Algorithms"
homepage           := Some(url(s"https://github.com/Sciss/${name.value}"))
licenses           := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt"))

resolvers          += "Oracle Repository" at "http://download.oracle.com/maven"  // required for sleepycat

// ---- main dependencies ----

lazy val soundProcessesVersion  = "3.8.0"
lazy val fscapeVersion          = "2.2.1"

// ---- test dependencies ----

lazy val melliteVersion         = "2.6.0"   // GPL!

libraryDependencies ++= Seq(
  "de.sciss" %% "soundprocesses-core" % soundProcessesVersion,
  "de.sciss" %% "fscape"              % fscapeVersion,
  "de.sciss" %% "mellite"             % melliteVersion  % "test"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture", "-encoding", "utf8", "-Xlint")

// ---- publishing ----

publishMavenStyle := true

publishTo :=
  Some(if (isSnapshot.value)
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := { val n = name.value
<scm>
  <url>git@github.com:Sciss/{n}.git</url>
  <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
}
