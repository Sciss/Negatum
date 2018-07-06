lazy val baseName = "Negatum"

name               := baseName
version            := "0.6.0-SNAPSHOT"
organization       := "de.sciss"
scalaVersion       := "2.12.6"
description        := "Genetic Algorithms"
homepage           := Some(url(s"https://github.com/Sciss/${name.value}"))
licenses           := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt"))

resolvers          += "Oracle Repository" at "http://download.oracle.com/maven"  // required for sleepycat

// ---- main dependencies ----

lazy val melliteVersion         = "2.24.0"
lazy val soundProcessesVersion  = "3.20.2"
lazy val scalaColliderVersion   = "1.27.0"
lazy val ugensVersion           = "1.19.1"
lazy val dspVersion             = "1.3.0"
lazy val strugatzkiVersion      = "2.18.0"
lazy val fileCacheVersion       = "0.4.0"
lazy val fileUtilVersion        = "1.1.3"
lazy val kollFlitzVersion       = "0.2.2"
lazy val libSVMVersion          = "3.22"

// ---- test dependencies ----

lazy val traceVersion           = "0.3.0"

libraryDependencies ++= Seq(
  "de.sciss"        %% "mellite"                    % melliteVersion,
  "de.sciss"        %% "soundprocesses-core"        % soundProcessesVersion,
  "de.sciss"        %% "scalacollider"              % scalaColliderVersion,
  "de.sciss"        %% "scalacolliderugens-core"    % ugensVersion,
  "de.sciss"        %% "scalacolliderugens-plugins" % ugensVersion,
  "de.sciss"        %% "scissdsp"                   % dspVersion,
  "de.sciss"        %% "strugatzki"                 % strugatzkiVersion,
  "de.sciss"        %% "filecache-txn"              % fileCacheVersion,
  "de.sciss"        %% "fileutil"                   % fileUtilVersion,   // (sbt bug)
  "de.sciss"        %% "kollflitz"                  % kollFlitzVersion,
//  "tw.edu.ntu.csie" %  "libsvm"                  % libSVMVersion,
  "com.datumbox"    %  "libsvm"                     % libSVMVersion,
  "at.iem"          %% "scalacollider-trace"        % traceVersion % "test"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture", "-encoding", "utf8", "-Xlint")

// ---- assembly ----

mainClass             in assembly := Some("de.sciss.negatum.gui.NegatumApp")
target                in assembly := baseDirectory.value
assemblyJarName       in assembly := s"$baseName.jar"
assemblyMergeStrategy in assembly := {
  case PathList("org", "xmlpull", xs @ _*) => MergeStrategy.first
  case PathList("org", "w3c", "dom", "events", xs @ _*) => MergeStrategy.first // bloody Apache Batik
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}


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
