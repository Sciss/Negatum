lazy val baseName = "Negatum"

lazy val commonSettings = Seq(
  name                := baseName,
  version             := "0.7.1-SNAPSHOT",
  organization        := "de.sciss",
  scalaVersion        := "2.12.8",
  crossScalaVersions  := Seq("2.13.0", "2.12.8", "2.11.12"),
  description         := "Genetic Algorithms",
  homepage            := Some(url(s"https://git.iem.at/sciss/${name.value}")),
  licenses            := Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
  resolvers           += "Oracle Repository" at "http://download.oracle.com/maven"  // required for sleepycat
)

lazy val deps = new {
  val main = new {
    val dsp                 = "1.3.2"
    val fileCache           = "0.5.1"
    val fileUtil            = "1.1.3"
    val kollFlitz           = "0.2.3"
    val libSVM              = "3.23"
    val mellite             = "2.37.0"
    val scalaCollider       = "1.28.4"
    val scalaColliderUGens  = "1.19.5"
    val soundProcesses      = "3.29.3"
    val strugatzki          = "2.19.0"
  }
  val test = new {
    val trace               = "0.4.0"
  }
}

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(assemblySettings)
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "de.sciss"        %% "mellite"                    % deps.main.mellite,
      "de.sciss"        %% "soundprocesses-core"        % deps.main.soundProcesses,
      "de.sciss"        %% "scalacollider"              % deps.main.scalaCollider,
      "de.sciss"        %% "scalacolliderugens-core"    % deps.main.scalaColliderUGens,
      "de.sciss"        %% "scalacolliderugens-plugins" % deps.main.scalaColliderUGens,
      "de.sciss"        %% "scissdsp"                   % deps.main.dsp,
      "de.sciss"        %% "strugatzki"                 % deps.main.strugatzki,
      "de.sciss"        %% "filecache-txn"              % deps.main.fileCache,
      "de.sciss"        %% "fileutil"                   % deps.main.fileUtil,   // (sbt bug)
      "de.sciss"        %% "kollflitz"                  % deps.main.kollFlitz,
      //  "tw.edu.ntu.csie" %  "libsvm"                  % libSVM,
      "com.datumbox"    %  "libsvm"                     % deps.main.libSVM,
      "de.sciss"        %% "scalacollider-trace"        % deps.test.trace % Test
    ),
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xsource:2.13", "-encoding", "utf8", "-Xlint")
  )

// ---- assembly ----

lazy val assemblySettings = Seq(
  mainClass             in assembly := Some("de.sciss.negatum.gui.NegatumApp"),
  target                in assembly := baseDirectory.value,
  assemblyJarName       in assembly := s"$baseName.jar",
  assemblyMergeStrategy in assembly := {
    case PathList("org", "xmlpull", _ @ _*) => MergeStrategy.first
    case PathList("org", "w3c", "dom", "events", _ @ _*) => MergeStrategy.first // bloody Apache Batik
    case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.first
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  }
)

// ---- publishing ----

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := { val n = name.value
  <scm>
    <url>git@git.iem.at:sciss/{n}.git</url>
    <connection>scm:git:git@git.iem.at:sciss/{n}.git</connection>
  </scm>
  <developers>
    <developer>
      <id>sciss</id>
      <name>Hanns Holger Rutz</name>
      <url>http://www.sciss.de</url>
    </developer>
  </developers>
  }
)
