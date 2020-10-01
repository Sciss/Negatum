lazy val baseName   = "Negatum"
lazy val baseNameL  = baseName.toLowerCase

lazy val coreVersion = "1.0.0-SNAPSHOT"

lazy val commonSettings = Seq(
  version             := coreVersion,
  organization        := "de.sciss",
  scalaVersion        := "2.13.3",
  crossScalaVersions  := Seq("2.13.3", "2.12.12"),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-Xsource:2.13", "-encoding", "utf8", "-Xlint"),
  homepage            := Some(url(s"https://git.iem.at/sciss/${name.value}")),
  licenses            := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  updateOptions       := updateOptions.value.withLatestSnapshots(false)
)

lazy val deps = new {
  val core = new {
    val fileCache               = "1.0.0"
    val fscape                  = "3.0.0-SNAPSHOT"
    val melliteCore             = "3.0.0-SNAPSHOT"
    val soundProcesses          = "4.0.0-SNAPSHOT"
    val ugens                   = "1.19.8"
  }
  val views = new {
    def melliteCore: String     = core.melliteCore
    val sonogram                = "2.0.0"
    def soundProcesses: String  = core.soundProcesses
  }
}

lazy val root = project.in(file("."))
  .aggregate(core, views)
  .dependsOn(core, views)
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name              := baseName,
    description       := "Genetic Algorithms",
//    packagedArtifacts := Map.empty
    publishArtifact in (Compile, packageBin) := false, // there are no binaries
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false, // there are no sources
    autoScalaLibrary := false,
  )

lazy val core = project.withId(s"$baseNameL-core").in(file("core"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name        := s"$baseName-core",
    description := "Genetic Algorithms (core abstractions)",
    libraryDependencies ++= Seq(
      "de.sciss"  %% "filecache-txn"            % deps.core.fileCache,
      "de.sciss"  %% "fscape-lucre"             % deps.core.fscape,
      "de.sciss"  %% "mellite-core"             % deps.core.melliteCore,
      "de.sciss"  %% "scalacolliderugens-core"  % deps.core.ugens,
      "de.sciss"  %  "scalacolliderugens-spec"  % deps.core.ugens,
      "de.sciss"  %% "soundprocesses-core"      % deps.core.soundProcesses,
    )
  )

lazy val views = project.withId(s"$baseNameL-views").in(file("views"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name        := s"$baseName-views",
    description := "Genetic Algorithms (GUI components)",
    libraryDependencies ++= Seq(
      "de.sciss"  %% "mellite-core"             % deps.views.melliteCore,
      "de.sciss"  %% "sonogramoverview"         % deps.views.sonogram,
      "de.sciss"  %% "soundprocesses-views"     % deps.views.soundProcesses,
    )
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
