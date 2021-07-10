lazy val baseName   = "Negatum"
lazy val baseNameL  = baseName.toLowerCase

lazy val projectVersion = "1.6.0"

ThisBuild / version       := projectVersion
ThisBuild / organization  := "de.sciss"
ThisBuild / versionScheme := Some("pvp")

lazy val commonSettings = Seq(
  scalaVersion        := "2.13.6",
  crossScalaVersions  := Seq("2.13.6", "2.12.14"),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-Xsource:2.13", "-encoding", "utf8", "-Xlint"),
  homepage            := Some(url(s"https://git.iem.at/sciss/${name.value}")),
  licenses            := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  updateOptions       := updateOptions.value.withLatestSnapshots(false)
)

lazy val deps = new {
  val core = new {
    val fileCache               = "1.1.1"
    val fscape                  = "3.7.0"
    val melliteCore             = "3.6.0"
    val soundProcesses          = "4.8.0"
    val ugens                   = "1.21.1"
  }
  val views = new {
    def melliteCore: String     = core.melliteCore
    val sonogram                = "2.2.1"
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
    Compile / packageBin / publishArtifact := false, // there are no binaries
    Compile / packageDoc / publishArtifact := false, // there are no javadocs
    Compile / packageSrc / publishArtifact := false, // there are no sources
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
  Test / publishArtifact := false,
  pomIncludeRepository := { _ => false },
  pomExtra := { val n = name.value
  <scm>
    <url>git@github.com:Sciss/{n}.git</url>
    <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
  </scm>
  <developers>
    <developer>
      <id>sciss</id>
      <name>Hanns Holger Rutz</name>
      <url>https://www.sciss.de</url>
    </developer>
  </developers>
  }
)
