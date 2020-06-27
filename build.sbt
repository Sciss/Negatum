lazy val baseName   = "Negatum"
lazy val baseNameL  = baseName.toLowerCase

lazy val coreVersion = "0.15.1"
lazy val appVersion  = "0.16.0-SNAPSHOT"

lazy val commonSettings = Seq(
  version             := coreVersion,
  organization        := "de.sciss",
  scalaVersion        := "2.13.1",
  crossScalaVersions  := Seq("2.13.1", "2.12.11"),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-Xsource:2.13", "-encoding", "utf8", "-Xlint"),
  homepage            := Some(url(s"https://git.iem.at/sciss/${name.value}")),
  licenses            := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  // resolvers           += "Oracle Repository" at "http://download.oracle.com/maven",  // required for sleepycat
  updateOptions       := updateOptions.value.withLatestSnapshots(false)
)

lazy val deps = new {
  val core = new {
    val fileCache               = "0.5.1"
    val fscape                  = "2.36.1"
    val melliteCore             = "2.45.0"
    val soundProcesses          = "3.35.5"
  }
  val views = new {
    def melliteCore: String     = core.melliteCore
    val sonogram                = "1.11.2"
    def soundProcesses: String  = core.soundProcesses
  }
  val app = new {
    val dsp                     = "1.3.2"
    val fileUtil                = "1.1.4"
    val kollFlitz               = "0.2.3"
    val libSVM                  = "3.23"
    val melliteApp              = "2.48.0-SNAPSHOT"
    val scalaCollider           = "1.28.5"
    val scalaColliderUGens      = "1.19.7"
    val scopt                   = "3.7.1"
  }
  val test = new {
    val trace                   = "0.4.0"
  }
}

lazy val root = project.in(file("."))
  .aggregate(core, views, app)
  .settings(
    name              := baseName,
    description       := "Genetic Algorithms",
    packagedArtifacts := Map.empty
  )

lazy val core = project.withId(s"$baseNameL-core").in(file("core"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name        := s"$baseName-core",
    description := "Genetic Algorithms (core abstractions)",
    libraryDependencies ++= Seq(
      "de.sciss"        %% "mellite-core"               % deps.core.melliteCore,
      "de.sciss"        %% "soundprocesses-core"        % deps.core.soundProcesses,
      "de.sciss"        %% "filecache-txn"              % deps.core.fileCache,
      "de.sciss"        %% "fscape-lucre"               % deps.core.fscape,
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
      "de.sciss"        %% "mellite-core"               % deps.views.melliteCore,
      "de.sciss"        %% "sonogramoverview"           % deps.views.sonogram,
      "de.sciss"        %% "soundprocesses-views"       % deps.views.soundProcesses,
    )
  )

lazy val app = project.withId(s"$baseNameL-app").in(file("app"))
  .dependsOn(core, views)
  .settings(commonSettings)
  .settings(assemblySettings)
  .settings(
    name        := s"$baseName-App",
    description := "Negatum sound piece / stand alone application",
    version     := appVersion,
    libraryDependencies ++= Seq(
      "de.sciss"          %% "mellite-app"                % deps.app.melliteApp,
      "de.sciss"          %% "scalacollider"              % deps.app.scalaCollider,
      "de.sciss"          %% "scalacolliderugens-core"    % deps.app.scalaColliderUGens,
      "de.sciss"          %% "scalacolliderugens-plugins" % deps.app.scalaColliderUGens,
      "de.sciss"          %% "scissdsp"                   % deps.app.dsp,
      "de.sciss"          %% "fileutil"                   % deps.app.fileUtil,   // (sbt bug)
      "de.sciss"          %% "kollflitz"                  % deps.app.kollFlitz,
      "com.datumbox"      %  "libsvm"                     % deps.app.libSVM,
      "com.github.scopt"  %% "scopt"                      % deps.app.scopt,           // parsing command line options
      "de.sciss"          %% "scalacollider-trace"        % deps.test.trace % Test
    ),
    packagedArtifacts := Map.empty    // don't send this to Sonatype
  )

// ---- assembly ----

lazy val assemblySettings = Seq(
  mainClass             in assembly := Some("de.sciss.negatum.gui.NegatumApp"),
  target                in assembly := baseDirectory.value,
  assemblyJarName       in assembly := s"$baseName.jar",
  assemblyMergeStrategy in assembly := {
    case PathList("org", "xmlpull", _ @ _*)                   => MergeStrategy.first
    case PathList("org", "w3c", "dom", "events", _ @ _*)      => MergeStrategy.first // bloody Apache Batik
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
