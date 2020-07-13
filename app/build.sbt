lazy val baseName   = "Negatum"
lazy val baseNameL  = baseName.toLowerCase

//lazy val coreVersion  = "0.15.3"
lazy val appVersion   = "0.16.0"

lazy val commonSettings = Seq(
  organization        := "de.sciss",
  scalaVersion        := "2.13.1",
  crossScalaVersions  := Seq("2.13.1", "2.12.11"),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-Xsource:2.13", "-encoding", "utf8", "-Xlint"),
  homepage            := Some(url(s"https://git.iem.at/sciss/${name.value}")),
  licenses            := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  updateOptions       := updateOptions.value.withLatestSnapshots(false)
)

lazy val deps = new {
  val app = new {
    val dsp                     = "1.3.2"
    val fileUtil                = "1.1.4"
    val kollFlitz               = "0.2.3"
    val libSVM                  = "3.23"
    val melliteApp              = "2.48.2"
    val scalaCollider           = "1.28.5"
    val ugens                   = "1.19.7"
    val scopt                   = "3.7.1"
  }
  val test = new {
    val trace                   = "0.4.0"
  }
}

lazy val app = project.in(file("."))
  .settings(commonSettings)
  .settings(assemblySettings)
  .settings(
    name        := s"$baseName-App",
    description := "Negatum sound piece / stand alone application",
    version     := appVersion,
    libraryDependencies ++= Seq(
//      "de.sciss"          %% "negatum-core"               % coreVersion,
//      "de.sciss"          %% "negatum-views"              % coreVersion,
      "de.sciss"          %% "mellite-app"                % deps.app.melliteApp,
      "de.sciss"          %% "scalacollider"              % deps.app.scalaCollider,
      "de.sciss"          %% "scalacolliderugens-core"    % deps.app.ugens,
      "de.sciss"          %% "scalacolliderugens-plugins" % deps.app.ugens,
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
