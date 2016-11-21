println("rec-done")
val attr = self.attr
for {
  fsc  <- attr.$[FScape]("fscape")
  loc  <- attr.$[ArtifactLocation]("dir")
  ens  <- attr.$[Ensemble]("context")
  done <- fsc.attr.get("done")
} {
  ens.playing match {
    case BooleanObj.Var(vr) => vr() = false
    case _ => println("Ensemble listen - playing not mutable")
  }

  val df = new java.text.SimpleDateFormat(
    "'zerophase-'yyMMdd_HHmmss'.aif'", java.util.Locale.US)

  import de.sciss.file._

  val dir = loc.value / "anemone" / "rec"
  require(dir.isDirectory)

  def mkChild(): File = {
    val name  = df.format(new java.util.Date)
    val child = dir / name
    if (child.exists()) {
      Thread.sleep(500)
      mkChild()
    } else child
  }

  val art = Artifact(loc, mkChild())
  fsc .attr.put("file-out", art)
  done.attr.put("file"    , art)

  val config = de.sciss.fscape.stream.Control.Config()
  config.useAsync = false
  fsc.run(config)
}