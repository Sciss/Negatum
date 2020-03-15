/*
 *  ActionHibernateRecDone.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum

import de.sciss.fscape.lucre.FScape
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.negatum.Hibernation.logComp
import de.sciss.synth.proc.Action.Universe
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc._

object ActionHibernateRecDone extends NamedAction("hibernate-rec-done") {
  def begin[S <: SSys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
//    val dsl = DSL[S]
    import universe._

    logComp("hibernate-rec-done")
    val attr           = self.attr

    val Some(fsc)   = attr.$[FScape]          ("fscape")
    val Some(artIn) = attr.$[Artifact]        ("file")
    val Some(loc)   = attr.$[ArtifactLocation]("dir")
    val Some(ens)   = attr.$[Ensemble]        ("context")
    val Some(done)  = fsc.attr.get("done")

    ens.stop()

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

    val artOut = Artifact(loc, mkChild())

    fsc .attr.put("file-in" , artIn )
    fsc .attr.put("file-out", artOut)
    done.attr.put("file-in" , artIn )
    done.attr.put("file-out", artOut)

    val config = de.sciss.fscape.stream.Control.Config()
    config.useAsync = false
    logComp("Starting FScape rendering...")

//    implicit val ctx = GenContext[S]

    implicit val u: Universe[S] = universe

    /* val renderFSc = */ fsc.run(config)
//    val selfH     = tx.newHandle(self)
//    var lastProg  = 0
//    renderFSc.reactNow { implicit tx => {
////      case FScape.Rendering.Progress(amt) =>
////        val amtI = (amt * 100 + 0.5).toInt
////        val amtM = amtI - (amtI % 10)
////        if (amtM != lastProg) {
////          logComp(s"FScape progress: $amtM %")
////          lastProg = amtM
////        }
//      case FScape.Rendering.Success =>
//        fscapeDone(selfH)
//
//      case FScape.Rendering.Failure(ex) =>
//        logCompErr("!! FScape failed:")
//        ex.printStackTrace()
//
//      case _ =>
//    }}
  }
}