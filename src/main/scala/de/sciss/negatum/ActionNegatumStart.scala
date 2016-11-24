package de.sciss.negatum

import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.Action.Universe
import de.sciss.synth.proc.{Action, Folder}
import Composition.logComp

object ActionNegatumStart extends NamedAction("negatum-start") {
  def begin[S <: SSys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
    val dsl = DSL[S]
    import dsl._
    import universe._
    val attr            = self.attr
    val Some(fNegatum)  = attr.$[Folder]("negatum-folder")
    val Some(rec)       = attr.$[Action]("rec")
    val Some(done)      = attr.$[Action]("done")
    val hasOpenNeg = fNegatum.lastOption.exists {
      case n: Negatum[S] if n.attrInt("count", 0) < 1000 => true
      case _ => false
    }
    if (hasOpenNeg) {
      logComp("Continuing with Negatum evolution")
      val univ1 = Action.Universe(done, universe.workspace)
      done.execute(univ1)
    } else {
      logComp("Starting new Negatum analysis")
      val univ1 = Action.Universe(rec, universe.workspace)
      rec.execute(univ1)
    }
  }
}