/*
 *  ActionNegatumStart.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum

import de.sciss.lucre.stm.Folder
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.negatum.Composition.logComp
import de.sciss.synth.proc.Action
import de.sciss.synth.proc.Action.Universe

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
      case n: Negatum[S] =>
        val count = n.attrInt("count", 0)
        count < Composition.MaxNegatum
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