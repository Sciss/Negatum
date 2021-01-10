/*
 *  ActionNegatumStart.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum

import de.sciss.lucre.stm.Folder
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.negatum.Composition.logComp
import de.sciss.proc.{Action, ActionRaw}
import de.sciss.proc.Action.Universe

object ActionNegatumStart extends NamedAction("negatum-start") {
  def begin[S <: SSys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
    val dsl = DSL[S]
    import dsl._
    import universe._
    val attr            = self.attr
    val Some(fNegatum)  = attr.$[Folder]("negatum-folder")
    val Some(rec)       = attr.$[ActionRaw]("rec")
    val Some(done)      = attr.$[ActionRaw]("done")
    val hasOpenNeg = fNegatum.lastOption.exists {
      case n: Negatum[S] =>
        val count = n.attrInt("count", 0)
        count < Composition.MaxNegatum
      case _ => false
    }
    implicit val u: Universe[S] = universe
    if (hasOpenNeg) {
      logComp("Continuing with Negatum evolution")
      val univ1 = Action.Universe(done)
      done.execute(univ1)
    } else {
      logComp("Starting new Negatum analysis")
      val univ1 = Action.Universe(rec)
      rec.execute(univ1)
    }
  }
}