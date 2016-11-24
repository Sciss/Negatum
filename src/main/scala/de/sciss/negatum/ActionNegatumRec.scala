/*
 *  ActionNegatumRec.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum

import de.sciss.lucre.artifact.ArtifactLocation
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.Action.Universe
import de.sciss.synth.proc.{Action, Ensemble, Proc}
import Composition.{logComp, mkDateString}
import de.sciss.synth.proc.Implicits._

object ActionNegatumRec extends NamedAction("negatum-rec") {
  def begin[S <: SSys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
    val dsl = DSL[S]
    import dsl._
    import universe._
    logComp("negatum-rec-begin")

    val attr          = self.attr
    val Some(ens)     = attr.$[Ensemble]        ("context")
    val Some(p)       = attr.$[Proc]            ("proc")
    val Some(dir)     = attr.$[ArtifactLocation]("dir")
    val Some(done)    = attr.$[Action]          ("done")

    import Util._
    import DefaultRandom._
    val recDur = rrand(5.0, 7.0)
    val micBus = rrand(0, 3)
    p.adjustDouble("rec-dur", recDur)
    p.adjustInt   ("bus-in" , micBus)

    val artRec = dir / s"anemone/rec/capture-negatum${mkDateString()}.aif"
    p   .attr.put("file", artRec)
    done.attr.put("file", artRec)

    ens.play()
  }
}