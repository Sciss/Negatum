/*
 *  ActionNegatumRec.scala
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

import de.sciss.lucre.artifact.ArtifactLocation
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.negatum.Composition.{logComp, mkDateString}
import de.sciss.proc.Action.Universe
import de.sciss.proc.Implicits._
import de.sciss.proc.{ActionRaw, Ensemble, Proc}

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
    val Some(done)    = attr.$[ActionRaw]       ("done")

    import de.sciss.negatum.impl.Util._
    import DefaultRandom._
    val recDur = rangeRand(5.0, 7.0)
    val micBus = rangeRand(0, 3)
    ens.stop()

    p.adjustDouble("rec-dur", recDur)
    p.adjustInt   ("bus-in" , micBus)

    val artName = s"anemone/rec/capture-negatum${mkDateString()}.aif"
    val artRec  = dir / artName
    println(s"----X $artRec")
    p   .attr.put("file", artRec)
    done.attr.put("file", artRec)

    ens.play()
  }
}