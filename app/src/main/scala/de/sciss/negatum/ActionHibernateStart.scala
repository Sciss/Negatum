/*
 *  ActionHibernateStart.scala
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
import de.sciss.lucre.expr.BooleanObj
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.negatum.Hibernation.{logComp, mkDateString}
import de.sciss.proc.Action.Universe
import de.sciss.proc.Implicits._
import de.sciss.proc.{ActionRaw, Ensemble, Proc}

object ActionHibernateStart extends NamedAction("hibernate-start") {
  def begin[S <: SSys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
    val dsl = DSL[S]
    import dsl._
    import universe._
    val attr          = self.attr
    val Some(done)    = attr.$[ActionRaw]       ("done")
    val Some(ensList) = attr.$[Ensemble]        ("context")
    val Some(p)       = attr.$[Proc]            ("proc")
    val Some(dir)     = attr.$[ArtifactLocation]("dir")
    val Some(side)    = attr.$[BooleanObj]      ("side")
//    val Some(ensPlay) = attr.$[Ensemble]        ("ens-play")

    val sideVal = side.value
    val recDur  = 90.0 // 60.0 // rrand(5.0, 7.0)
    val micBus  = if (sideVal) 1 else 0
    ensList.stop()
//    ensPlay.stop()

    p.adjustDouble("rec-dur", recDur)
    p.adjustInt   ("bus-in" , micBus)

    val artName = s"anemone/rec/capture-hibernate${mkDateString()}.aif"
    val artRec  = dir / artName
    logComp(s"recording bus $micBus to $artRec")
    p   .attr.put("file", artRec)
    done.attr.put("file", artRec)

    ensList.play()
  }
}