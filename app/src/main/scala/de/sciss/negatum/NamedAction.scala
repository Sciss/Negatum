/*
 *  NamedAction.scala
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

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.negatum.Composition.NoSys
import de.sciss.synth.proc.Action
import de.sciss.synth.proc.Action.Universe

abstract class NamedAction(val name: String) extends Action.Body {
  final def apply[S <: Sys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
    type T = NoSys
    // yes, that's ugly
    begin[T](universe.asInstanceOf[Universe[T]])(tx.asInstanceOf[T#Tx])
  }

  def begin[S <: SSys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit
}