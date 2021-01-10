/*
 *  NegatumView.scala
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

package de.sciss.negatum.gui

import de.sciss.lucre.swing.View
import de.sciss.lucre.{Txn, synth}
import de.sciss.mellite.UniverseView
import de.sciss.negatum.Negatum
import de.sciss.negatum.gui.impl.NegatumViewImpl
import de.sciss.proc.Universe

object NegatumView {
  def apply[T <: synth.Txn[T]](n: Negatum[T])(implicit tx: T, universe: Universe[T]): NegatumView[T] =
    NegatumViewImpl(n)
}
trait NegatumView[T <: Txn[T]] extends UniverseView[T] with View.Editable[T] {
  def negatum(implicit tx: T): Negatum[T]

//  def rendering(implicit tx: S#Tx): Option[Rendering[S, Unit]]
}