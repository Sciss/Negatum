/*
 *  NegatumView.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum
package gui

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.View
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.negatum.gui.impl.{NegatumViewImpl => Impl}
import de.sciss.synth.proc.Universe
import de.sciss.synth.proc.gui.UniverseView

object NegatumView {
  def apply[S <: SSys[S]](n: Negatum[S])(implicit tx: S#Tx, universe: Universe[S]): NegatumView[S] = Impl(n)
}
trait NegatumView[S <: Sys[S]] extends UniverseView[S] with View.Editable[S] {
  def negatum(implicit tx: S#Tx): Negatum[S]

  def rendering(implicit tx: S#Tx): Option[Rendering[S, Unit]]
}