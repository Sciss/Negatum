/*
 *  NegatumView.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum.gui

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.View
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.UniverseView
import de.sciss.negatum.Negatum
import de.sciss.negatum.gui.impl.NegatumViewImpl
import de.sciss.synth.proc.Universe

object NegatumView {
  def apply[S <: SSys[S]](n: Negatum[S])(implicit tx: S#Tx, universe: Universe[S]): NegatumView[S] =
    NegatumViewImpl(n)
}
trait NegatumView[S <: Sys[S]] extends UniverseView[S] with View.Editable[S] {
  def negatum(implicit tx: S#Tx): Negatum[S]

//  def rendering(implicit tx: S#Tx): Option[Rendering[S, Unit]]
}