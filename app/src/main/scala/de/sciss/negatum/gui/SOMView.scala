/*
 *  SOMView.scala
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
package gui

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.UniverseView
import de.sciss.negatum.gui.impl.{SOMViewImpl => Impl}
import de.sciss.proc.Universe

object SOMView {
  def apply[S <: SSys[S]](map: SOM[S])(implicit tx: S#Tx, universe: Universe[S]): SOMView[S] = Impl(map)
}
trait SOMView[S <: Sys[S]] extends UniverseView[S] {
  def map(implicit tx: S#Tx): SOM[S]

  def rendering(implicit tx: S#Tx): Option[Rendering[S, Int]]
}