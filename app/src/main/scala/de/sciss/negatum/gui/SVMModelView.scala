/*
 *  SVMModelView.scala
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
import de.sciss.negatum.gui.impl.{SVMModelViewImpl => Impl}
import de.sciss.synth.proc.Universe
import de.sciss.synth.proc.gui.UniverseView

object SVMModelView {
  def apply[S <: Sys[S]](m: SVMModel[S])(implicit tx: S#Tx, universe: Universe[S]): SVMModelView[S] = Impl(m)
}
trait SVMModelView[S <: Sys[S]] extends UniverseView[S] /* with View.Editable[S] */ {
  def model(implicit tx: S#Tx): SVMModel[S]

  def rendering(implicit tx: S#Tx): Option[Rendering[S, Int]]
}