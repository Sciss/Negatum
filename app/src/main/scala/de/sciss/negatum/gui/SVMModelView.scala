/*
 *  SVMModelView.scala
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
import de.sciss.mellite.UniverseView
import de.sciss.negatum.gui.impl.{SVMModelViewImpl => Impl}
import de.sciss.proc.Universe

object SVMModelView {
  def apply[S <: Sys[S]](m: SVMModel[S])(implicit tx: S#Tx, universe: Universe[S]): SVMModelView[S] = Impl(m)
}
trait SVMModelView[S <: Sys[S]] extends UniverseView[S] /* with View.Editable[S] */ {
  def model(implicit tx: S#Tx): SVMModel[S]

  def rendering(implicit tx: S#Tx): Option[Rendering[S, Int]]
}