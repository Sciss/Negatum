/*
 *  SVMModelView.scala
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
package gui

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.gui.ViewHasWorkspace
import de.sciss.synth.proc.Workspace

object SVMModelView {
  def apply[S <: Sys[S]](m: SVMModel[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                         workspace: Workspace[S]): SVMModelView[S] = ??? // Impl(m)
}
trait SVMModelView[S <: Sys[S]] extends ViewHasWorkspace[S] /* with View.Editable[S] */ {
  def model(implicit tx: S#Tx): SVMModel[S]

  // def rendering(implicit tx: S#Tx): Option[Negatum.Rendering[S]]
}