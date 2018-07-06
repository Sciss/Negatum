/*
 *  NegatumView.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2018 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.swing.View
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.gui.ViewHasWorkspace
import de.sciss.negatum.gui.impl.{NegatumViewImpl => Impl}
import de.sciss.synth.proc.Workspace

object NegatumView {
  def apply[S <: SSys[S]](n: Negatum[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                        workspace: Workspace[S]): NegatumView[S] = Impl(n)
}
trait NegatumView[S <: Sys[S]] extends ViewHasWorkspace[S] with View.Editable[S] {
  def negatum(implicit tx: S#Tx): Negatum[S]

  def rendering(implicit tx: S#Tx): Option[Rendering[S, Unit]]
}