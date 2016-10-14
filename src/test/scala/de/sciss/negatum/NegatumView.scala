/*
 *  NegatumView.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.mellite.gui.ViewHasWorkspace
import de.sciss.synth.proc.Workspace
import impl.{NegatumViewImpl => Impl}

object NegatumView {
  def apply[S <: Sys[S]](n: Negatum[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                        workspace: Workspace[S]): NegatumView[S] = Impl(n)
}
trait NegatumView[S <: Sys[S]] extends ViewHasWorkspace[S] {
  def negatum(implicit tx: S#Tx): Negatum[S]

  def rendering(implicit tx: S#Tx): Option[Negatum.Rendering[S]]
}
