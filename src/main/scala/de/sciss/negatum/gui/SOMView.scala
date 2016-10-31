/*
 *  SOMView.scala
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
import de.sciss.mellite.gui.ViewHasWorkspace
import de.sciss.negatum.gui.impl.{SOMViewImpl => Impl}
import de.sciss.synth.proc.Workspace

object SOMView {
  def apply[S <: Sys[S]](map: SOM[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                      workspace: Workspace[S]): SOMView[S] = Impl(map)
}
trait SOMView[S <: Sys[S]] extends ViewHasWorkspace[S] {
  def map(implicit tx: S#Tx): SOM[S]

  def rendering(implicit tx: S#Tx): Option[Rendering[S, Int]]
}