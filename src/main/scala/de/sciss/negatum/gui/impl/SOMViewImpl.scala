/*
 *  SOMViewImpl.scala
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
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.Workspace

object SOMViewImpl {
  def apply[S <: Sys[S]](map: SOM[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                      workspace: Workspace[S]): SOMView[S] = ???
}