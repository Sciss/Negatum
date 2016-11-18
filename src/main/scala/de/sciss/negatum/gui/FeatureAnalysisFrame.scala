/*
 *  FeatureAnalysisFrame.scala
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
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.negatum.gui.impl.{FeatureAnalysisFrameImpl => Impl}
import de.sciss.synth.proc.Workspace

object FeatureAnalysisFrame {
  def apply[S <: SSys[S]](negatum: Negatum[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                         workspace: Workspace[S]): FeatureAnalysisFrame[S] = Impl[S](negatum)
}
trait FeatureAnalysisFrame[S <: Sys[S]] extends Window[S] {
  override def view: FeatureAnalysisView[S]
}