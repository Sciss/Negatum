/*
 *  FeatureAnalysisFrame.scala
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
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.negatum.gui.impl.{FeatureAnalysisFrameImpl => Impl}
import de.sciss.synth.proc.Universe

object FeatureAnalysisFrame {
  def apply[S <: SSys[S]](negatum: Negatum[S])(implicit tx: S#Tx,
                                               universe: Universe[S]): FeatureAnalysisFrame[S] =
    Impl[S](negatum)
}
trait FeatureAnalysisFrame[S <: Sys[S]] extends Window[S] {
  override def view: FeatureAnalysisView[S]
}