/*
 *  FeatureAnalysisFrame.scala
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

import de.sciss.lucre.swing.Window
import de.sciss.lucre.{Txn, synth}
import de.sciss.negatum.gui.impl.FeatureAnalysisFrameImpl
import de.sciss.proc.Universe

object FeatureAnalysisFrame {
  def apply[T <: synth.Txn[T]](negatum: Negatum[T])(implicit tx: T,
                                                    universe: Universe[T]): FeatureAnalysisFrame[T] =
    FeatureAnalysisFrameImpl[T](negatum)
}
trait FeatureAnalysisFrame[T <: Txn[T]] extends Window[T] {
  override def view: FeatureAnalysisView[T]
}