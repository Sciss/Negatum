/*
 *  FeatureAnalysisView.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum.gui

import de.sciss.lucre.{Txn, synth}
import de.sciss.mellite.UniverseView
import de.sciss.negatum.Negatum
import de.sciss.negatum.gui.impl.FeatureAnalysisViewImpl
import de.sciss.synth.proc.Universe

object FeatureAnalysisView {
  def apply[T <: synth.Txn[T]](negatum: Negatum[T])(implicit tx: T, universe: Universe[T]): FeatureAnalysisView[T] =
    FeatureAnalysisViewImpl[T](negatum)
}
trait FeatureAnalysisView[T <: Txn[T]] extends UniverseView[T]