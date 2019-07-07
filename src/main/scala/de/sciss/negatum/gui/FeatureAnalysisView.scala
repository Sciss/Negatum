/*
 *  FeatureAnalysisView.scala
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
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.negatum.gui.impl.{FeatureAnalysisViewImpl => Impl}
import de.sciss.synth.proc.Universe
import de.sciss.synth.proc.gui.UniverseView

object FeatureAnalysisView {
  def apply[S <: SSys[S]](negatum: Negatum[S])(implicit tx: S#Tx, universe: Universe[S]): FeatureAnalysisView[S] =
    Impl[S](negatum)
}
trait FeatureAnalysisView[S <: Sys[S]] extends UniverseView[S]