/*
 *  FeatureAnalysisFrameImpl.scala
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
package impl

import de.sciss.lucre.synth.Sys
import de.sciss.mellite.impl.WindowImpl
import de.sciss.synth.proc.Universe

object FeatureAnalysisFrameImpl {
  def apply[S <: Sys[S]](negatum: Negatum[S])(implicit tx: S#Tx, universe: Universe[S]): FeatureAnalysisFrame[S] = {
    val view = FeatureAnalysisView[S](negatum)
    val res = new Impl[S](view)
    res.init()
    res
  }

  private final class Impl[S <: Sys[S]](val view: FeatureAnalysisView[S])
    extends WindowImpl[S] with FeatureAnalysisFrame[S] {
  }
}
