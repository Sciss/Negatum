/*
 *  FeatureAnalysisFrameImpl.scala
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
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.synth.proc.Workspace

object FeatureAnalysisFrameImpl {
  def apply[S <: Sys[S]](negatum: Negatum[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                         workspace: Workspace[S]): FeatureAnalysisFrame[S] = {
    val view = FeatureAnalysisView[S](negatum)
    val res = new Impl[S](view)
    res.init()
    res
  }

  private final class Impl[S <: Sys[S]](val view: FeatureAnalysisView[S])
    extends WindowImpl[S] with FeatureAnalysisFrame[S] {
  }
}
