package de.sciss.negatum
package gui

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.gui.ViewHasWorkspace
import de.sciss.negatum.gui.impl.{FeatureAnalysisViewImpl => Impl}
import de.sciss.synth.proc.Workspace

object FeatureAnalysisView {
  def apply[S <: SSys[S]](negatum: Negatum[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                         workspace: Workspace[S]): FeatureAnalysisView[S] =
    Impl[S](negatum)
}
trait FeatureAnalysisView[S <: Sys[S]] extends ViewHasWorkspace[S]