/*
 *  FeatureAnalysisFrameImpl.scala
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

package de.sciss.negatum
package gui
package impl

import de.sciss.lucre.{Txn, synth}
import de.sciss.mellite.impl.WindowImpl
import de.sciss.proc.Universe

object FeatureAnalysisFrameImpl {
  def apply[T <: synth.Txn[T]](negatum: Negatum[T])(implicit tx: T, universe: Universe[T]): FeatureAnalysisFrame[T] = {
    val view = FeatureAnalysisView[T](negatum)
    val res = new Impl[T](view)
    res.init()
    res
  }

  private final class Impl[T <: Txn[T]](val view: FeatureAnalysisView[T])
    extends WindowImpl[T] with FeatureAnalysisFrame[T] {
  }
}
