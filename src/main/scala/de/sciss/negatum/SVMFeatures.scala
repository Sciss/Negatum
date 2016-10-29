/*
 *  SVMFeatures.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.negatum.impl.{SVMFeaturesImpl => Impl}
import de.sciss.processor.Processor

object SVMFeatures {
  /** Calculates the feature vector for the SVM-based selection.
    * The vectors are stored in the attribute map of the folder
    * children at key `Negatum.attrFeatures`.
    *
    * @param n          the `Negatum` whose population contains `Proc` instances to evaluate.
    * @param numCoeff   the number of coefficients (vector will be twice
    *                   the size, having `numCoeff` elements for spectral
    *                   and `numCoeff` elements for temporal features).
    * @param overwrite  if `false` (default), will not re-evaluate objects
    *                   which already have a feature vector of the right size
    */
  def apply[S <: Sys[S]](n: Negatum[S], numCoeff: Int = 24, overwrite: Boolean = false)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): Processor[Unit] =
    Impl(n, numCoeff = numCoeff, overwrite = overwrite)
}