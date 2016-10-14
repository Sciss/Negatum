/*
 *  NegatumIn.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package ugen

final case class NegatumIn() extends Lazy.Expander[Unit] {
  protected def makeUGens: Unit = {
    RandSeed.ir()
  }
}