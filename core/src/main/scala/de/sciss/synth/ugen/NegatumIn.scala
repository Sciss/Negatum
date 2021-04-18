/*
 *  NegatumIn.scala
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

package de.sciss.synth
package ugen

import de.sciss.synth.UGenSource.{ProductType, RefMapIn}

object NegatumIn extends ProductType[NegatumIn] {
  def expand(): Unit = RandSeed.ir()

  override final val typeId = 708

  override def read(in: RefMapIn, prefix: String, arity: Int): NegatumIn = {
    require (arity == 0)
    new NegatumIn()
  }
}
final case class NegatumIn() extends Lazy.Expander[Unit] {
  protected def makeUGens: Unit =
    RandSeed.ir().expand  // `.expand` to make sure the UGen is first!
}