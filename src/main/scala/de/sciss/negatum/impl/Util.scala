/*
 *  Util.scala
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

package de.sciss.negatum
package impl

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{Seq => ISeq}
import scala.language.higherKinds
import scala.util.Random

object Util {
  def scramble[A, CC[~] <: IndexedSeq[~], To](in: CC[A])(implicit random: Random, cbf: CanBuildFrom[CC[A], A, To]): To = {
    val b = cbf(in)
    var rem = in: IndexedSeq[A]
    while (rem.nonEmpty) {
      val idx = random.nextInt(rem.size)
      val e = rem(idx)
      rem = rem.patch(idx, Nil, 1)
      b += e
    }
    b.result()
  }

  def rrand(lo: Int, hi: Int)(implicit random: Random): Int = lo + random.nextInt(hi - lo + 1)

  def exprand(lo: Double, hi: Double)(implicit random: Random): Double =
    lo * math.exp(math.log(hi / lo) * random.nextDouble())

  def coin(p: Double = 0.5)(implicit random: Random): Boolean = random.nextDouble() < p

  def choose[A](xs: ISeq[A])(implicit random: Random): A = xs.toIndexedSeq(random.nextInt(xs.size))
}
