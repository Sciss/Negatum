/*
 *  Util.scala
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
package impl

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.Random

object Util {
  def scramble[A, CC[~] <: Seq[~], To](in: CC[A])(implicit random: Random, cbf: CanBuildFrom[CC[A], A, To]): To = {
    val b = cbf(in)
    var rem = in: Seq[A]
    while (rem.nonEmpty) {
      val idx = random.nextInt(rem.size)
      val e = rem(idx)
      rem = rem.patch(idx, Nil, 1)
      b += e
    }
    b.result()
  }

  def roulette[A](in: Seq[(A, Int)])(implicit random: Random): A = {
    val sum         = in.map(_._2).sum
    val norm        = in.zipWithIndex.map { case ((c, f), j) => (j, f / sum) }
    val sorted      = norm.sortBy(_._2)
    val accum       = sorted.scanLeft(0.0) { case (a, (_, f)) => a + f } .tail
    val roul        = random.nextDouble() // * max
    val idxS        = accum.indexWhere(_ > roul)
    val idx         = if (idxS >= 0) sorted(idxS)._1 else in.size - 1
    val (chosen, _) = in(idx)
    chosen
  }

  def rrand(lo: Int, hi: Int)(implicit random: Random): Int = lo + random.nextInt(hi - lo + 1)

  def exprand(lo: Double, hi: Double)(implicit random: Random): Double =
    lo * math.exp(math.log(hi / lo) * random.nextDouble())

  def coin(p: Double = 0.5)(implicit random: Random): Boolean = random.nextDouble() < p

  def choose[A](xs: Seq[A])(implicit random: Random): A = xs(random.nextInt(xs.size))
}
