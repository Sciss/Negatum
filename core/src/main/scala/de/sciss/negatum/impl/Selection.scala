/*
 *  Selection.scala
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

package de.sciss.negatum
package impl

import de.sciss.negatum.Negatum.Config

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.{IndexedSeq => CVec}
import scala.util.Random

object Selection {
  /* Runs the selection stage of the algorithm, using `all` inputs which
   * are chromosomes paired with their fitness values.
   */
  def apply(config: Config, all: CVec[Individual])(implicit random: Random): Vec[Individual] = {
    import config.breed.selectFraction
    val pop   = all.size
    val n     = (pop * selectFraction + 0.5).toInt
    val outB  = Vector.newBuilder[Individual]
    outB.sizeHint(n)

    @tailrec def loop(rem: Int, in: CVec[Individual]): Unit =
      if (rem == 0 || in.isEmpty) () else {
        val sum     = in.iterator.map(_.fitness).sum
        val rem1    = rem - 1
        if (sum == 0.0) {
          val chosen = in.head
          outB += chosen
          loop(rem1, in.tail)
        } else {
          val inIdx       = in.zipWithIndex // [Individual, Array[(Individual, Int)]](breakOut)
          val norm        = inIdx.map {
            case (individual, j) => (j, individual.fitness / sum)
          }
          val sorted      = norm.sortBy(_._2)
          val acc         = sorted.scanLeft(0.0) { case (a, (_, f)) => a + f } .tail
          val roulette    = random.nextDouble()
          val idxS        = acc.indexWhere(_ > roulette)
          val idx         = if (idxS >= 0) sorted(idxS)._1 else in.size - 1
          val (chosen, _) = inIdx(idx)
          val in1         = in.patch(idx, Nil, 1)
          outB += chosen
          loop(rem1, in1 /* in - chosen */ /*,  out + chosen */)
        }
      }

    loop(n, all.filterNot { i => i.fitness.isInfinity|| i.fitness.isNaN })
    val sel = outB.result().distinct
    // val remove  = all -- sel
    // remove.foreach(prev.remove)
    sel
  }

  /* Selects the best matching chromosomes. */
  def elitism(config: Config, all: CVec[Individual]): Vec[Individual] = {
    import config.breed.{elitism => numElitism}
    if (numElitism == 0) Vector.empty else {
      // ensure that elite choices are distinct (don't want to accumulate five identical chromosomes over time)!
      val eliteCandidates = all.sortBy(-_.fitness)
      val res = Vector.newBuilder[Individual]
      res.sizeHint(numElitism)
      val it = eliteCandidates.iterator
      var sz = 0
      var fl = Double.NaN
      while (sz < numElitism && it.hasNext) {
        val individual = it.next()
        val f = individual.fitness
        if (f != fl) {
          res += individual
          sz  += 1
          fl   = f
        }
      }
      res.result()
    }
  }
}
