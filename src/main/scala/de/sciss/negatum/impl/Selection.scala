/*
 *  Selection.scala
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

import de.sciss.negatum.Negatum.Config

import scala.annotation.tailrec
import scala.collection._
import scala.util.Random

object Selection {
  /* Runs the selection stage of the algorithm, using `all` inputs which
   * are chromosomes paired with their fitness values.
   */
  def apply(config: Config, all: IndexedSeq[Individual])(implicit random: Random): Set[Individual] = {
    import config.breeding.selectionFrac
    val pop   = all.size
    val n     = (pop * selectionFrac + 0.5).toInt

    @tailrec def loop(rem: Int, in: Set[Individual], out: Set[Individual]): Set[Individual] =
      if (rem == 0) out else {
        val sum     = in.iterator.map(_.fitness).sum
        val rem1    = rem - 1
        if (sum == 0.0) {
          val chosen = in.head
          loop(rem1, in - chosen, out + chosen)
        } else {
          val inIdx       = in.zipWithIndex[Individual, Array[(Individual, Int)]](breakOut)
          val norm        = inIdx.map {
            case (indiv, j) => (j, indiv.fitness / sum)
          }
          val sorted      = norm.sortBy(_._2)
          val acc         = sorted.scanLeft(0.0) { case (a, (_, f)) => a + f } .tail
          val roulette    = random.nextDouble()
          val idxS        = acc.indexWhere(_ > roulette)
          val idx         = if (idxS >= 0) sorted(idxS)._1 else in.size - 1
          val (chosen, _) = inIdx(idx)
          loop(rem1, in - chosen, out + chosen)
        }
      }

    val sel = loop(n, all.toSet.filterNot { indiv => indiv.fitness.isInfinity|| indiv.fitness.isNaN }, Set.empty)
    // val remove  = all -- sel
    // remove.foreach(prev.remove)
    sel
  }

  /* Selects the best matching chromosomes. */
  def elitism(config: Config, all: IndexedSeq[Individual]): Vec[Individual] = {
    import config.breeding.numElitism
    if (numElitism == 0) Vector.empty else {
      // ensure that elite choices are distinct (don't want to accumulate five identical chromosomes over time)!
      val eliteCandidates = all.sortBy(-_.fitness)
      val res = Vector.newBuilder[Individual]
      res.sizeHint(numElitism)
      val it = eliteCandidates.iterator
      var sz = 0
      var fl = Double.NaN
      while (sz < numElitism && it.hasNext) {
        val indiv = it.next()
        val f     = indiv.fitness
        if (f != fl) {
          res += indiv
          sz  += 1
          fl   = f
        }
      }
      res.result()
    }
  }
}
