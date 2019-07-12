/*
 *  Util.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum.impl

import de.sciss.negatum.impl.Chromosome.SynthGraphT
import de.sciss.synth.ugen.{BinaryOpUGen, UnaryOpUGen}

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.higherKinds
import scala.util.Random

object Util {
  object DefaultRandom {
    implicit val random: Random = new Random
  }

  def expRand(lo: Double, hi: Double)(implicit random: Random): Double =
    lo * math.exp(math.log(hi / lo) * random.nextDouble)

  def rangeRand(lo: Double, hi: Double)(implicit random: Random): Double =
    random.nextDouble() * (hi - lo) + lo

  /** `lo` to `hi` (inclusive). */
  def rangeRand(lo: Int, hi: Int)(implicit random: Random): Int = {
    if (lo <= hi) {
      random.nextInt(hi - lo + 1) + lo
    } else {
      random.nextInt(lo - hi + 1) + hi
    }
  }

  /** `0 to (i-1)` or `(0 until i)` (exclusive) */
  def rand(i: Int)(implicit random: Random): Int = random.nextInt(i)

  /** `0` until `d` (exclusive). */
  def rand(d: Double)(implicit random: Random): Double  = random.nextDouble() * d

  def coin(w: Double = 0.5)(implicit random: Random): Boolean = random.nextDouble() < w

  def choose[A](seq: Seq[A])(implicit random: Random): A =
    seq(random.nextInt(seq.size))

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
    val norm        = in.zipWithIndex.map { case ((_ /* c */, f), j) => (j, f / sum) }
    val sorted      = norm.sortBy(_._2)
    val accum       = sorted.scanLeft(0.0) { case (a, (_, f)) => a + f } .tail
    val roul        = random.nextDouble() // * max
    val idxS        = accum.indexWhere(_ > roul)
    val idx         = if (idxS >= 0) sorted(idxS)._1 else in.size - 1
    val (chosen, _) = in(idx)
    chosen
  }

  def graphElemName(in: Product): String =
    in match {
      case bin: BinaryOpUGen =>
        s"Bin_${bin.selector.id}"
      case un: UnaryOpUGen =>
        s"Un_${un.selector.id}"
      case _ => in.productPrefix
    }

  def getGraphRoots(top: SynthGraphT): Vec[Vertex.UGen] = {
    val ugens = top.vertices.reverseIterator.collect {
      case ugen: Vertex.UGen => ugen
    }
    val edges = top.edges
    val f = ugens.filter { ugen =>
      edges.forall(_.targetVertex != ugen)
    }
    f.toIndexedSeq
  }
}