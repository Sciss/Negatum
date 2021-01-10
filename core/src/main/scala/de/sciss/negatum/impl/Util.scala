/*
 *  Util.scala
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

package de.sciss.negatum.impl

import de.sciss.negatum.Negatum.SynthGraphT
import de.sciss.negatum.Vertex
import de.sciss.synth.ugen.{BinaryOpUGen, UnaryOpUGen}

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.util.Random

object Util {
  def dct(in: Array[Double], off: Int, len: Int, numCoeff: Int): Array[Double] = {
    val c = new Array[Double](numCoeff)
    var n = 0
    val r = math.Pi / len
    while (n < numCoeff) {
      var i = 0
      val s = r * n
      while (i < len) {
        c(n) += in(i + off) * math.cos(s * (i + 0.5))
        i += 1
      }
      n += 1
    }
    c
  }

  /** Mutates `a` by multiplying its contents with `b`. */
  def mul(a: Array[Double], aOff: Int, b: Array[Double], bOff: Int, len: Int): Unit = {
    var ai = aOff
    val stop = ai + len
    var bi = bOff
    while (ai < stop) {
      a(ai) *= b(bi)
      ai += 1
      bi += 1
    }
  }

  /** Mutates `a` by adding `b` to it. */
  def add(a: Array[Double], aOff: Int, b: Array[Double], bOff: Int, len: Int): Unit = {
    var ai = aOff
    val stop = ai + len
    var bi = bOff
    while (ai < stop) {
      a(ai) += b(bi)
      ai += 1
      bi += 1
    }
  }

  /** Mutates `a` by multiplying each element with `f` */
  def mul(a: Array[Double], off: Int, len: Int, f: Double): Unit = {
    var ai = off
    val stop = ai + len
    while (ai < stop) {
      a(ai) *= f
      ai += 1
    }
  }

  /** Calculates RMS */
  def energy(in: Array[Double], off: Int, len: Int): Double = {
    var sum = 0.0
    var i = off
    val j = i + len
    while (i < j) {
      sum += in(i) * in(i)
      i += 1
    }
    math.sqrt(sum / len)
  }

  object DefaultRandom {
    implicit val random: Random = new Random
  }

  def expRand(lo: Double, hi: Double)(implicit random: Random): Double =
    lo * math.exp(math.log(hi / lo) * random.nextDouble())

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
    val accumulated = sorted.scanLeft(0.0) { case (a, (_, f)) => a + f } .tail
    val r           = random.nextDouble() // * max
    val idxS        = accumulated.indexWhere(_ > r)
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