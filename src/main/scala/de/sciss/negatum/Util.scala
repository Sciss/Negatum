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

package de.sciss.negatum

import de.sciss.kollflitz.Vec
import de.sciss.negatum.Delaunay.Vector2

import scala.annotation.switch
import scala.util.Random

object Util {
  object DefaultRandom {
    implicit val random: Random = new Random
  }

  def exprand(lo: Double, hi: Double)(implicit random: Random): Double =
    lo * math.exp(math.log(hi / lo) * random.nextDouble)

  def rrand(lo: Double, hi: Double)(implicit random: Random): Double =
    random.nextDouble() * (hi - lo) + lo

  /** `lo` to `hi` (inclusive). */
  def rrand(lo: Int, hi: Int)(implicit random: Random): Int = {
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

  def randomRectSides(n: Int)(implicit random: Random): Vec[Vector2] = {
    val sides = random.shuffle(0 until 4: Vec[Int]).take(n)
    sides.map { si =>
      val r = random.nextFloat()
      (si: @switch) match {
        case 0 => Vector2(0f, r) // left
        case 1 => Vector2(r, 0f)  // top
        case 2 => Vector2(r, 1f)  // bottom
        case 3 => Vector2(1f, r)  // right
      }
    }
  }
}