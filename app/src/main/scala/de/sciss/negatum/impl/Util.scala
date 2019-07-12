///*
// *  Util.scala
// *  (Negatum)
// *
// *  Copyright (c) 2016-2019 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU General Public License v3+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.negatum
//package impl
//
//import scala.collection.generic.CanBuildFrom
//import scala.language.higherKinds
//import scala.util.Random
//
//object Util {
//
//  def rrand(lo: Int, hi: Int)(implicit random: Random): Int = lo + random.nextInt(hi - lo + 1)
//
//  def exprand(lo: Double, hi: Double)(implicit random: Random): Double =
//    lo * math.exp(math.log(hi / lo) * random.nextDouble())
//
//  def coin(p: Double = 0.5)(implicit random: Random): Boolean = random.nextDouble() < p
//
//  def choose[A](xs: Seq[A])(implicit random: Random): A = xs(random.nextInt(xs.size))
//}
