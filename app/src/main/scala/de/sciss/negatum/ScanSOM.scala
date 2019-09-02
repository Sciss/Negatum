/*
 *  ScanSOM.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum

import java.util

import de.sciss.lucre.expr.SpanLikeObj
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.span.Span
import de.sciss.synth.proc.{TimeRef, Timeline}
import de.sciss.{kollflitz, numbers}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

object ScanSOM {
//  private[this] lazy val init: Unit = ActionRaw.registerPredef(id, body)

  sealed trait ConfigLike {
    /** Mean duration in seconds. */
    def durMean: Double

    /** Standard deviation of duration in seconds. */
    def durStdDev: Double

    /** Mean overlap in seconds. */
    def overlapMean: Double

    /** Standard deviation of overlap in seconds. */
    def overlapStdDev: Double

    def seed: Long

//    def selectObj(in: Obj[S], time: Long)(implicit tx: S#Tx): Option[Obj[S]]
  }

  object Config {
    def apply(): ConfigBuilder = new ConfigBuilder

    implicit def build(b: ConfigBuilder): Config = {
      import b._
      Impl(durMean = durMean, durStdDev = durStdDev, overlapMean = overlapMean, overlapStdDev = overlapStdDev,
        seed = seed)
    }

    private final case class Impl(durMean    : Double, durStdDev    : Double,
                                  overlapMean: Double, overlapStdDev: Double,
                                  seed       : Long)
      extends Config
  }
  trait Config extends ConfigLike
  final class ConfigBuilder extends ConfigLike {
    var durMean       : Double  = 6.0
    var durStdDev     : Double  = 2.0

    var overlapMean   : Double  = 4.0
    var overlapStdDev : Double  = 2.0

    var seed          : Long    = System.currentTimeMillis()
  }

//  private def pointOption[S <: Sys[S]](obj: Obj[S], dim: Int)(implicit tx: S#Tx): Option[Seq[Double]] = obj match {
//    case d: DoubleVector[S] =>
//      val v   = d.value
//      val res = Seq.tabulate(dim)(i => v(i % v.size))
//      Some(res)
//    case i: IntObj[S]       =>
//      val v = i.value.toDouble
//      val res = Seq.fill(dim)(v)
//      Some(res)
//    case d: DoubleObj[S] =>
//      val v = d.value
//      val res = Seq.fill(dim)(v)
//      Some(res)
//  }

//  private def linlin(time: Long, pt1: Seq[Double], time1: Long, pt2: Seq[Double], time2: Long): Seq[Int] = {
//      ...
//  }

//  // if not found, returns an empty list
//  private def linearInterpolation[S <: Sys[S]](g: Grapheme[S], time: Long, dim: Int)
//                                              (implicit tx: S#Tx): Option[Seq[Int]] = {
//    for {
//      BiPin.Entry(time1Obj, obj1) <- g.floor(time)
//      BiPin.Entry(time2Obj, obj2) <- g.floor(time)
//      pt1                         <- pointOption(obj1, dim = dim)
//      pt2                         <- pointOption(obj2, dim = dim)
//    } yield {
//      linlin(time = time, pt1 = pt1, time1 = time1Obj.value, pt2 = pt2, time2 = time2Obj.value)
//    }
//  }

  final case class Input[S <: Sys[S]](obj: Obj[S], span: Span, idx: Int)

  def apply[S <: Sys[S]](som: SOM[S], timeline: Timeline.Modifiable[S], span: Span,
                         trajectory: Vec[Seq[Double]], config: Config)
                        (prepare: Input[S] => Option[Obj[S]])
                        (implicit tx: S#Tx): Unit = {

    val dim   = som.config.dimensions
    val rnd   = new util.Random(config.seed)

    import kollflitz.Ops._
    val trjLens = trajectory.mapPairs { (pt1, pt2) =>
      require(pt1.size == dim && pt2.size == dim)
      val ds = (pt1, pt2).zipped.map { (c1, c2) => val d: Double = c1 - c2; d * d }
      math.sqrt(ds.sum)
    }
    val lensInt           = (0.0 +: trjLens).integrate // [Array[Double]](Numeric.DoubleIsFractional, breakOut)
    val totalLen: Double  = lensInt.last
    val durMean           = math.max(0.1, config.durMean)
    val side              = som.config.extent * 2

    var time  = span.start
    var count = 0
    val lensIntA = lensInt.toArray
    while (time < span.stop) {
      import numbers.Implicits._
      val dist   = time.linLin(span.start, span.stop, 0.0, totalLen)
      // if not found, returned value is `(-insertion_point - 1)`
      // insertion_point = -(result + 1)
      val trjIdx  = util.Arrays.binarySearch(lensIntA, dist)
      val pt: Seq[Int] = if (trjIdx >= 0) {
        trajectory(trjIdx).map { d =>
          (d * side).toInt
        }
      } else {
        val idxC  = -(trjIdx + 1)
        val idxF  = idxC - 1
        val ptF   = trajectory(idxF)
        val ptC   = trajectory(idxC)
        val wC    = dist.linLin(lensInt(idxF), lensInt(idxC), 0, 1)
        val wF    = 1 - wC
        Seq.tabulate(dim) { i =>
          val d = ptF(i) * wF + ptC(i) * wC
          (d * side).toInt
        }
      }

      val durSec  = (rnd.nextGaussian() * config.durStdDev     + durMean           ).clip(0.1, durMean * 4)
      val lapSec  = (rnd.nextGaussian() * config.overlapStdDev + config.overlapMean).min(durSec * 0.95)
      val durFr   = (durSec * TimeRef.SampleRate).toLong
      val lapFr   = (lapSec * TimeRef.SampleRate).toLong
      val insSpan = Span(time, time + durFr)

      for {
        (_, objIn)  <- som.query(pt)
        objOut      <- prepare(Input(objIn, insSpan, count))
      } {
        val objSpan = SpanLikeObj.newVar[S](insSpan) // XXX TODO --- or we omit the var?
        timeline.add(objSpan, objOut)
        count += 1
      }

      val nextTime = time + durFr - lapFr
      time = if (nextTime < time) Long.MaxValue else nextTime
    }
  }
}