/*
 *  SVMModel.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.negatum.SVMModel.Stats
import de.sciss.negatum.impl.{SVMModelImpl => Impl}
import de.sciss.processor.Processor
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec, Seq => ISeq}

object SVMModel extends Obj.Type {
  final val typeID = 0x40001

  type Trained[S <: Sys[S]] = stm.Source[S#Tx, SVMModel[S]]

  def train[S <: Sys[S]](n: ISeq[Negatum[S]], config: SVMConfig, numCoeff: Int = 24)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): Processor[Trained[S]] =
    Impl.train(n, config, numCoeff = numCoeff)

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, SVMModel[S]] = Impl.serializer[S]

  object FeatureStat {
    implicit object serializer extends ImmutableSerializer[FeatureStat] {
      def write(fs: FeatureStat, out: DataOutput): Unit = {
        import fs._
        out.writeFloat(min   )
        out.writeFloat(max   )
        out.writeFloat(mean  )
        out.writeFloat(stdDev)
      }

      def read(in: DataInput): FeatureStat = {
        val min    = in.readFloat()
        val max    = in.readFloat()
        val mean   = in.readFloat()
        val stdDev = in.readFloat()
        FeatureStat(min = min, max = max, mean = mean, stdDev = stdDev)
      }
    }
  }
  final case class FeatureStat(min: Float, max: Float, mean: Float, stdDev: Float) {
    override def toString: String = s"$productPrefix(min = $min, max = $max, mean = $mean, stdDev = $stdDev)"
  }

  object Stats {
    private[this] final val COOKIE = 0x73746174

    implicit object serializer extends ImmutableSerializer[Stats] {
      def write(stats: Stats, out: DataOutput): Unit = {
        import stats._
        out.writeInt(COOKIE)
        out.writeInt(count)
        out.writeInt(selected)
        out.writeInt(features.size)
        val fs = FeatureStat.serializer
        features.foreach(fs.write(_, out))
      }

      def read(in: DataInput): Stats = {
        val cookie = in.readInt()
        if (cookie != COOKIE) sys.error(s"Unexpected cookie ${cookie.toHexString} (expected ${COOKIE.toHexString})")
        val count     = in.readInt()
        val selected  = in.readInt()
        val numFeat   = in.readInt()
        val fs        = FeatureStat.serializer
        val features  = Vector.fill(numFeat)(fs.read(in))
        Stats(count = count, selected = selected, features = features)
      }
    }
  }
  final case class Stats(count: Int, selected: Int, features: Vec[FeatureStat]) {
    override def toString: String = {
      val pre = s"$productPrefix(count = $count, selected = $selected, features = Vector(\n    "
      features.mkString(pre, ",\n    ", ")\n  )")
    }
  }
}
trait SVMModel[S <: Sys[S]] extends Obj[S] {
  def config: SVMConfig

  def stats: Stats

  def predict(vec: Vec[Double], normalize: Boolean = false): Double
}