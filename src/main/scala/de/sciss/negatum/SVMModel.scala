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

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj, Sys}
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

  // ---- rendering ----

  object Rendering {
    sealed trait State {
      def isComplete: Boolean
    }
    case class Success(selected: Int) extends State {
      def isComplete = true
    }
    /** Rendering either failed or was aborted.
      * In the case of abortion, the throwable is
      * of type `Cancelled`.
      */
    final case class Failure(ex: Throwable) extends State {
      def isComplete = true
    }
    final case class Progress(amount: Double) extends State {
      def isComplete = false
    }

    val  Cancelled = Processor.Aborted
    type Cancelled = Processor.Aborted
  }
  trait Rendering[S <: Sys[S]] extends Observable[S#Tx, Rendering.State] with Disposable[S#Tx] {
    def state(implicit tx: S#Tx): Rendering.State

    /** Like `react` but invokes the function immediately with the current state. */
    def reactNow(fun: S#Tx => Rendering.State => Unit)(implicit tx: S#Tx): Disposable[S#Tx]

    /** Cancels the process and does not keep results. */
    def cancel()(implicit tx: S#Tx): Unit

//    /** Stops process at the next possible moment, and return current results. */
//    def stop  ()(implicit tx: S#Tx): Unit
  }

  def attrSelected: String = Negatum.attrSelected
}
trait SVMModel[S <: Sys[S]] extends Obj[S] {
  def config: SVMConfig

  def stats: Stats

  def predictOne(vec: Vec[Double]): Double

  /** Predicts the selection of all individuals in a folder. Results are stored with the
    * `Negatum.attrSelected` key in the individuals' attribute maps.
    * The processor returns the number of selected individuals.
    */
  def predict(n: Negatum[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): SVMModel.Rendering[S]
}