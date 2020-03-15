/*
 *  SVMModelImpl.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum
package impl

import java.util.concurrent.TimeUnit

import de.sciss.kollflitz.Vec
import de.sciss.lucre.event.impl.ConstObjImpl
import de.sciss.lucre.expr.{BooleanObj, DoubleVector}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.negatum.SVMConfig.{Type, Weight => SVMWeight}
import de.sciss.negatum.SVMModel.{FeatureStat, Stats, Trained}
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.proc.SoundProcesses
import libsvm.{svm, svm_model, svm_node, svm_problem}

import scala.annotation.tailrec
import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, blocking}

object SVMModelImpl {
  def train[S <: Sys[S]](n: ISeq[Negatum[S]], config: SVMConfig, numCoeff: Int)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): Processor[Trained[S]] = {
    val res = new TrainImpl[S](config, numCoeff = numCoeff,
      negatumH = n.iterator.map(tx.newHandle(_)).toList)
    tx.afterCommit {
      import ExecutionContext.Implicits.global
      res.start()
    }
    res
  }

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, SVMModel[S]] = anySer.asInstanceOf[Ser[S]]

  private[this] val anySer = new Ser[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, SVMModel[S]] {
    def tpe: Obj.Type = SVMModel
  }

  private final class LabelledFeatures(val vec: Array[Double], val label: Boolean)

  private final class TrainImpl[S <: Sys[S]](config0: SVMConfig, numCoeff: Int,
                                             negatumH: List[stm.Source[S#Tx, Negatum[S]]])
                                            (implicit cursor: stm.Cursor[S])
    extends ProcessorImpl[Trained[S], Processor[Trained[S]]]
      with Processor[Trained[S]] {

    override def toString = s"SVMModel.train@${hashCode.toHexString}"

    protected def body(): Trained[S] = blocking {
      // ---- ensure features have been extracted ----

      val procFeatW = 0.8 / negatumH.size

      @tailrec
      def loopFeat(rem: List[stm.Source[S#Tx, Negatum[S]]], count: Int): Unit = rem match {
        case head :: tail =>
          val procFeatFut = SoundProcesses.atomic[S, Processor[Unit]] { implicit tx =>
            val n = head()
            SVMFeatures(n, numCoeff = numCoeff, overwrite = false)
          }
          val procFeat = Await.result(procFeatFut, Duration(10, TimeUnit.SECONDS))
          await(procFeat, offset = count * procFeatW, weight = procFeatW)

          loopFeat(tail, count = count + 1)

        case _ =>
      }

      loopFeat(negatumH, count = 0)

      // ---- collect features ----

      val vecSize = numCoeff * 2
      val futLabelled: Future[Array[LabelledFeatures]] =
        SoundProcesses.atomic[S, Array[LabelledFeatures]] { implicit tx =>
          negatumH.iterator.flatMap { nS =>
            val f = nS().population
            f.iterator.flatMap { obj =>
              val attr = obj.attr
              for {
                feat <- attr.$[DoubleVector](Negatum.attrFeatures).map(_.value)
                if feat.size == vecSize
              } yield {
                val isIn = attr.$[BooleanObj](Negatum.attrSelected).exists(_.value)
                new LabelledFeatures(feat.toArray, label = isIn)
              }
            } .toArray[LabelledFeatures]
          } .toArray // (breakOut)
        }

      val labelled  = Await.result(futLabelled, Duration(30, TimeUnit.SECONDS))

      progress = 0.85
      checkAborted()

      // ---- stats ----

      var statSel   = 0
      val statCount = labelled.length
      val statMin   = Array.fill[Float](vecSize)(Float.MaxValue)
      val statMax   = Array.fill[Float](vecSize)(Float.MinValue)
      val statMean  = Array.fill[Float](vecSize)(0f)
      val statStdDev= Array.fill[Float](vecSize)(0f)

      labelled.foreach { l =>
        var idx   = 0
        val arr   = l.vec
        if (l.label) statSel += 1
        while (idx < arr.length) {
          val value   = arr(idx)
          val valueF  = value.toFloat
          if (valueF < statMin(idx)) statMin(idx) = valueF
          if (valueF > statMax(idx)) statMax(idx) = valueF
          statMean(idx) += valueF
          idx        += 1
        }
      }

      var j = 0
      if (statCount > 0) while (j < vecSize) {
        statMean(j) /= statCount
        j += 1
      }

      labelled.foreach { l =>
        var idx   = 0
        val arr   = l.vec
        while (idx < arr.length) {
          val d   = arr(idx) - statMean(idx)
          val ds  = d * d
          statStdDev(idx) += ds.toFloat
          idx += 1
        }
      }

      j = 0
      if (statCount > 1) while (j < vecSize) {
        statStdDev(j) = math.sqrt(statStdDev(j) / (statCount - 1)).toFloat
        j += 1
      }

      val statFeat = Vector.tabulate(vecSize) { idx =>
        FeatureStat(min = statMin(idx), max = statMax(idx), mean = statMean(idx), stdDev = statStdDev(idx))
      }
      val stats = Stats(count = statCount, selected = statSel, features = statFeat)

      // ---- model ----

      val config   = config0.tpe match {
        case tpe @ Type.CSVC(_, Nil) =>
          val w0      = SVMWeight(label = 0, value = statSel.toFloat / statCount)
          val w1      = SVMWeight(label = 1, value = (statCount - statSel).toFloat / statCount)
          val weights = List(w0, w1)
          val tpe1    = tpe.copy(weights = weights)
          val res     = SVMConfig()
          res.read(config0)
          res.tpe     = tpe1
          res.build
        case _ => config0
      }
      val svmParam  = config.toLibSVM
      val svmProb   = new svm_problem
      svmProb.l     = labelled.length
      svmProb.y     = labelled.map { l => if (l.label) 1.0 else 0.0 }
      val normalize = config.normalize
      svmProb.x     = labelled.map { l =>
        var idx   = 0
        val arr   = l.vec
        val nodes = new Array[svm_node](arr.length)
        while (idx < arr.length) {
          val n       = new svm_node
          n.index     = idx + 1
          val value   = arr(idx)
          val valueN  = if (normalize) (value - statMean(idx)) / statStdDev(idx) else value
          n.value     = valueN
          nodes(idx)  = n
          idx        += 1
        }
        nodes
      } // (breakOut)

      val svmModel  = svm.svm_train(svmProb, svmParam)

      progress = 0.95
      checkAborted()

      val futRes = SoundProcesses.atomic[S, Trained[S]] { implicit tx =>
        val id    = tx.newId()
        val model = new Impl[S](id, config, svmModel, stats): SVMModel[S]
        tx.newHandle(model)
      }

      val result = Await.result(futRes, Duration(30, TimeUnit.SECONDS))

      progress = 1.0
      result
    }
  }

  private final val SER_VERSION = 1

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): SVMModel[S] = {
    val constCookie = in.readByte()
    if (constCookie != 3) sys.error(s"Expected constant (3) identifier, found $constCookie")
    val id        = tx.readId(in, access)
    val ver       = in.readByte()
    if (ver != SER_VERSION) sys.error(s"Unsupported serialized version $ver (expectd $SER_VERSION)")
    val config    = SVMConfig.serializer.read(in)
    val m         = readModel(in, config)
    val stats     = Stats.serializer    .read(in)
    new Impl[S](id, config, m, stats)
  }

  private def readDoubleArray(in: DataInput): Array[Double] = {
    val sz  = in.readInt()
    if (sz < 0) return null
    val arr = new Array[Double](sz)
    var i = 0
    while (i < sz) {
      arr(i) = in.readDouble()
      i += 1
    }
    arr
  }

  private def readIntArray(in: DataInput): Array[Int] = {
    val sz  = in.readInt()
    if (sz < 0) return null
    val arr = new Array[Int](sz)
    var i = 0
    while (i < sz) {
      arr(i) = in.readInt()
      i += 1
    }
    arr
  }

  private def writeDoubleArray(arr: Array[Double], out: DataOutput): Unit = {
    val sz = if (arr == null) -1 else arr.length
    out.writeInt(sz)
    var i = 0
    while (i < sz) {
      out.writeDouble(arr(i))
      i += 1
    }
  }

  private def writeIntArray(arr: Array[Int], out: DataOutput): Unit = {
    val sz = if (arr == null) -1 else arr.length
    out.writeInt(sz)
    var i = 0
    while (i < sz) {
      out.writeInt(arr(i))
      i += 1
    }
  }

  private def readModel(in: DataInput, config: SVMConfig): svm_model = {
    val m         = new svm_model
    m.param       = config.toLibSVM
    m.nr_class    = in.readInt()
    m.l           = in.readInt()
    val numIndiv  = in.readInt()
    val sv        = new Array[Array[svm_node]](numIndiv)
    var i = 0
    while (i < numIndiv) {
      val numFeat = in.readInt()
      val nodes   = new Array[svm_node](numFeat)
      var j = 0
      while (j < numFeat) {
        val index = in.readInt()
        val value = in.readDouble()
        val n     = new svm_node
        n.index   = index
        n.value   = value
        nodes(j)  = n
        j += 1
      }
      sv(i) = nodes
      i += 1
    }
    m.SV        = sv
    val numRows = in.readInt()
    val svCoef  = new Array[Array[Double]](numRows)
    i = 0
    while (i < numRows) {
      val column  = readDoubleArray(in)
      svCoef(i) = column
      i += 1
    }
    m.sv_coef = svCoef

    m.rho         = readDoubleArray(in)
    m.probA       = readDoubleArray(in)
    m.probB       = readDoubleArray(in)
    m.sv_indices  = readIntArray   (in)
    m.label       = readIntArray   (in)
    m.nSV         = readIntArray   (in)

    m
  }
  
  private def writeModel(m: svm_model, out: DataOutput): Unit = {
    out.writeInt(m.nr_class)
    out.writeInt(m.l       )
    val sv        = m.SV
    val numIndiv  = sv.length
    out.writeInt(numIndiv)
    var i = 0
    while (i < numIndiv) {
      val nodes   = sv(i)
      val numFeat = nodes.length
      out.writeInt(numFeat)
      var j = 0
      while (j < numFeat) {
        val n = nodes(j)
        out.writeInt   (n.index)
        out.writeDouble(n.value)
        j += 1
      }
      i += 1
    }
    val svCoef  = m.sv_coef
    val numRows = svCoef.length
    out.writeInt(numRows)
    i = 0
    while (i < numRows) {
      val column = svCoef(i) 
      writeDoubleArray(column, out)
      i += 1
    }
    m.sv_coef = svCoef

    writeDoubleArray(m.rho        , out)
    writeDoubleArray(m.probA      , out)
    writeDoubleArray(m.probB      , out)
    writeIntArray   (m.sv_indices , out)
    writeIntArray   (m.label      , out)
    writeIntArray   (m.nSV        , out)
  }

  private def mkNodes(in: IndexedSeq[Double], stats: Stats, normalize: Boolean): Array[svm_node] = {
    val nodes = new Array[svm_node](in.length)
    var idx   = 0
    while (idx < in.length) {
      val n       = new svm_node
      n.index     = idx + 1
      val value   = in(idx)
      val valueN  = if (normalize) (value - stats.features(idx).mean) / stats.features(idx).stdDev else value
      n.value     = valueN
      nodes(idx)  = n
      idx        += 1
    }
    nodes
  }

  private final class Run(val vec: Vec[Double], val folderIdx: Int) {
    var label = false
  }

  private final class PredictImpl[S <: Sys[S]](m: SVMModel[S],
                                               negatumH: stm.Source[S#Tx, Negatum[S]], numCoeff: Int)
                                              (implicit protected val cursor: stm.Cursor[S])
    extends RenderingImpl[S, Int, Int] {

    override def toString = s"SVMModel.predict@${hashCode.toHexString}"

    protected def fillResult(out: Int)(implicit tx: S#Tx): Int = out

    protected def body(): Int = blocking {
      val procFeatFut = SoundProcesses.atomic[S, Processor[Unit]] { implicit tx =>
        SVMFeatures(negatumH(), numCoeff = numCoeff, overwrite = false)
      }
      val procFeat = Await.result(procFeatFut, Duration(10, TimeUnit.SECONDS))
      await(procFeat, offset = 0.0, weight = 0.8)

      val vecSize = numCoeff * 2
      val featFut = SoundProcesses.atomic[S, Array[Run]] { implicit tx =>
        val f = negatumH().population
        f.iterator.zipWithIndex.flatMap { case (obj, fIdx) =>
          obj.attr.$[DoubleVector](Negatum.attrFeatures).map(_.value).filter(_.size == vecSize)
            .map { vec => new Run(vec, folderIdx = fIdx) }
        } .toArray
      }

      val runs = Await.result(featFut, Duration.Inf)
      var i = 0
      while (i < runs.length) {
        val f     = runs(i)
        val label = m.predictOne(f.vec)
        f.label   = label > 0.5
        i += 1
      }

      progress = 0.9
      checkAborted()

      val futAssign = SoundProcesses.atomic[S, Unit] { implicit tx =>
        val folder  = negatumH().population
        val fIter   = folder.iterator.zipWithIndex
        val rIter   = runs.iterator

        while (rIter.hasNext && fIter.hasNext) {
          val r     = rIter.next()
          var done  = false
          while (!done && fIter.hasNext) {
            fIter.next() match {
              case (obj, r.folderIdx) =>
                obj.attr.$[BooleanObj](Negatum.attrSelected) match {
                  case Some(BooleanObj.Var(vr)) => vr() = r.label
                  case _ => obj.attr.put(Negatum.attrSelected, BooleanObj.newConst[S](r.label))
                }
                done = true

              case _ =>
            }
          }
        }
      }
      val assnTimeOut = math.max(30.0, runs.length * 0.05)
      Await.result(futAssign, Duration(assnTimeOut, TimeUnit.SECONDS))

      progress = 1.0
      runs.count(_.label)
    }
  }

  private final class Impl[S <: Sys[S]](val id: S#Id, val config: SVMConfig, peer: svm_model,
                                        val stats: Stats)
    extends SVMModel[S] with ConstObjImpl[S, Any] {

    override def toString = s"SVMModel$id"

    def tpe: Obj.Type = SVMModel

    private[this] lazy val numFeatures = stats.features.size

    def numCoeff: Int = numFeatures >> 1

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(SER_VERSION)
      SVMConfig.serializer.write(config, out)
      writeModel                (peer  , out)
      Stats.serializer    .write(stats , out)
    }

    def predictOne(vec: Vec[Double]): Double = {
      require(vec.size == numFeatures,
        s"predict - given vector has size ${vec.size}, but model has size $numFeatures")
      val nodes = mkNodes(vec, stats, normalize = config.normalize)
      svm.svm_predict(peer, nodes)
    }

    def predict(n: Negatum[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): Rendering[S, Int] = {
      val res = new PredictImpl(this, tx.newHandle(n), numCoeff = numCoeff)
      res.startTx()
      res
    }

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl[Out](txOut.newId(), config = config, peer = peer, stats = stats)
  }
}