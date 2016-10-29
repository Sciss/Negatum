/*
 *  SVMModelImpl.scala
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
package impl

import java.util.concurrent.TimeUnit

import de.sciss.lucre.event.impl.ConstObjImpl
import de.sciss.lucre.expr.{BooleanObj, DoubleVector}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Copy, Elem, Obj, Sys}
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.synth.proc.SoundProcesses
import libsvm.{svm, svm_model, svm_problem}

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, blocking}

object SVMModelImpl {
  def train[S <: Sys[S]](n: ISeq[Negatum[S]], config: SVMConfig, numCoeff: Int)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): Processor[SVMModel[S]] = {
    val res = new TrainImpl[S](config, numCoeff = numCoeff, negatumH = n.map(tx.newHandle(_))(breakOut))
    tx.afterCommit {
      import ExecutionContext.Implicits.global
      res.start()
    }
    res
  }

  private final class LabelledFeatures(val vec: Vec[Double], val label: Boolean)

  private final class TrainImpl[S <: Sys[S]](config: SVMConfig, numCoeff: Int,
                                             negatumH: List[stm.Source[S#Tx, Negatum[S]]])
                                            (implicit cursor: stm.Cursor[S])
    extends ProcessorImpl[SVMModel[S], Processor[SVMModel[S]]] with Processor[SVMModel[S]] {

    protected def body(): SVMModel[S] = blocking {
      // ---- ensure features have been extracted ----

      val procFeatW = 1.0 / negatumH.size

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
      val futLabelled: Future[Vec[LabelledFeatures]] =
        SoundProcesses.atomic[S, Vec[LabelledFeatures]] { implicit tx =>
          negatumH.flatMap { nS =>
            val f = nS().population
            f.iterator.flatMap { obj =>
              val attr = obj.attr
              for {
                feat <- attr.$[DoubleVector](Negatum.attrFeatures).map(_.value)
                if feat.size == vecSize
              } yield {
                val isIn = attr.$[BooleanObj](Negatum.attrSelected).exists(_.value)
                new LabelledFeatures(feat, label = isIn)
              }
            } .toVector
          } (breakOut)
        }

      val labelled  = Await.result(futLabelled, Duration(30, TimeUnit.SECONDS))
      val svmParam  = config.toLibSVM
      val svmProb   = new svm_problem
      svmProb.l     = labelled.size
      svmProb.y     = labelled.map(l => if (l.label) 1.0 else 0.0)(breakOut)
      svmProb.x     = ???
      val svmModel  = svm.svm_train(svmProb, svmParam)

      val futRes = SoundProcesses.atomic[S, SVMModel[S]] { implicit tx =>
        val id = tx.newID()
        new Impl[S](id, config, svmModel)
      }
      Await.result(futRes, Duration(30, TimeUnit.SECONDS))
    }
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): SVMModel[S] = ???

  private final class Impl[S <: Sys[S]](val id: S#ID, val config: SVMConfig, peer: svm_model)
    extends SVMModel[S] with ConstObjImpl[S, Any] {

    def tpe: Obj.Type = SVMModel

    protected def writeData(out: DataOutput): Unit = ???

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl[Out](txOut.newID(), config = config, peer = peer)
  }
}