/*
 *  SVMFeaturesImpl.scala
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

import de.sciss.file.File
import de.sciss.lucre.expr.DoubleVector
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Folder, Sys}
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.synth.SynthGraph
import de.sciss.proc.{AudioCue, Proc, SoundProcesses}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, blocking}
import scala.util.control.NonFatal

object SVMFeaturesImpl {
  def apply[S <: Sys[S]](n: Negatum[S], numCoeff: Int, overwrite: Boolean)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): Processor[Unit] = {
    val folder = n.population
    val sel1 = folder.iterator.zipWithIndex.collect {
      case (p: Proc[S], fIdx) => (p, fIdx)
    }
    val vecSize = numCoeff * 2
    val sel2 = if (overwrite) sel1 else sel1.filterNot { case (p, _) =>
      p.attr.$[DoubleVector](Negatum.attrFeatures).map(_.value.size).contains(vecSize)
    }
    val runs = sel2.map { case (p, folderIdx) =>
      val graph = p.graph.value
      new Run(graph = graph, folderIdx = folderIdx)
    }
    val res = new Impl[S](runs.toVector, numCoeff = numCoeff, template = n.template.value,
      folderH = tx.newHandle(folder))
    tx.afterCommit {
      import ExecutionContext.Implicits.global
      res.start()
    }
    res
  }

  private final class Run(val graph: SynthGraph, val folderIdx: Int) {
    var result = Vector.empty[Double]
  }

  private final class Impl[S <: Sys[S]](runs: Vector[Run], numCoeff: Int, template: AudioCue,
                                        folderH: stm.Source[S#Tx, Folder[S]])
                                       (implicit cursor: stm.Cursor[S])
    extends ProcessorImpl[Unit, Processor[Unit]] with Processor[Unit] {

    protected def body(): Unit = blocking {
      val bncTimeOut = math.max(10.0, template.spec.numFrames / template.spec.sampleRate)
      runs.iterator.zipWithIndex.foreach { case (r, rIdx) =>
        try {
          val audioF = File.createTemp(suffix = ".aif")
          try {
            val proc = Evaluation.bounce(graph = r.graph, audioF = audioF, inputSpec = template.spec)
            Await.result(proc, Duration(bncTimeOut, TimeUnit.SECONDS))
            // val spec = AudioFile.readSpec(audioF)
            val wOpt = Weight(audioF, numCoeff = numCoeff)
            wOpt.foreach { w =>
              r.result = w.spectral.toVector ++ w.temporal.toVector
            }
          } finally {
            audioF.delete()
          }
        } catch { // do not fail the entire process if individuals cannot be properly bounced
          case NonFatal(ex) => ex.printStackTrace()
        }
        progress = (rIdx + 1).toDouble / runs.size
        checkAborted()
      }

      val futAssign = SoundProcesses.atomic[S, Unit] { implicit tx =>
        val folder  = folderH()
        val fIter   = folder.iterator.zipWithIndex
        val rIter   = runs.iterator.filter(_.result.nonEmpty)

        while (rIter.hasNext && fIter.hasNext) {
          val r     = rIter.next()
          var done  = false
          while (!done && fIter.hasNext) {
            fIter.next() match {
              case (obj, r.folderIdx) =>
                obj.attr.$[DoubleVector](Negatum.attrFeatures) match {
                  case Some(DoubleVector.Var(vr)) => vr() = r.result
                  case _ => obj.attr.put(Negatum.attrFeatures, DoubleVector.newConst[S](r.result))
                }
                done = true

              case _ =>
            }
          }
        }
      }
      val assnTimeOut = math.max(30.0, runs.size * 0.05)
      Await.result(futAssign, Duration(assnTimeOut, TimeUnit.SECONDS))
    }
  }
}