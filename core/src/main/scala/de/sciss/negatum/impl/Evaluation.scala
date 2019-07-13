/*
 *  Evaluation.scala
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
package impl

import de.sciss.file._
import de.sciss.lucre.synth.InMemory
import de.sciss.negatum.Negatum.Config
import de.sciss.numbers
import de.sciss.processor.Processor
import de.sciss.span.Span
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc.{Bounce, Proc, TimeRef, Universe}

import scala.concurrent.{ExecutionContext, Future}

object Evaluation {
  private[this] val inMemory = InMemory()

  def apply(config: Config, graph: SynthGraph, inputSpec: AudioFileSpec,
            inputExtr: File, numVertices: Int)(implicit exec: ExecutionContext): Future[Float] = {
    import config.eval
    import config.eval._
    import config.gen._
    import config.penalty._
    val audioF  = File.createTemp(prefix = "muta_bnc", suffix = ".aif")
    val bnc0    = Evaluation.bounce(graph, audioF = audioF, inputSpec = inputSpec)
    val simFut  = bnc0.flatMap { _ =>
      val featCfg = Features.Config(
        minFreq = eval.minFreq,
        maxFreq = eval.maxFreq,
        numMFCC = eval.numMFCC,
        numMel  = eval.numMel
      )
      Features.correlate(bounceF = audioF, inputSpec = inputSpec, inputFeatureF = inputExtr,
        config = featCfg, maxBoost = maxBoost, temporalWeight = timeWeight)
    }
    val res = simFut.map { sim0 =>
      import numbers.Implicits._
      val pen = vertexPenalty
      //      if (sim0 > 0.46) {
      //        println(s"DEBUG $audioF")
      //      }
      val sim = if (pen <= 0 || minVertices == maxVertices) sim0 else
        sim0 - numVertices.clip(minVertices, maxVertices).linLin(minVertices, maxVertices, 0, pen)
      sim.toFloat // new Evaluated(cH, sim)
    }
    res.onComplete(_ =>
      audioF.delete())
    res
  }


  /** Bounces a synth def to an audio file.nce
    *
    * @param graph       the synth graph to play and evaluate
    * @param audioF      the audio output file to bounce to
    * @param inputSpec   the spec of the original target sound
    * @param duration0   the duration to bounce in seconds or `-1` to bounce the duration of the target sound
    */
  def bounce(graph: SynthGraph, audioF: File, inputSpec: AudioFileSpec, duration0: Double = -1)
            (implicit exec: ExecutionContext): Future[Any] = {
    val duration    = if (duration0 > 0) duration0 else inputSpec.numFrames.toDouble / inputSpec.sampleRate
    val sampleRate  = inputSpec.sampleRate.toInt
    bounce1(graph = graph, audioF = audioF, duration = duration, sampleRate = sampleRate)
  }

  def bounce1(graph: SynthGraph, audioF: File, duration: Double, sampleRate: Int)
             (implicit exec: ExecutionContext): Future[Any] = {
    def invoke() = bounce2(graph = graph, audioF = audioF, duration = duration, sampleRate = sampleRate)

    def recover(f: Future[Any]): Future[Any] =
      f.recoverWith {
        case ex: Processor.Aborted => Future.failed(ex)
        case _ => invoke()
      }

    // scsynth seems to crash randomly. give it three chances
    val proc1 = invoke()
    val proc2 = recover(proc1)
    val proc3 = recover(proc2)
    proc3
  }

  private def bounce2(graph: SynthGraph, audioF: File, duration: Double, sampleRate: Int)
                     (implicit exec: ExecutionContext): Processor[Any] = {
    type I  = InMemory
    implicit val iCursor: I = inMemory

    // val exp = ExprImplicits[I]

    val (objH, _u) = inMemory.step { implicit tx =>
      val proc      = Proc[I]
      proc.graph()  = graph
      tx.newHandle(proc) -> Universe.dummy[I]
    }
    implicit val u: Universe[I] = _u

    val bncCfg              = Bounce.Config[I]
    bncCfg.group            = objH :: Nil
    // val audioF           = File.createTemp(prefix = "muta_bnc", suffix = ".aif")
    val sCfg                = bncCfg.server
    sCfg.nrtOutputPath      = audioF.path
    sCfg.inputBusChannels   = 0
    sCfg.outputBusChannels  = 1
    sCfg.wireBuffers        = 1024 // higher than default
    sCfg.blockSize          = 64   // keep it compatible to real-time
    sCfg.sampleRate         = sampleRate
    // bc.init : (S#Tx, Server) => Unit
    bncCfg.span             = Span(0L, (duration * TimeRef.SampleRate).toLong)
    val bnc0                = Bounce[I]().apply(bncCfg)
    // tx.afterCommit {
    bnc0.start()
    // }
    bnc0
  }
}