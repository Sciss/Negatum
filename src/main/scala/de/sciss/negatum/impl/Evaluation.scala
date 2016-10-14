/*
 *  Evaluation.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum
package impl

import java.util.concurrent.TimeUnit

import de.sciss.file._
import de.sciss.lucre.synth.InMemory
import de.sciss.negatum.Negatum.Config
import de.sciss.numbers
import de.sciss.processor.Processor
import de.sciss.span.Span
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc.{Bounce, Proc, TimeRef, WorkspaceHandle}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Evaluation {
  private[this] val inMemory = InMemory()

  def apply(config: Config, graph: SynthGraph, inputSpec: AudioFileSpec,
            inputExtr: File, numVertices: Int): Future[Float] = {
    import config.evaluation._
    import config.generation._
    import config.penalty._
    val audioF  = File.createTemp(prefix = "muta_bnc", suffix = ".aif")
    val bnc0    = Evaluation.bounce(graph, audioF = audioF, inputSpec = inputSpec)
    Await.result(bnc0, Duration(20, TimeUnit.SECONDS))
    val simFut  = Features.correlate(bounceF = audioF, inputSpec = inputSpec, inputExtr = inputExtr,
      numMFCC = numMFCC, normalizeMFCC = normalizeMFCC, maxBoost = maxBoost, temporalWeight = temporalWeight)

    import scala.concurrent.ExecutionContext.Implicits.global
    val res = simFut.map { sim0 =>
      import numbers.Implicits._
      val pen = vertexPenalty
      //      if (sim0 > 0.46) {
      //        println(s"DEBUG $audioF")
      //      }
      val sim = if (pen <= 0) sim0 else
        sim0 - numVertices.clip(minNumVertices, maxNumVertices).linlin(minNumVertices, maxNumVertices, 0, pen)
      sim.toFloat // new Evaluated(cH, sim)
    }
    res.onComplete(_ =>
      audioF.delete())
    res
  }

  /** Bounces a synth def to an audio file.
    *
    * @param graph       the synth graph to play and evaluate
    * @param audioF      the audio output file to bounce to
    * @param inputSpec   the spec of the original target sound
    * @param duration0   the duration to bounce in seconds or `-1` to bounce the duration of the target sound
    */
  def bounce(graph: SynthGraph, audioF: File, inputSpec: AudioFileSpec, duration0: Double = -1): Processor[Any] = {
    type I  = InMemory
    implicit val iCursor = inMemory

    // val exp = ExprImplicits[I]

    val objH = inMemory.step { implicit tx =>
      val proc      = Proc[I]
      proc.graph()  = graph
      // val procObj   = Obj(Proc.Elem(proc))
      tx.newHandle(proc) // (Obj.typedSerializer[I, Proc.Elem[I]])
    }

    import WorkspaceHandle.Implicits._
    val bncCfg              = Bounce.Config[I]
    bncCfg.group            = objH :: Nil
    // val audioF           = File.createTemp(prefix = "muta_bnc", suffix = ".aif")
    val duration            = if (duration0 > 0) duration0 else inputSpec.numFrames.toDouble / inputSpec.sampleRate
    val sCfg                = bncCfg.server
    sCfg.nrtOutputPath      = audioF.path
    sCfg.inputBusChannels   = 0
    sCfg.outputBusChannels  = 1
    sCfg.wireBuffers        = 1024 // higher than default
    sCfg.blockSize          = 64   // keep it compatible to real-time
    sCfg.sampleRate         = inputSpec.sampleRate.toInt
    // bc.init : (S#Tx, Server) => Unit
    bncCfg.span             = Span(0L, (duration * TimeRef.SampleRate).toLong)
    val bnc0                = Bounce[I, I].apply(bncCfg)
    // tx.afterCommit {
    import scala.concurrent.ExecutionContext.Implicits.global
    bnc0.start()
    // }
    bnc0
  }
}
