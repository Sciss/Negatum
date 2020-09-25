/*
 *  Evaluation.scala
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

import de.sciss.file._
import de.sciss.lucre.swing.LucreSwing.defer
import de.sciss.lucre.synth.InMemory
import de.sciss.mellite.{Application, Prefs}
import de.sciss.model.impl.ModelImpl
import de.sciss.negatum.Negatum.Config
import de.sciss.numbers
import de.sciss.processor.Processor
import de.sciss.processor.impl.FutureProxy
import de.sciss.span.Span
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc.{Bounce, Proc, TimeRef, Universe}

import scala.concurrent.{ExecutionContext, Future, Promise}

object Evaluation {
  private[this] val inMemory = InMemory()

  private final class Apply(bounce: Processor[Any], val peerFuture: Future[Float])
                           (implicit exec: ExecutionContext)
    extends Processor[Float]
      with FutureProxy[Float]
      with ModelImpl[Processor.Update[Float, Processor[Float]]] {

    def abort() : Unit    = bounce.abort()
    def progress: Double  = if (peerFuture.isCompleted) 1.0 else bounce.progress

    peerFuture.onComplete(res => dispatch(Processor.Result(this, res)))
  }

  def apply(config: Config, graph: SynthGraph, inputSpec: AudioFileSpec,
            inputExtr: File, numVertices: Int)(implicit exec: ExecutionContext): Processor[Float] = {
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
    res.onComplete { _ =>
      audioF.delete()
    }

    new Apply(bnc0, res)
  }

  /** Bounces a synth def to an audio file.nce
    *
    * @param graph       the synth graph to play and evaluate
    * @param audioF      the audio output file to bounce to
    * @param inputSpec   the spec of the original target sound
    * @param duration0   the duration to bounce in seconds or `-1` to bounce the duration of the target sound
    */
  def bounce(graph: SynthGraph, audioF: File, inputSpec: AudioFileSpec, duration0: Double = -1)
            (implicit exec: ExecutionContext): Processor[Any] = {
    val duration    = if (duration0 > 0) duration0 else inputSpec.numFrames.toDouble / inputSpec.sampleRate
    val sampleRate  = inputSpec.sampleRate.toInt
    bounce1(graph = graph, audioF = audioF, duration = duration, sampleRate = sampleRate)
  }

  private final class DeferredBounce(graph: SynthGraph, audioF: File, duration: Double, sampleRate: Int)
                                    (implicit exec: ExecutionContext)
    extends Processor[Any]
      with FutureProxy[Any]
      with ModelImpl[Processor.Update[Any, Processor[Any]]] {

    private[this] val p = Promise[Any]()

    lazy val processor: Processor[Any] = {
      val res = bounce2(graph = graph, audioF = audioF, duration = duration, sampleRate = sampleRate)
      p.completeWith(res)
      res
    }

    defer {
      processor
    }

    protected def peerFuture: Future[Any] = p.future

    def abort(): Unit = defer {
      processor.abort()
    }

    def progress: Double = if (peerFuture.isCompleted) 1.0 else 0.0 // bounce.progress
  }

  def bounce1(graph: SynthGraph, audioF: File, duration: Double, sampleRate: Int)
             (implicit exec: ExecutionContext): Processor[Any] = {

    new DeferredBounce(graph = graph, audioF = audioF, duration = duration,
      sampleRate = sampleRate)

//    def recover(f: Future[Any]): Future[Any] =
//      f.recoverWith {
//        case ex: Processor.Aborted => Future.failed(ex)
//        case _ => invoke()
//      }

//    // scsynth seems to crash randomly. give it three chances
//    val proc1 = invoke()
//    val proc2 = recover(proc1)
//    val proc3 = recover(proc2)
//    proc3
  }

  // must run on EDT because of preferences
  private def bounce2(graph: SynthGraph, audioF: File, duration: Double, sampleRate: Int)
                     (implicit exec: ExecutionContext): Processor[Any] = {
    type I  = InMemory.Txn
    implicit val iCursor: InMemory = inMemory

    // val exp = ExprImplicits[I]

    val (objH, _u) = inMemory.step { implicit tx =>
      val proc      = Proc[I]()
      proc.graph()  = graph
      tx.newHandle(proc) -> Universe.dummy[I]
    }
    implicit val u: Universe[I] = _u

    val bncCfg              = Bounce.Config[I]()
    bncCfg.group            = objH :: Nil
    Application.applyAudioPreferences(bncCfg.server, bncCfg.client, useDevice = false, pickPort = false)
    val sCfg                = bncCfg.server
    sCfg.nrtOutputPath      = audioF.path
    sCfg.inputBusChannels   = 0
    sCfg.outputBusChannels  = 1
    val numPrivate = Prefs.audioNumPrivate.getOrElse(Prefs.defaultAudioNumPrivate)
    import numbers.Implicits._
    sCfg.audioBusChannels   = (sCfg.outputBusChannels + numPrivate).nextPowerOfTwo
    sCfg.wireBuffers        = math.max(sCfg.wireBuffers, 1024) // possibly higher than default
//    sCfg.blockSize          = 64   // configurable through Mellite preferences now
    sCfg.sampleRate         = sampleRate
    // bc.init : (T, Server) => Unit
    bncCfg.span             = Span(0L, (duration * TimeRef.SampleRate).toLong)
    val bnc0                = Bounce[I]().apply(bncCfg)
    // tx.afterCommit {
    bnc0.start()
    // }
    bnc0
  }
}