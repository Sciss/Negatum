/*
 *  Optimize.scala
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

import de.sciss.file.File
import de.sciss.lucre.synth.InMemory
import de.sciss.model.Model
import de.sciss.negatum.impl.{Chromosome, MkSynthGraph, MkTopology}
import de.sciss.numbers
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{Processor, ProcessorFactory}
import de.sciss.span.Span
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{Bounce, Proc, TimeRef, Universe}

import scala.concurrent.blocking

object Optimize extends ProcessorFactory {
  type Repr = Generic

  /** The processor result.
    *
    * @param graph      the optimized graph
    * @param numConst   the number of UGens replaced by constants
    * @param numEqual   the number of UGens removed because they were equal to other UGens
    */
  final case class Result(graph: SynthGraph, numConst: Int, numEqual: Int)

  type Product = Result

  /** Optimizes a graph by replacing effectively constant UGen signals by
    * scalar constants, and by removing redundant UGens that produce the
    * same signals as other UGens.
    *
    * @param  graph         the graph to optimize
    * @param  sampleRate    the sample rate to use during internal bouncing
    * @param  analysisDur   the duration of the signal analysis in seconds. The process only
    *                       looks at this interval to determine which UGens are identical
    */
  final case class Config(graph: SynthGraph, sampleRate: Double, analysisDur: Double = 2.0) {
    require (analysisDur > 0.0)

    override def toString = s"$productPrefix(SynthGraph@${graph.hashCode().toHexString}, $sampleRate, $analysisDur)"
  }

  protected def prepare(config: Config): Prepared = {
    new Impl(config)
  }

  private final class Impl(config: Config) extends ProcessorImpl[Product, Processor[Product]]
    with Processor[Product] {

    protected def body(): Product = {
      import config.{graph => graphIn}
      var idxMap      = Map.empty[Int, Int] // channels to indices in synth-graph sources
      var idxGE       = 0
      var idxLazy     = 0
      val graphAdd = SynthGraph {
        import de.sciss.synth._
        import de.sciss.synth.ugen._
        val sigB    = List.newBuilder[GE]
        graphIn.sources.foreach {
          case _: NegatumOut | _: NegatumIn | _: Protect | _: Mix =>  // these are filtered by MkTopology
            idxLazy += 1
          case lz =>
            lz match {
              case in: GE =>
                sigB    += in
                idxMap  += idxGE -> idxLazy
                idxGE   += 1
              case _ =>
            }
            idxLazy += 1
        }
        val sig     = sigB.result()
//        (numSignals: GE).poll(0, "numSignals")
        ReplaceOut.ar(0, sig)
      }

      val numSignals        = idxGE
      val (topIn, srcMapIn) = MkTopology.withSourceMap(graphIn)
      assert (srcMapIn.size == numSignals)

//      val DEBUG_IDX = idxMap  .toList.sortBy(_._1)
//      val DEBUG_SRC = srcMapIn.toList.sortBy(_._1)

      if (numSignals == 0) throw new IllegalArgumentException("Input graph is empty")

      // XXX TODO -- we don't know the limit; let's just print a warning for now
      if (numSignals > 1024) {
        println(s"WARNING: negatum.Optimize -- number of input signals is $numSignals -- likely too high!")
      }

      val graphBnc = graphIn.copy(sources = graphIn.sources ++ graphAdd.sources)

      type S = InMemory
      implicit val cursor: S = InMemory()

      implicit val u: Universe[S] = cursor.step { implicit tx =>
        Universe.dummy
      }

      val bnc       = Bounce[S]()
      val bCfg      = Bounce.Config[S]()
      bCfg.realtime = false
      bCfg.span     = Span(0L, (config.analysisDur * TimeRef.SampleRate).toLong)
      bCfg.server.outputBusChannels = numSignals
      import numbers.Implicits._
      bCfg.server.audioBusChannels  = math.max(128, numSignals.nextPowerOfTwo)  // ScalaCollider #84
      bCfg.server.sampleRate        = config.sampleRate.toInt
      bCfg.group    = cursor.step { implicit tx =>
        val p = Proc[S]
        p.graph() = graphBnc
        tx.newHandle(p) :: Nil
      }

      val pBnc: Processor[File] = bnc.run(bCfg)(Model.EmptyListener)

      val fBnc = await(pBnc, weight = 0.5)

      try {
        val afBnc = blocking { AudioFile.openRead(fBnc) }
        try {
          assert (afBnc.numChannels == numSignals)
          checkAborted()

          // println(f"Bounce length: ${afBnc.numFrames / afBnc.sampleRate}%1.1f sec.")
          val buf = afBnc.buffer(afBnc.numFrames.toInt)
          blocking { afBnc.read(buf) }
          var analysisMap = Map.empty[Int, Either[Float, Int]]
          for (ch1 <- buf.indices) {
            val b1 = buf(ch1)
            val v0 = b1(0)
            if (b1.forall(_ == v0)) {
              analysisMap += ch1 -> Left(v0)
            } else {
              for (ch2 <- 0 until ch1; if !analysisMap.contains(ch1) && !analysisMap.contains(ch2)) {
                val b2 = buf(ch2)
                if (b1 sameElements b2) {
                  analysisMap += ch1 -> Right(ch2)
                }
              }
            }

            checkAborted()
//            progress = 0.5 + 0.5 * (ch1 + 1) / numSignals
            progress = 0.5 + 0.5 * ch1 / numSignals
          }

          // _NOT_: highest indices first, so the successive indices
          // stay valid while we manipulate the topology
          println("DEBUGGING -- CONTINUE HERE")
//          val analysis = analysisMap.toList.sortBy(-_._1)
          // without further analysis, removing from higher to
          // lower indices leaves an incomplete graph. works
          // correctly in normal order (we use the indices only
          // in static maps, so they remain valid anyway)
          val analysis = analysisMap.toList.sortBy(_._1)

//          println(s"srcMapIn.size = ${srcMapIn.size}")

          val topOut = analysis.foldLeft(topIn) {
            case (topTemp, (ch, eth)) =>
              val srcIdx  = idxMap(ch)
              val vOld    = srcMapIn(srcIdx)
              val vNew    = eth match {
                case Left  (value) => Vertex.Constant(value)
                case Right (chTgt) =>
                  val srcIdxTgt = idxMap(chTgt)
                  val _vNew = srcMapIn(srcIdxTgt)
//                  assert (topTemp.vertices.contains(_vNew))
                  _vNew
              }
              println(s"Replace $vOld by $vNew")
              // Note: a UGen vNew may also not be in the current topology,
              // because it may have been removed before as an orphan in the
              // process. Thus, always ensure it is added to the topology
              val topTemp1 = if (vNew.isConstant || !topTemp.vertices.contains(vNew)) {
                topTemp.addVertex(vNew)
              } else {
                topTemp
              }
              val topTemp2 = Chromosome.replaceVertex(topTemp1, vOld = vOld, vNew = vNew)
//              Chromosome.checkComplete(topTemp2, "Oh noes")
              topTemp2
          }

          val graphOut    = MkSynthGraph(topOut)
          val countConst  = analysis.count(_._2.isLeft  )
          val countEqual  = analysis.count(_._2.isRight )
          progress = 1.0
          Result(graphOut, numConst = countConst, numEqual = countEqual)

        } finally {
          afBnc.close()
        }
      } finally {
        fBnc.delete()
      }
    }
  }
}