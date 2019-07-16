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
import de.sciss.negatum.Negatum.SynthGraphT
import de.sciss.negatum.impl.{Chromosome, MkSynthGraph, MkTopology, Util}
import de.sciss.numbers
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{Processor, ProcessorFactory}
import de.sciss.span.Span
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{Bounce, Proc, TimeRef, Universe}
import de.sciss.synth.ugen.Protect

import scala.annotation.tailrec
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
      val graphAdd = SynthGraph {
        import de.sciss.synth._
        import de.sciss.synth.ugen._
        val sigB    = List.newBuilder[GE]
        graphIn.sources.iterator.zipWithIndex.foreach { case (lz, idxLazy) =>
          lz match {
            case _: NegatumOut | _: NegatumIn /*| _: Protect*/ | _: Mix =>  // these are filtered by MkTopology
//            case p @ Protect(in, _, _, _) =>
//              val idxPeer = graphIn.sources.indexOf(in)
//              // assert (idxPeer >= 0, in.toString)
//              if (idxPeer >= 0) { // ignore constants for now
//                sigB    += p
//                idxMap  += idxGE -> idxPeer
//                idxGE   += 1
//              }

            case in: GE =>
              sigB    += in
              idxMap  += idxGE -> idxLazy
              idxGE   += 1

            case _ =>
          }
        }
        val sig     = sigB.result()
//        (numSignals: GE).poll(0, "numSignals")
        ReplaceOut.ar(0, sig)
      }

      val numSignals = idxGE
      // we do not remove the `Protect` elements, because we may want to replace them.
      // what is important is that we explicitly remove them from the topology after
      // the optimization, before calling `MkSynthGraph` which cannot handle them
      // (it inserts them again by itself where needed).
      val (topIn, srcMapIn) = MkTopology.withSourceMap(graphIn, removeProtect = false)
//      assert (srcMapIn.size == numSignals)

//      val DEBUG_IDX = idxMap  .toList.sortBy(_._1)
//      val DEBUG_SRC = srcMapIn.toList.sortBy(_._1)

      if (numSignals == 0) throw new IllegalArgumentException("Input graph is empty")
      println(s"numSignals = $numSignals")

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
          val analysis    = analysisMap.toList.sortBy(_._1) // .take(21)
          val countConst  = analysis.count(_._2.isLeft  )
          val countEqual  = analysis.count(_._2.isRight )
          println(s"countConst = $countConst, countEqual = $countEqual")

//          println(s"srcMapIn.size = ${srcMapIn.size}")
//          val LAST = analysis.last._1

          val rootsIn       = Util.getGraphRoots(topIn)
          var rootsOutSet   = rootsIn.toSet
          println(s"---- ${rootsIn.size} ROOTS IN ----")
          rootsIn.foreach { v =>
            println(v)
          }
          println()

          val topOut0 = analysis.foldLeft(topIn) {
            case (topTemp, (ch, eth)) =>
              val srcIdx  = idxMap(ch)
              val vOld    = srcMapIn(srcIdx)
              val vNew    = eth match {
                case Left  (value) =>
                  val _vNew = Vertex.Constant(value)
                  if (rootsOutSet.contains(vOld)) {
                    rootsOutSet -= vOld
                  }
                  _vNew
                case Right (chTgt) =>
                  val srcIdxTgt = idxMap(chTgt)
                  val _vNew: Vertex.UGen = srcMapIn(srcIdxTgt)
                  if (rootsOutSet.contains(vOld)) {
                    rootsOutSet = rootsOutSet - vOld + _vNew
                  }
//                  assert (topTemp.vertices.contains(_vNew))
                  _vNew
              }
//              if (ch == LAST) {
//                println("---here")
//              }
              println(s"Replace $vOld by $vNew")
              // Note: a UGen vNew may also not be in the current topology,
              // because it may have been removed before as an orphan in the
              // process. Thus, always ensure it is added to the topology
              val topTemp1 = if (vNew.isConstant /*|| !topTemp.vertices.contains(vNew)*/) {
                topTemp.addVertex(vNew)
              } else {
                topTemp
              }
              val topTemp2 = if (vOld == vNew) topTemp1 else {
                Chromosome.replaceVertex(topTemp1, vOld = vOld, vNew = vNew)
              }
//              Chromosome.checkComplete(topTemp2, "Oh noes")
              topTemp2
          }

          val protects = topOut0.vertices.collect {
            case v: Vertex.UGen if v.info.name == "Protect" => v
          }

          println(s"Removing ${protects.size} Protect elements now")
          val topOut1 = protects.foldLeft(topOut0) { (topTmp1, pr) =>
            Chromosome.removeVertex(topTmp1, pr)
          }

          val rootsOut = rootsOutSet.toList

          @tailrec
          def removeOrphans(topTmp: SynthGraphT): SynthGraphT = {
            val rootsDirty = Util.getGraphRoots(topTmp)
            val orphans = rootsDirty diff rootsOut /* rootsIn */
            if (orphans.isEmpty) topTmp else {
              val topTmp1 = orphans.foldLeft(topTmp) { (topTmp1, orphan) =>
                Chromosome.removeVertex(topTmp1, orphan)
              }
              removeOrphans(topTmp1)
            }
          }

          val topOut      = removeOrphans(topOut1)
          val graphOut    = MkSynthGraph(topOut)

          {
            val rootsOutTest = Util.getGraphRoots(topOut)
            println(s"---- ${rootsOutTest.size} ROOTS OUT ----")
            rootsOutTest.foreach { v =>
              println(v)
            }
            println()
          }

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