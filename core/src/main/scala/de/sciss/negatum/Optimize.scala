/*
 *  Optimize.scala
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

import de.sciss.file.File
import de.sciss.lucre.synth.InMemory
import de.sciss.model.Model
import de.sciss.negatum.Negatum.SynthGraphT
import de.sciss.negatum.impl.{Chromosome, MkSynthGraph, MkTopology, UGens, Util}
import de.sciss.numbers
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{Processor, ProcessorFactory}
import de.sciss.span.Span
import de.sciss.synth.SynthGraph
import de.sciss.audiofile.AudioFile
import de.sciss.proc.impl.MkSynthGraphSource
import de.sciss.proc.{Bounce, Proc, TimeRef, Universe}
import de.sciss.synth.ugen.{BinaryOpUGen, Protect}

import scala.annotation.tailrec
import scala.concurrent.blocking

object Optimize extends ProcessorFactory {
  type Repr = Generic

  var DEBUG = false

  /** maximum memory size in mega-bytes */
  var MAX_MEM_SIZE_MB = 128

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
    * @param  expandProtect if `true` (default), replaces `Protect` elements in the graph by their
    *                       expanded elements before running the optimization.
    * @param  expandIO      if `true` (default), replaces `NegatumIn` and `NegatumOut` elements
    *                       in the graph by their expanded elements before returning the optimization.
    */
  final case class Config(graph         : SynthGraph,
                          sampleRate    : Double,
                          analysisDur   : Double  = 2.0,
                          blockSize     : Int     = 64,
                          expandProtect : Boolean = true,
                          expandIO      : Boolean = true
                         ) {
    require (analysisDur > 0.0)

    override def toString = s"$productPrefix(SynthGraph@${graph.hashCode().toHexString}, $sampleRate, $analysisDur)"
  }

  protected def prepare(config: Config): Prepared = {
    new Impl(config)
  }

  private final class Impl(config: Config) extends ProcessorImpl[Product, Processor[Product]]
    with Processor[Product] {

    protected def body(): Product = {
      import config.{graph => graphIn0}
      var idxMap      = Map.empty[Int, Int] // channels to indices in synth-graph sources
      var idxGE       = 0
      val graphIn     = if (!config.expandProtect ||
        !graphIn0.sources.exists { case _: Protect => true; case _ => false }) graphIn0 else {

        val topTmp    = MkTopology(graphIn0)
        val _graphIn  = MkSynthGraph(topTmp, protect = true, expandProtect = true)
        if (DEBUG) {
          val sourceTmp = MkSynthGraphSource(_graphIn)
          println("AFTER EXPAND-PROTECT:")
          println(sourceTmp)
        }
        _graphIn
      }

      var rootsOutDC = 0.0

      val graphAdd = SynthGraph {
        import de.sciss.synth._
        import de.sciss.synth.ugen._
        val sigB    = List.newBuilder[GE]
        graphIn.sources.iterator.zipWithIndex.foreach { case (lz, idxLazy) =>
          lz match {
            // issue #8 -- capture DC constants in Mix.
            // the constants may be result of UnaryOpGen or BinaryOpUGen optimisation.
            // (this should be fixed now, but we keep it to fix existing graphs)
            // XXX TODO drop this in future versions
            case Mix(GESeq(elems)) if config.expandProtect => // XXX TODO this is all messy
              elems.foreach {
                case Constant(f) => rootsOutDC += f
                case _ =>
              }

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
      if (DEBUG) println(s"numSignals = $numSignals")

      // XXX TODO -- we don't know the limit; let's just print a warning for now
      if (numSignals > 1024) {
        println(s"WARNING: negatum.Optimize -- number of input signals is $numSignals -- likely too high!")
      }

      val graphBnc = graphIn.copy(sources = graphIn.sources ++ graphAdd.sources)

      type S = InMemory
      type T = InMemory.Txn
      implicit val cursor: S = InMemory()

      implicit val u: Universe[T] = cursor.step { implicit tx =>
        Universe.dummy
      }

      val bnc       = Bounce[T]()
      val bCfg      = Bounce.Config[T]()
      bCfg.realtime = false
      bCfg.span     = Span(0L, (config.analysisDur * TimeRef.SampleRate).toLong)
      bCfg.server.outputBusChannels = numSignals
      import numbers.Implicits._
      bCfg.server.audioBusChannels  = math.max(128, (numSignals + 1).nextPowerOfTwo)  // ScalaCollider #84
      bCfg.server.sampleRate        = config.sampleRate.toInt
      bCfg.server.wireBuffers       = 1024  // crude guess :-/
      bCfg.server.blockSize         = config.blockSize
      bCfg.group    = cursor.step { implicit tx =>
        val p = Proc[T]()
        p.graph() = graphBnc
        tx.newHandle(p) :: Nil
      }

      val pBnc: Processor[File] = bnc.run(bCfg)(Model.EmptyListener)

      val fBnc = await(pBnc, target = 0.5)

      try {
        val afBnc = blocking { AudioFile.openRead(fBnc) }
        try {
          val numChannels = afBnc.numChannels
          val numFrames   = afBnc.numFrames.toInt
          assert (afBnc.numChannels == numSignals)
          checkAborted()

          // println(f"Bounce length: ${afBnc.numFrames / afBnc.sampleRate}%1.1f sec.")
          // limit memory usage (for Pi)
          val maxSamples  = MAX_MEM_SIZE_MB * (1024 * 1024 / 4)
          val blockSize   = math.min(numFrames, math.max(8192, maxSamples / numChannels))
          val hasBlocks   = blockSize < numFrames
          val buf         = afBnc.buffer(blockSize)
          var analysisMap = Map.empty[Int, Either[Float, Int]]
          var off         = 0
          val v0a         = new Array[Float   ](numChannels)
          val chanDiff    = new Array[Boolean ](numChannels)
          while (off < numFrames) {
            val chunkSize = math.min(numFrames - off, blockSize)
            blocking { afBnc.read(buf, 0, chunkSize) }
            var ch1 = 0
            while (ch1 < numChannels) {
              if (!chanDiff(ch1)) {
                val b1 = buf(ch1)
                val v0 = if (off == 0) {
                  val v = b1(0)
                  v0a(ch1) = v.toFloat
                  v
                } else {
                  v0a(ch1)
                }
                var i = 0
                while (i < chunkSize) {
                  if (b1(i) == v0) {
                    i += 1
                  } else {
                    chanDiff(ch1) = true
                    i = chunkSize
                  }
                }
              }
              ch1 += 1
            }
            off += chunkSize
          }

          if (hasBlocks) {
            blocking { afBnc.position = 0L }
          }

          val interDiff = new Array[Boolean](numChannels * numChannels)
          off = 0
          while (off < numFrames) {
            val chunkSize = math.min(numFrames - off, blockSize)
            if (hasBlocks) blocking { afBnc.read(buf, 0, chunkSize) }
            var ch1 = 0
            while (ch1 < numChannels) {
              if (chanDiff(ch1)) {
                val b1 = buf(ch1)
                var ch2 = 0
                while (ch2 < ch1) {
                  if (chanDiff(ch2)) {
                    val cc = ch1 * numChannels + ch2
                    if (!interDiff(cc)) {
                      val b2 = buf(ch2)
                      var i = 0
                      while (i < chunkSize) {
                        if (b1(i) == b2(i)) {
                          i += 1
                        } else {
                          interDiff(cc) = true
                          i = chunkSize
                        }
                      }
                    }
                  }
                  ch2 += 1
                }
              }
              ch1 += 1
            }
            off += chunkSize
          }

          {
            var ch1 = 0
            while (ch1 < numChannels) {
              if (!chanDiff(ch1)) {
                analysisMap += ch1 -> Left(v0a(ch1))
              } else {
                // we find the _highest_ ch2 which is identical to ch1
                // (I don't think this is necessary and we could have gone
                // from low to high, but this matches an earlier implementation)
                var ch2 = ch1 - 1 // 0
                while (ch2 >= 0) { // < ch1
                  if (chanDiff(ch2)) {
                    val cc = ch1 * numChannels + ch2
                    if (!interDiff(cc)) {
                      analysisMap += ch1 -> Right(ch2)
                      chanDiff(ch1) = false // "mark as replaced" so we do not encounter it later
  //                    ch2 = ch1
                      ch2 = 0
                    }
                  }
                  ch2 -=1 // += 1
                }
              }
              ch1 += 1
            }
          }

          checkAborted()
          progress = 0.9
//          progress = 0.5 + 0.5 * ch1 / numSignals

          // _NOT_: highest indices first, so the successive indices
          // stay valid while we manipulate the topology
//          val analysis = analysisMap.toList.sortBy(-_._1)
          // without further analysis, removing from higher to
          // lower indices leaves an incomplete graph. works
          // correctly in normal order (we use the indices only
          // in static maps, so they remain valid anyway)
          val analysis    = analysisMap.toList.sortBy(_._1) // .take(21)
          val countConst  = analysis.count(_._2.isLeft  )
          val countEqual  = analysis.count(_._2.isRight )
          if (DEBUG) println(s"countConst = $countConst, countEqual = $countEqual")

//          println(s"srcMapIn.size = ${srcMapIn.size}")
//          val LAST = analysis.last._1

          val rootsIn     : Seq[Vertex.UGen]      = Util.getGraphRoots(topIn)
          var rootsOutMap : Map[Vertex.UGen, Int] = rootsIn.iterator.map(v => (v, 1)).toMap
          if (DEBUG) {
            println(s"---- ${rootsIn.size} ROOTS IN ----")
            rootsIn.foreach { v =>
              println(v)
            }
            println()
          }

          val topOut0 = analysis.foldLeft(topIn) {
            case (topTemp, (ch, eth)) =>
              val srcIdx  = idxMap(ch)
              val vOld    = srcMapIn(srcIdx)
              val vNew    = eth match {
                case Left  (value) =>
                  val _vNew = Vertex.Constant(value)
                  if (rootsOutMap.contains(vOld)) {
                    rootsOutMap -= vOld
                    rootsOutDC  += value
                  }
                  _vNew
                case Right (chTgt) =>
                  val srcIdxTgt = idxMap(chTgt)
                  val _vNew: Vertex.UGen = srcMapIn(srcIdxTgt)
                  if (rootsOutMap.contains(vOld)) {
                    val cOld = rootsOutMap(vOld) - 1
                    if (cOld == 0) rootsOutMap -= vOld else rootsOutMap += (vOld -> cOld)
                    val cNew = rootsOutMap.getOrElse(_vNew, 0) + 1
                    rootsOutMap += (_vNew -> cNew)
                  }
//                  assert (topTemp.vertices.contains(_vNew))
                  _vNew
              }
//              if (ch == LAST) {
//                println("---here")
//              }
              if (DEBUG) println(s"Replace $vOld by $vNew")
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
          if (config.expandProtect) assert (protects.isEmpty)

//          if (DEBUG) println(s"Removing ${protects.size} Protect elements now")
//          val topOut1 = protects.foldLeft(topOut0) { (topTmp1, pr) =>
//            Chromosome.removeVertex(topTmp1, pr)
//          }
          val topOut1 = topOut0

          val topOut2 = if (rootsOutDC == 0.0) topOut1 else {
            // XXX TODO: could look for existing DC in roots
            val vArg  = Vertex.Constant(rootsOutDC.toFloat)
            val vDC   = Vertex.UGen(UGens.mapAll("DC"))
            val e     = Edge(vDC, vArg, inlet = "in")
            rootsOutMap += (vDC -> 1)
            topOut1.addVertex(vArg).addVertex(vDC).addEdge(e).get._1
          }

          val (rootsSingleM, rootsMultiM) = rootsOutMap.partition(_._2 == 1)
          val rootsSingle = rootsSingleM.keys.toList
          val (topOut3: SynthGraphT, rootsOut: Seq[Vertex.UGen]) =
            if (rootsMultiM.isEmpty) (topOut2, rootsSingle) else {
              rootsMultiM.foldLeft((topOut2, rootsSingle)) { case ((topTmp, rootsTmp), (vBase0, count)) =>
                val vBase     = if (vBase0.info.name != "Protect") vBase0 else {
                  topTmp.edgeMap(vBase0).head.targetVertex
                }
                val vArg      = Vertex.Constant(count.toFloat)
                val vMul      = Vertex.UGen(UGens.map(s"Bin_${BinaryOpUGen.Times.id}"))
                val eA        = Edge(vMul, vBase, inlet = "a")
                val eB        = Edge(vMul, vArg , inlet = "b")
                var topTmp1   = topTmp  .addVertex(vArg)
                topTmp1       = topTmp1 .addVertex(vMul)
                topTmp1       = topTmp1 .addEdge(eA).get._1
                topTmp1       = topTmp1 .addEdge(eB).get._1
//                val rootsTmp1 = vMul :: rootsTmp
                val rootsTmp1 = rootsTmp :+ vMul  // nicer if they appear at the end
                (topTmp1, rootsTmp1)
              }
            }

          if (DEBUG) println(s"Removing ${protects.size} Protect elements now")
          val topOut4 = protects.foldLeft(topOut3) { (topTmp1, pr) =>
//            Chromosome.removeVertex(topTmp1, pr)
            val inSet = topTmp1.edgeMap(pr)
            assert (inSet.size == 1)
            val in = inSet.head.targetVertex
            Chromosome.replaceVertex(topTmp1, pr, in)
          }

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

          val topOut      = removeOrphans(topOut4)
          val graphOut    = MkSynthGraph(topOut, expandProtect = config.expandProtect, expandIO = config.expandIO)

          if (DEBUG) {
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