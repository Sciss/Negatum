/*
 *  RenderingImpl.scala
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
import de.sciss.filecache.{TxnConsumer, TxnProducer}
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.DoubleObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Sys, TxnLike}
import de.sciss.lucre.synth.InMemory
import de.sciss.negatum.Negatum.Rendering.State
import de.sciss.negatum.Negatum.{Config, Rendering}
import de.sciss.{filecache, numbers}
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.span.Span
import de.sciss.strugatzki.{FeatureCorrelation, FeatureExtraction, Strugatzki}
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.synth.proc.{AudioCue, Bounce, Folder, Proc, SynthGraphObj, TimeRef, WorkspaceHandle}
import de.sciss.synth.{SynthGraph, UGenSpec, UndefinedRate, proc}
import de.sciss.topology.Topology

import scala.annotation.{switch, tailrec}
import scala.collection.breakOut
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.duration.Duration
import scala.concurrent.stm.{Ref, TxnExecutor}
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException, blocking}
import scala.language.higherKinds
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success, Try}

object RenderingImpl {
  private final val featNorms = Array[Array[Float]](
    Array(0.006015186f,1.4569731f),
    Array(-1.4816481f,3.093808f),
    Array(-1.4089416f,1.267046f),
    Array(-0.860692f,1.4034394f),
    Array(-0.65952975f,1.431201f),
    Array(-0.66072506f,0.8506244f),
    Array(-0.2808966f,0.90672106f),
    Array(-0.29912513f,0.705802f),
    Array(-0.22443223f,0.67802113f),
    Array(-0.1471797f,0.68207365f),
    Array(-0.104354106f,0.6723507f),
    Array(-0.2412649f,0.70821077f),
    Array(-0.16983563f,0.6771785f),
    Array(-0.10048226f,0.64655834f)
  )

  private val DEBUG = false

  final case class FeatureExtractionFailed(cause: Throwable) extends Exception(cause)

  private object CacheValue {
    implicit object serializer extends ImmutableSerializer[CacheValue] {
      def write(v: CacheValue, out: DataOutput): Unit = {
        out.writeLong(v.lastModified)
        out.writeUTF (v.meta   .getCanonicalPath)
        out.writeUTF (v.feature.getCanonicalPath)
      }

      def read(in: DataInput): CacheValue = {
        val mod     = in.readLong()
        val meta    = file(in.readUTF())
        val feature = file(in.readUTF())
        CacheValue(lastModified = mod, meta = meta, feature = feature)
      }
    }
  }

  private type CacheKey = (File, Int)
  private case class CacheValue(lastModified: Long, meta: File, feature: File)

  private val cCfg  = {
    val c = filecache.Config[CacheKey, CacheValue]()
    // c.executionContext = SoundProcesses.executionContext
    c.capacity  = filecache.Limit(count = 10)
    c.accept    = { (key, value) => key._1.lastModified() == value.lastModified }
    c.space     = { (key, value) => value.meta.length() + value.feature.length() }
    c.evict     = { (key, value) => value.meta.delete() ; value.feature.delete() }
    c.build
  }
  private val cacheP = TxnExecutor.defaultAtomic { implicit tx => TxnProducer(cCfg) }
  private val cache  = TxnConsumer(cacheP)(mkCacheValue)

  private def mkCacheValue(key: (File, Int)): Future[CacheValue] = {
    val (f, numCoeffs) = key
    val inputSpec         = AudioFile.readSpec(f)
    val inputMod          = f.lastModified()
    require(inputSpec.numChannels == 1, s"Input file '${f.name}' must be mono but has ${inputSpec.numChannels} channels")
    val exCfg             = FeatureExtraction.Config()
    exCfg.audioInput      = f
    val inputFeature      = File.createTemp(suffix = ".aif")
    exCfg.featureOutput   = inputFeature
    val inputExtr         = File.createTemp(suffix = "_feat.xml")
    exCfg.metaOutput      = Some(inputExtr)
    exCfg.numCoeffs       = numCoeffs
    val futInputExtr      = FeatureExtraction(exCfg)
    import cacheP.executionContext
    futInputExtr.start()
    futInputExtr.map { _ =>
      CacheValue(lastModified = inputMod, meta = inputExtr, feature = inputFeature)
    }
  }
}
final class RenderingImpl[S <: Sys[S]](config: Config, template: AudioCue,
                                       popIn: Vec[Individual], populationH: stm.Source[S#Tx, Folder[S]], numIter: Int)
                                      (implicit cursor: stm.Cursor[S])
  extends Rendering[S]
    with ObservableImpl[S, Rendering.State]
    with ProcessorImpl[Vec[Individual], Rendering[S]] {

  import RenderingImpl._

  private[this] val _state        = Ref[Rendering.State](Rendering.Progress(0.0))
  private[this] val _disposed     = Ref(false)
  private[this] val random        = new Random()  // XXX TODO --- make seed customisable

  protected def body(): Vec[Individual] = blocking {
    import config._
    import generation._
    val pop = new Array[Individual](population)
    var i = 0
    while (i < popIn.size) {
      pop(i) = popIn(i)
      i += 1
    }
    while (i < population) {
      val indiv = mkIndividual()
      pop(i) = indiv
      i += 1
    }

    val inputExtr: File = {
      val fut = calcInputSpec()
      Await.result(fut, Duration(30, TimeUnit.SECONDS))._1
    }

    val PROG_WEIGHT = 1.0 / (numIter.toLong * (pop.length * 2))

    var iter = 0
    var PROG_COUNT = 0L
    while (iter < numIter) {

      def evalPop(): Unit = {
        var ii = 0
        while (ii < pop.length) {
          val indiv = pop(ii)
          if (indiv.fitness.isNaN) {
            val numVertices = indiv.graph.sources.size  // XXX TODO -- ok?
            val fut = evaluateFut(graph = indiv.graph, inputSpec = template.spec,
              inputExtr = inputExtr, numVertices = numVertices)
            // XXX TODO --- Mutagen used four parallel processes; should we do the same?
            val sim = try {
              Await.result(fut, Duration(30, TimeUnit.SECONDS))
            } catch {
              case NonFatal(_) => 0.0f
            }
            indiv.fitness = sim
            checkAborted()
            PROG_COUNT += 1
            progress = PROG_COUNT * PROG_WEIGHT
          }
          ii += 1
        }
      }

      // evaluate those that haven't been
      evalPop()

      val el    = elitism(pop)
      val _sel0 = select (pop)
      val sel   = scramble(_sel0.toIndexedSeq)

      import config.breeding._

      val numGolem1 = math.min(numGolem, pop.length - el.size)
      val nGen      = pop.length - el.size - numGolem1
      val nMut      = (mutProb * nGen + 0.5).toInt
      val nCross    = nGen - nMut

      val mut       = mutate    (sel, nMut)
      val cross     = crossover (sel, nCross)

      val golem     = Vector.fill(numGolem1)(mkIndividual())

      // genome.chromosomes() = el ++ (mut ++ cross).map(_.apply()) ++ golem
      i = 0
      el.foreach { indiv =>
        pop(i) = indiv
        i += 1
      }
      mut.foreach { indiv =>
        pop(i) = indiv
        i += 1
      }
      cross.foreach { indiv =>
        pop(i) = indiv
        i += 1
      }
      golem.foreach { indiv =>
        pop(i) = indiv
        i += 1
      }

      evalPop()

      iter += 1
    }

    pop.toIndexedSeq
  }

  /* Produces a sequence of `n` items by mutating the input `sel` selection. */
  private def mutate(sq: Vec[Individual], n: Int): Vec[Individual] = {
    var res = Vector.empty[Individual]

    while (res.size < n) {
      val chosen = sq(res.size % sq.size)
      val hOpt: Option[Individual] = {
        val ok = tryMutate(chosen.graph)
        if (ok ne chosen.graph) {
          Some(new Individual(ok))
        } else None
      }
      hOpt.foreach { h => res :+= h }
    }
    res
  }

  private def tryMutate(chosen: SynthGraph): SynthGraph = {
    import config.breeding.{mutMin, mutMax}

    val chosenT       = MkTopology(chosen)
    val mutationIter  = rrand(mutMin, mutMax)
    require(mutationIter > 0)
    val res = (chosenT /: (1 to mutationIter)) { case (pred, iter) =>
      val tpe = random.nextInt(2 /* 7 */)
      val next: SynthGraphT = (tpe: @switch) match {
        case 0 => addVertex   (pred)
        case 1 => removeVertex(pred)
        case 2 => ??? // changeVertex(pred)
        case 3 => ??? // changeEdge  (pred)
        case 4 => ??? // swapEdge    (pred)
        case 5 => ??? // splitVertex (pred)
        case 6 => ??? // mergeVertex (pred)
      }

//      if (next ne pred) {
//        validate1(next)
//      }

      next
    }

    if (res ne chosenT) MkSynthGraph(res) else chosen
  }

  /* Runs the selection stage of the algorithm, using `all` inputs which
   * are chromosomes paired with their fitness values.
   */
  private def select(all: Array[Individual]): Set[Individual] = {
    import config.breeding.selectionFrac
    val pop   = all.length
    val n     = (pop * selectionFrac + 0.5).toInt

    @tailrec def loop(rem: Int, in: Set[Individual], out: Set[Individual]): Set[Individual] =
      if (rem == 0) out else {
        val sum     = in.iterator.map(_.fitness).sum
        val rem1    = rem - 1
        if (sum == 0.0) {
          val chosen = in.head
          loop(rem1, in - chosen, out + chosen)
        } else {
          val inIdx       = in.zipWithIndex[Individual, Array[(Individual, Int)]](breakOut)
          val norm        = inIdx.map {
            case (indiv, j) => (j, indiv.fitness / sum)
          }
          val sorted      = norm.sortBy(_._2)
          val acc         = sorted.scanLeft(0.0) { case (a, (_, f)) => a + f } .tail
          val roulette    = random.nextDouble()
          val idxS        = acc.indexWhere(_ > roulette)
          val idx         = if (idxS >= 0) sorted(idxS)._1 else in.size - 1
          val (chosen, _) = inIdx(idx)
          loop(rem1, in - chosen, out + chosen)
        }
      }

    val sel = loop(n, all.toSet.filterNot { indiv => indiv.fitness.isInfinity|| indiv.fitness.isNaN }, Set.empty)
    // val remove  = all -- sel
    // remove.foreach(prev.remove)
    sel
  }

  /* Selects the best matching chromosomes. */
  private def elitism(all: Array[Individual]): Vec[Individual] = {
    import config.breeding.numElitism
    if (numElitism == 0) Vector.empty else {
      // ensure that elite choices are distinct (don't want to accumulate five identical chromosomes over time)!
      val eliteCandidates = all.sortBy(-_.fitness)
      val res = Vector.newBuilder[Individual]
      res.sizeHint(numElitism)
      val it = eliteCandidates.iterator
      var sz = 0
      var fl = Double.NaN
      while (sz < numElitism && it.hasNext) {
        val indiv = it.next()
        val f     = indiv.fitness
        if (f != fl) {
          res += indiv
          sz  += 1
          fl   = f
        }
      }
      res.result()
    }
  }

  /* Produces a sequence of `n` items by crossing each two parents from the input `sel` selection. */
  private def crossover(sq: Vec[Individual], n: Int): Vec[Individual] = {
    var res = Vector.empty[Individual]
    while (res.size < n) {
      val idx0      = res.size << 1
      val chosen1   = sq( idx0      % sq.size)
      val chosen2   = sq((idx0 + 1) % sq.size)
      val chosen1T  = MkTopology(chosen1.graph)
      val chosen2T  = MkTopology(chosen2.graph)

      val (x1, x2) = performCrossover(top1 = chosen1T, top2 = chosen2T)

      val hs = if (res.size + 1 == n) x1 :: Nil else {
        x1 :: x2 :: Nil
      }
      hs.foreach { h => res :+= new Individual(MkSynthGraph(h)) }
    }
    res
  }

  private def performCrossover(top1: SynthGraphT, top2: SynthGraphT): (SynthGraphT, SynthGraphT) = {
    import config.generation.maxNumVertices
    val v1      = top1.vertices
    val v2      = top2.vertices

    val (pos1, pos2) = if (coin(0.8)) {   // XXX TODO -- make that a parameter
    val posRel  = random.nextFloat()
      val _pos1   = (posRel * v1.size - 1).toInt + 1
      val _pos2   = (posRel * v2.size - 1).toInt + 1
      (_pos1, _pos2)
    } else {
      val posRel1 = random.nextFloat()
      val _pos1   = (posRel1 * v1.size - 1).toInt + 1
      val posRel2 = random.nextFloat()
      val _pos2   = (posRel2 * v2.size - 1).toInt + 1
      (_pos1, _pos2)
    }

    val (head1, tail1)  = v1.splitAt(pos1)
    val (head2, tail2)  = v2.splitAt(pos2)
    val edgesHead1      = top1.edges.filter(e => head1.contains(e.sourceVertex) && head1.contains(e.targetVertex))
    val edgesTail1      = top1.edges.filter(e => tail1.contains(e.sourceVertex) && tail1.contains(e.targetVertex))
    val edgesHead2      = top2.edges.filter(e => head2.contains(e.sourceVertex) && head2.contains(e.targetVertex))
    val edgesTail2      = top2.edges.filter(e => tail2.contains(e.sourceVertex) && tail2.contains(e.targetVertex))

    val severedHeads1   = top1.edges.collect {
      case Edge(source: Vertex.UGen, target, _) if head1.contains(source) && tail1.contains(target) => source
    }
    val severedHeads2   = top2.edges.collect {
      case Edge(source: Vertex.UGen, target, _) if head2.contains(source) && tail2.contains(target) => source
    }

    @tailrec def shrinkTop(top: SynthGraphT, target: Int, iter: Int): SynthGraphT =
      if (top.vertices.size <= target || iter == maxNumVertices) top else {
        val (top1, _) = removeVertex1(top)
        shrinkTop(top1, target = target, iter = iter + 1)
      }

    def mkTop(vertices1: Vec[Vertex], edges1: Set[Edge], vertices2: Vec[Vertex], edges2: Set[Edge]): SynthGraphT = {
      val t1a = (Topology.empty[Vertex, Edge] /: vertices1)(_ addVertex _)
      val t1b = (t1a /: edges1)(_.addEdge(_).get._1)  // this is now the first half of the original top

      val (t2a, e2cpy) = ((t1b, edges2) /: vertices2) { case ((t0, e0), v0) =>
        // two parents might share the same vertices from a common
        // ancestry; in that case we must individualize the vertex
        // (making a copy means they get fresh object identity and hash)
        val isNew = !vertices1.contains(v0)
        val v     = if (isNew) v0 else v0.copy()
        val tRes  = t0.addVertex(v)
        val eRes  = if (isNew) e0 else e0.map { e =>
          if      (e.sourceVertex == v0) e.copy(sourceVertex = v)
          else if (e.targetVertex == v0) e.copy(targetVertex = v)
          else e
        }
        (tRes, eRes)
      }
      (t2a /: e2cpy) { (t0, e0) =>
        val res = t0.addEdge(e0)
        if (res.isFailure) {
          println("WARNING: Cross-over mkTop - cycle detected!")
        }
        res.toOption.fold(t0)(_._1)
      }
    }

    val topC1a = mkTop(head1, edgesHead1, tail2, edgesTail2)
    val topC2a = mkTop(head2, edgesHead2, tail1, edgesTail1)

    def complete(top: SynthGraphT, inc: Set[Vertex.UGen]): SynthGraphT = {
      val top1 = if (inc.isEmpty) top else (top /: inc)((res, v) => completeUGenInputs(res, v))
      val top2 = shrinkTop(top1, top.vertices.size, 0)
      top2
    }

    val topC1 = complete(topC1a, severedHeads1)
    val topC2 = complete(topC2a, severedHeads2)

    if (DEBUG) {
      val s1 = s"p1 = (${v1.size}, ${top1.edges.size}), p2 = (${v2.size}, ${top2.edges.size})"
      val s2 = s"c1 = (${topC1.vertices.size}, ${topC1.edges.size}), c2 = (${topC2.vertices.size}, ${topC2.edges.size})"
      println(s"crossover. $s1. $s2")
    }

    (topC1, topC2)
  }

  // ---- evaluation ----

  private def calcInputSpec(): Future[(File, AudioFileSpec)] = {
    import config.evaluation._
    val input     = template.artifact
    val key       = input -> numMFCC
    val futMeta   = TxnExecutor.defaultAtomic { implicit tx =>
      cache.acquire(key)
    }
    val res       = futMeta.map { v =>
      val inputExtr = v.meta
      val inputSpec = blocking(AudioFile.readSpec(input))
      TxnExecutor.defaultAtomic { implicit tx =>
        cache.release(key)
      }
      (inputExtr, inputSpec)
    }
    // res.onComplete(_ => TxnExecutor.defaultAtomic { implicit tx => cache.release(key) })
    res
  }

  private def evaluateFut(graph: SynthGraph, inputSpec: AudioFileSpec,
                          inputExtr: File, numVertices: Int): Future[Float] = {
    import config.generation._
    import config.penalty._
    val audioF  = File.createTemp(prefix = "muta_bnc", suffix = ".aif")
    val bnc0    = bounce(graph, audioF = audioF, inputSpec = inputSpec)
    val simFut  = eval1(wait = Some(bnc0), bounceF = audioF, inputSpec = inputSpec, inputExtr = inputExtr)
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

  private[this] val inMemory = InMemory()

  /* Bounces a synth def to an audio file.
   *
   * @param graph       the synth graph to play and evaluate
   * @param audioF      the audio output file to bounce to
   * @param inputSpec   the spec of the original target sound
   * @param duration0   the duration to bounce in seconds or `-1` to bounce the duration of the target sound
   */
  private def bounce(graph: SynthGraph, audioF: File, inputSpec: AudioFileSpec, duration0: Double = -1): Processor[Any] = {
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
    bnc0.start()
    // }
    bnc0
  }

  // XXX TODO --- replace Strugatzki by FScape
  private def eval1(wait: Option[Processor[Any]], bounceF: File, inputSpec: AudioFileSpec,
                    inputExtr: File): Future[Double] = {
    import config.evaluation.{wait => _, _}

    val bnc = Future {
      wait.foreach { bnc0 =>
        Await.result(bnc0, Duration(4.0, TimeUnit.SECONDS))
      }
      // XXX TODO -- would be faster if we could use a Poll during
      // the bounce and instruct the bounce proc to immediately terminate
      // when seeing a particular message in the console?
      blocking {
        val af = AudioFile.openRead(bounceF)
        try {
          val bufSize = 512
          val b       = af.buffer(bufSize)
          var i       = 0L
          while (i < af.numFrames) {
            val len = math.min(bufSize, af.numFrames - i).toInt
            af.read(b, 0, len)
            var ch = 0
            while (ch < af.numChannels) {
              val bc = b(ch)
              var j = 0
              while (j < len) {
                if (bc(j).isNaN || bc(j).isInfinite) {
                  if (DEBUG) println("Detected NaNs")
                  throw FeatureExtractionFailed(null)
                }
                j += 1
              }
              ch += 1
            }
            i += len
          }
        } finally {
          af.cleanUp()
        }
      }
    }

    val genFolder           = File.createTemp(prefix = "muta_eval", directory = true)
    val genExtr             = genFolder / "gen_feat.xml"

    val normF   = genFolder / Strugatzki.NormalizeName
    if (normalizeMFCC) {
      if (numMFCC != featNorms.length + 1)
        throw new IllegalArgumentException(s"Normalize option requires numCoeffs == ${featNorms.length - 1}")
      blocking {
        val normAF  = AudioFile.openWrite(normF, AudioFileSpec(numChannels = featNorms.length, sampleRate = 44100))
        normAF.write(featNorms)
        normAF.close()
      }
    }
    val featF   = File.createTemp(prefix = "gen_feat", suffix = ".aif")

    val ex = bnc.flatMap { _ =>
      val exCfg             = FeatureExtraction.Config()
      exCfg.audioInput      = bounceF
      exCfg.featureOutput   = featF
      exCfg.metaOutput      = Some(genExtr)
      exCfg.numCoeffs       = numMFCC
      val _ex               = FeatureExtraction(exCfg)
      _ex.start()
      //      _ex.onFailure {
      //        case t => println(s"gen-extr failed with $t")
      //      }
      _ex.recover {
        case cause => throw FeatureExtractionFailed(cause)
      }
    }

    val numFrames = inputSpec.numFrames

    val corr = ex.flatMap { _ =>
      val corrCfg           = FeatureCorrelation.Config()
      corrCfg.metaInput     = inputExtr
      corrCfg.databaseFolder= genFolder
      corrCfg.minSpacing    = Long.MaxValue >> 1
      corrCfg.numMatches    = 1
      corrCfg.numPerFile    = 1
      corrCfg.maxBoost      = maxBoost.toFloat
      corrCfg.normalize     = normalizeMFCC
      corrCfg.minPunch      = numFrames
      corrCfg.maxPunch      = numFrames
      corrCfg.punchIn       = FeatureCorrelation.Punch(
        span = Span(0L, numFrames),
        temporalWeight = temporalWeight.toFloat)
      val _corr             = FeatureCorrelation(corrCfg)
      _corr.start()
      _corr
    }

    val simFut0 = corr.map { matches =>
      // assert(matches.size == 1)
      val sim0 = matches.headOption.map { m =>
        if (DEBUG) println(m)
        m.sim
      } .getOrElse(0f)
      val sim  = if (sim0.isNaN || sim0.isInfinite) 0.0 else sim0.toDouble
      sim
    }

    val simFut = simFut0.recover {
      case Bounce.ServerFailed(_) => 0.0
      case FeatureExtractionFailed(_) =>
        if (DEBUG) println("Gen-extr failed!")
        0.0

      case _: TimeoutException =>
        if (DEBUG) println("Bounce timeout!")
        wait.foreach { bnc0 => bnc0.abort() }
        0.0    // we aborted the process after 4 seconds
    }

    val res = simFut

    res.onComplete { _ =>
      if (normalizeMFCC) normF.delete()
      featF.delete()
      // audioF    .delete()
      genExtr.delete()
      genFolder.delete()
    }
    res
  }

  // ---- utility ----

  private def scramble[A, CC[~] <: IndexedSeq[~], To](in: CC[A])(implicit cbf: CanBuildFrom[CC[A], A, To]): To = {
    val b = cbf(in)
    var rem = in: IndexedSeq[A]
    while (rem.nonEmpty) {
      val idx = random.nextInt(rem.size)
      val e = rem(idx)
      rem = rem.patch(idx, Nil, 1)
      b += e
    }
    b.result()
  }

  @inline
  private[this] def rrand(lo: Int, hi: Int): Int = lo + random.nextInt(hi - lo + 1)

  @inline
  private[this] def exprand(lo: Double, hi: Double): Double =
    lo * math.exp(math.log(hi / lo) * random.nextDouble())

  @inline
  private[this] def coin(p: Double = 0.5): Boolean = random.nextDouble() < p

  @inline
  private[this] def choose[A](xs: ISeq[A]): A = xs.toIndexedSeq(random.nextInt(xs.size))

  /* Creates an individual chromosome. */
  private def mkIndividual(): Individual = {
    import config.generation._
    val num = rrand(minNumVertices, maxNumVertices)
    @tailrec def loopGraph(pred: SynthGraphT): SynthGraphT =
      if (pred.vertices.size >= num) pred else loopGraph(addVertex(pred))

    val t0  = loopGraph(Topology.empty)
    val g   = MkSynthGraph(t0, mono = true, removeNaNs = true, specialOut = true, ranges = true)
    new Individual(g)
  }

  // ---- mutations ----

  private def addVertex(pred: SynthGraphT): SynthGraphT = {
    import config.generation.constProb
    val next: SynthGraphT = if (coin(constProb)) {
      val v = mkConstant()
      pred.addVertex(v)

    } else {
      val v   = mkUGen()
      val t1  = pred.addVertex(v)
      completeUGenInputs(t1, v)
    }
    next
  }

  private def removeVertex(c: SynthGraphT): SynthGraphT = {
    import config.generation._
    val vertices    = c.vertices
    val numVertices = vertices.size
    if (numVertices <= minNumVertices) c else {
      val (res, _) = removeVertex1(c)
      checkComplete(res, s"removeVertex($c)")
//      stats(1) += 1
      res
    }
  }

  def removeVertex1(top: SynthGraphT): (SynthGraphT, Vertex) = {
    val vertices    = top.vertices
    val numVertices = vertices.size
    val idx         = random.nextInt(numVertices)
    val v           = vertices(idx)
    val targets     = getArgUsages(top, v)
    val top1        = top.removeVertex(v)
    val top3        = (top1 /: targets) { (top2, e) =>
      val x = top2.removeEdge(e)
      assert(x ne top2)
      x
    }
    val succ = (top3 /: targets) { case (top4, Edge(t: Vertex.UGen, _, _)) =>
      completeUGenInputs(top4, t)
    }
    (succ, v)
  }

//  private def changeVertex[S <: Sys[S]](config: Algorithm.Config, top: Chromosome[S])
//                                       (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Boolean = {
//    val vertices    = top.vertices
//    val numVertices = vertices.size
//
//    val idx     = random.nextInt(numVertices)
//    val vOld    = vertices(idx)
//    vOld match {
//      case f: Vertex.Constant[S] => changeVertexConstant(        top, f)
//      case u: Vertex.UGen[S]     => changeVertexUGen    (config, top, u)
//    }
//    stats(2) += 1
//
//    true
//  }
//
//  private def changeVertexConstant[S <: Sys[S]](top: Chromosome[S], vc: Vertex.Constant[S])
//                                               (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Unit = {
//    val fNew: Float = if (Util.coin())
//      ChromosomeImpl.mkConstantValue()            // completely random
//    else
//      vc.f * Util.exprand(0.9, 1.0/0.9).toFloat  // gradual change
//
//    vc.f = fNew
//  }
//
//  private def changeVertexUGen[S <: Sys[S]](config: Algorithm.Config, top: Chromosome[S], vu: Vertex.UGen[S])
//                                           (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Unit = {
//    val outlet  = getTargets(top, vu)
//    val inlets  = top.targets(vu) // .getOrElse(Set.empty)
//    outlet.foreach(top.removeEdge)
//    inlets.foreach(top.removeEdge)
//    top.removeVertex(vu)
//
//    val vNew = ChromosomeImpl.mkUGen()
//
//    val oldInletNames: Vec[String] = vu match {
//      case Vertex.UGen(info) => /* ChromosomeImpl.geArgs(info).map(_.name) */ info.inputs.map(_.arg)
//      case _ => Vec.empty
//    }
//
//    top.addVertex(vNew)
//    outlet.map(_.copy1(targetVertex = vNew)).foreach(top.addEdge /* .get */)
//
//    // just as many as possible, leaving tail inlets empty
//    val newInlets = vNew match {
//      case vNewU: Vertex.UGen[S] if oldInletNames.nonEmpty =>
//        val newInletNames = vNewU.info.inputs.map(_.arg)
//        inlets.collect {
//          case e if oldInletNames.indexOf(e.inlet) < newInletNames.size =>
//            e.copy1(sourceVertex = vNewU, inletIndex = oldInletNames.indexOf(e.inlet))
//        }
//      case _ => Vec.empty
//    }
//
//    newInlets.foreach(top.addEdge /* .get */)
//    vNew match {
//      case vu: Vertex.UGen[S] => ChromosomeImpl.completeUGenInputs(config, top, vu)
//      case _ =>
//    }
//  }
//
//  private def changeEdge[S <: Sys[S]](config: Algorithm.Config, top: Chromosome[S])
//                                     (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Boolean = {
//    val vertices    = top.vertices
//
//    val candidates  = vertices.iterator.collect {
//      case v: Vertex.UGen[S] if ChromosomeImpl.geArgs(v.info).nonEmpty /* spec.inputs.nonEmpty */ => v
//    } .toIndexedSeq
//
//    if (candidates.isEmpty) false else {
//      val v     = Util.choose(candidates)
//      val edges = top.targets(v) // edgeMap.get(v).getOrElse(Set.empty)
//      if (edges.isEmpty) false else {
//        top.removeEdge(Util.choose(edges))
//        ChromosomeImpl.completeUGenInputs[S](config, top, v)
//        stats(3) += 1
//        true
//      }
//    }
//  }
//
//  private def swapEdge[S <: Sys[S]](top: Chromosome[S])(implicit tx: S#Tx, random: TxnRandom[S#Tx]): Boolean = {
//    val vertices    = top.vertices
//
//    val candidates  = vertices.iterator.collect {
//      case v: Vertex.UGen[S] if top.targets(v).size >= 2 /* edgeMap.get(v).exists(_.size >= 2) */ => v
//    } .toIndexedSeq
//
//    if (candidates.isEmpty) false else {
//      val v     = Util.choose(candidates)
//      val edges = top.targets(v) // edgeMap.get(v).getOrElse(Set.empty)
//      val e1    = Util.choose(edges)
//      val e2    = Util.choose(edges - e1)
//      top.removeEdge(e1)
//      top.removeEdge(e2)
//      val e1New = e1.copy1(targetVertex = e2.targetVertex)
//      val e2New = e2.copy1(targetVertex = e1.targetVertex)
//      top.addEdge(e1New) // .get
//      top.addEdge(e2New) // .get
//      stats(4) += 1
//      true
//    }
//  }

  // ---- fix ups ----

  private[this] val CHECK = false

  private def checkComplete(succ: SynthGraphT, message: => String): Unit =
    if (CHECK) succ.vertices.foreach {
      case v: Vertex.UGen =>
        val inc = findIncompleteUGenInputs(succ, v)
        if (inc.nonEmpty) {
          println("MISSING SLOTS:")
          inc.foreach(println)
          sys.error(s"UGen is not complete: $v - $message")
        }
      case _ =>
    }

//  private def validate1(top: SynthGraphT): Boolean = {
//    val errors = top.validate()
//    if (errors.nonEmpty) {
//      println(s"===== WARNING: found ${errors.size} errors =====")
//      errors.foreach(println)
//      println("\nChromosome:")
//      println(debugString)
//    }
//    errors.isEmpty
//  }
//
//  private def validate(top: SynthGraphT): Vec[String] = {
//    val u = top.unconnected
//    val b = Vector.newBuilder[String]
//    if (u < 0 || u > top.vertices.size) b += s"Illegal number of unconnected vertices: $u"
//    top.vertices.iterator.zipWithIndex.foreach { case (v, idx) =>
//      val key = v.hashCode()
//      val hasEdges = sourceEdgeMap.get(key).exists(_.get(v).exists(_.nonEmpty)) ||
//        targetEdgeMap.get(key).exists(_.get(v).exists(_.nonEmpty))
//      if (idx  < u &&  hasEdges) b += s"Vertex $v has edges although it is marked unconnected"
//      if (idx >= u && !hasEdges) b += s"Vertex $v has no edges although it is marked connected"
//    }
//    top.edges.iterator.foreach { e =>
//      val s1 = sourceEdgeMap.get(e.sourceVertex.hashCode()).getOrElse(Map.empty).getOrElse(e.sourceVertex, Set.empty)
//      if (!s1.contains(e)) b += s"Edge $e is not found in sourceEdgeMap"
//      val s2 = targetEdgeMap.get(e.targetVertex.hashCode()).getOrElse(Map.empty).getOrElse(e.targetVertex, Set.empty)
//      if (!s2.contains(e)) b += s"Edge $e is not found in targetEdgeMap"
//    }
//    val numEdges1 = top.edges.size
//    val numEdges2 = sourceEdgeMap.iterator.map { case (_, map) => map.map(_._2.size).sum } .sum
//    val numEdges3 = targetEdgeMap.iterator.map { case (_, map) => map.map(_._2.size).sum } .sum
//    if (numEdges1 != numEdges2) b += s"Edge list has size $numEdges1, while sourceEdgeMap contains $numEdges2 entries"
//    if (numEdges1 != numEdges3) b += s"Edge list has size $numEdges1, while targetEdgeMap contains $numEdges3 entries"
//    b.result()
//  }

  private def completeUGenInputs(t1: SynthGraphT, v: Vertex.UGen): SynthGraphT = {
    import config.generation.nonDefaultProb

    val spec    = v.info
    // An edge's source is the consuming UGen, i.e. the one whose inlet is occupied!
    // A topology's edgeMap uses source-vertices as keys. Therefore, we can see
    // if the an argument is connected by getting the edges for the ugen and finding
    // an edge that uses the inlet name.
    val edgeSet = t1.edgeMap.getOrElse(v, Set.empty)
    val argsFree = geArgs(spec).filter { arg => !edgeSet.exists(_.inlet == arg.name) }
    val (hasDef, hasNoDef)          = argsFree.partition(_.defaults.contains(UndefinedRate))
    val (useNotDef, _ /* useDef */) = hasDef.partition(_ => coin(nonDefaultProb))
    val findDef = hasNoDef ++ useNotDef

    @tailrec def loopVertex(rem: Vec[UGenSpec.Argument], pred: SynthGraphT): SynthGraphT = rem match {
      case head +: tail =>
        val options = pred.vertices.filter { vi =>
          val e = Edge(v, vi, head.name)
          pred.canAddEdge(e)
        }
        val next = if (options.nonEmpty) {
          val vi  = choose(options)
          val e   = Edge(v, vi, head.name)
          pred.addEdge(e).get._1
        } else {
          val vi  = mkConstant()
          val n0  = pred.addVertex(vi)
          val e   = Edge(v, vi, head.name)
          n0.addEdge(e).get._1
        }

        loopVertex(tail, next)

      case _ => pred
    }

    loopVertex(findDef, t1)
  }

  private def getArgUsages(top: SynthGraphT, arg: Vertex): Set[Edge] =
    top.edges.filter(_.targetVertex == arg)

  // ---- creation ----

  private def mkUGen(): Vertex.UGen = {
    val spec = choose(UGens.seq)
    Vertex.UGen(spec)
  }

  private def mkConstant(): Vertex.Constant = Vertex.Constant(mkConstantValue())

  private def mkConstantValue(): Float = {
    val f0  = exprand(0.001, 10000.001) - 0.001
    val f   = if (coin(0.25)) -f0 else f0
    f.toFloat
  }

  // ---- observable ----

  def reactNow(fun: (S#Tx) => (State) => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
    val res = react(fun)
    fun(tx)(state)
    res
  }

  private def progressTx(amt: Double): Unit = if (!_disposed.single.get)
    cursor.step { implicit tx =>
      state = Negatum.Rendering.Progress(amt)
    }

  private def completeWith(t: Try[Vec[Individual]]): Unit = if (!_disposed.single.get)
    cursor.step { implicit tx =>
      import TxnLike.peer
      if (!_disposed()) t match {
        case Success(popOut) =>
          val folder = populationH()
          folder.clear()  // XXX TODO --- re-use existing procs?
          popOut.foreach { indiv =>
            val gObj  = SynthGraphObj.newConst[S](indiv.graph)
            val p     = Proc[S]
            import proc.Implicits._
            val attr  = p.attr
            p.name    = s"negatum-${indiv.graph.hashCode().toHexString}"
            p.graph() = gObj
            if (!indiv.fitness.isNaN) {
              attr.put(Negatum.attrFitness, DoubleObj.newConst[S](indiv.fitness))
            }
            // XXX TODO --- should we store fitness or not?
            folder.addLast(p)
          }
          state = Rendering.Success
        case Failure(ex) =>
          state = Rendering.Failure(ex)
      }
    }

  def startTx()(implicit tx: S#Tx, workspace: WorkspaceHandle[S]): Unit = {
    tx.afterCommit {
      addListener {
        case Processor.Progress(_, d)   => progressTx(d)
        case Processor.Result(_, value) => completeWith(value)
      }
      // NB: bad design in `ProcessorImpl`; because we're in the sub-class,
      // we have implicit execution context in scope, but that's the one
      // we want to _set_ here.
      start()(ExecutionContext.Implicits.global /* SoundProcesses.executionContext */)
//      this.andThen {
//        case x => completeWith(x)
//      }
    }
  }

  def state(implicit tx: S#Tx): State = {
    import TxnLike.peer
    _state()
  }

  protected def state_=(value: Rendering.State)(implicit tx: S#Tx): Unit = {
    import TxnLike.peer
    val old = _state.swap(value)
    if (old != value) fire(value)
  }

  def cancel()(implicit tx: S#Tx): Unit =
    tx.afterCommit(abort())

  def dispose()(implicit tx: S#Tx): Unit = {
    cancel()
  }
}