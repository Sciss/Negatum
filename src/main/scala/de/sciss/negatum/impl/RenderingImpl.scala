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
import de.sciss.filecache
import de.sciss.filecache.{TxnConsumer, TxnProducer}
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.DoubleObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Sys, TxnLike}
import de.sciss.lucre.synth.InMemory
import de.sciss.negatum.Negatum.Rendering.State
import de.sciss.negatum.Negatum.{Config, Rendering}
import de.sciss.numbers
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.span.Span
import de.sciss.strugatzki.{FeatureCorrelation, FeatureExtraction, Strugatzki}
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.synth.proc
import de.sciss.synth.proc.{AudioCue, Bounce, Folder, Proc, SoundProcesses, SynthGraphObj, TimeRef, WorkspaceHandle}
import de.sciss.synth.{SynthGraph, UGenSpec, UndefinedRate}
import de.sciss.topology.Topology

import scala.annotation.tailrec
import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException, blocking}
import scala.concurrent.stm.{Ref, TxnExecutor}
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
      val g = mkIndividual()
      pop(i) = new Individual(g)
      i += 1
    }

    val inputExtr: File = {
      val fut = calcInputSpec()
      Await.result(fut, Duration(30, TimeUnit.SECONDS))._1
    }

    val PROG_WEIGHT = 1.0 / (numIter.toLong * pop.length)

    var iter = 0
    var PROG_COUNT = 0L
    while (iter < numIter) {
      // evaluate those that haven't been
      i = 0
      while (i < pop.length) {
        val indiv = pop(i)
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
        }
        i += 1
        checkAborted()
        PROG_COUNT += 1
        progress = PROG_COUNT * PROG_WEIGHT
      }
      iter += 1
    }

    pop.toIndexedSeq
  }

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
    import config.penalty._
    import config.generation._
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
  private def mkIndividual(): SynthGraph = {
    import config.generation._
    val num = rrand(minNumVertices, maxNumVertices)
    @tailrec def loopGraph(pred: SynthGraphT): SynthGraphT =
      if (pred.vertices.size >= num) pred else loopGraph(addVertex(pred))

    val t0  = loopGraph(Topology.empty)
    val res = MkSynthGraph(t0, mono = true, removeNaNs = true, specialOut = true, ranges = true)
    res
  }

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