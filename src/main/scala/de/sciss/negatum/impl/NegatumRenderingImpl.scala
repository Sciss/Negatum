/*
 *  NegatumRenderingImpl.scala
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

import java.io.FileOutputStream
import java.util.concurrent.{TimeoutException, TimeUnit}

import de.sciss.file._
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.DoubleObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Sys, TxnLike}
import de.sciss.negatum.Rendering.State
import de.sciss.negatum.Negatum.Config
import de.sciss.negatum.impl.Util._
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc
import de.sciss.synth.proc.{AudioCue, Folder, Proc, SynthGraphObj, WorkspaceHandle}
import de.sciss.synth.proc.impl.MkSynthGraphSource

import scala.concurrent.duration.Duration
import scala.concurrent.stm.Ref
import scala.concurrent.{Await, ExecutionContext, blocking}
import scala.language.higherKinds
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success, Try}

object NegatumRenderingImpl {
  /** DEBUGGING*/
  var instance: NegatumRenderingImpl[_] = null
}
final class NegatumRenderingImpl[S <: Sys[S]](config: Config, template: AudioCue,
                                              popIn: Vec[Individual], populationH: stm.Source[S#Tx, Folder[S]], numIter: Int)
                                             (implicit cursor: stm.Cursor[S])
  extends Rendering[S, Unit]
    with ObservableImpl[S, Rendering.State[Unit]]
    with ProcessorImpl[Vec[Individual], Rendering[S, Unit]] {

  NegatumRenderingImpl.instance = this

  private[this] val STORE_BAD_DEFS = false

  private[this] val _state        = Ref[State[Unit]](Rendering.Progress(0.0))
  private[this] val _disposed     = Ref(false)

  @volatile
  private[this] var _shouldStop   = false

  private[this] implicit val random = new Random(config.seed)

  var DEBUG_ARRAY: Array[Individual] = null

  protected def body(): Vec[Individual] = blocking {
    import config._
    import gen._
    val pop = new Array[Individual](population)
    DEBUG_ARRAY = pop

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
      val fut = Features.extract(template.artifact, config.eval.numMFCC)
      Await.result(fut, Duration(30, TimeUnit.SECONDS))._1
    }

    val INIT_COUNT  = pop.count(_.fitness.isNaN)
    val PROG_WEIGHT = 1.0 / (numIter.toLong * pop.length + INIT_COUNT)

    var iter = 0
    var PROG_COUNT = 0L

    def evalPop(): Unit = {
      var ii = 0
      while (ii < pop.length) {
        val indiv = pop(ii)
        if (indiv.fitness.isNaN) {
          val graph       = indiv.graph
          val numVertices = graph.sources.size  // XXX TODO -- ok?
          val fut = Evaluation(config, graph = graph, inputSpec = template.spec,
            inputExtr = inputExtr, numVertices = numVertices)
          // XXX TODO --- Mutagen used four parallel processes; should we do the same?
          val sim = try {
            Await.result(fut, Duration(30, TimeUnit.SECONDS))
          } catch {
            case NonFatal(ex) =>
              val message = if (ex.isInstanceOf[TimeoutException]) {
                "timeout"
              } else {
                s"failed - ${ex.getClass.getSimpleName}${if (ex.getMessage == null) "" else " - " + ex.getMessage}"
              }
              Console.err.println(s"Negatum: evaluation $message")
              if (STORE_BAD_DEFS) {
                val dir = userHome / "Documents" / "temp" / "negatum_broken"
                dir.mkdirs()
                val source = MkSynthGraphSource(graph)
                val name   = mkGraphName(graph)
                try {
                  val fos  = new FileOutputStream(dir / s"$name.scala")
                  try {
                    fos.write(source.getBytes("UTF-8"))
                  } finally {
                    fos.close()
                  }
                } catch {
                  case NonFatal(ex2) =>
                    ex2.printStackTrace()
                }
              }
              0.0f
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

    while (iter < numIter && !_shouldStop) {
      val el    = Selection.elitism(config, pop)
      val _sel0 = Selection(config, pop)
      val sel   = scramble(_sel0)

      import config.breed._

      val numGolem1 = math.min(golem, pop.length - el.size)
      val nGen      = pop.length - el.size - numGolem1
      val nMut      = (probMut * nGen + 0.5).toInt
      val nCross    = nGen - nMut

      val mut       = Mutation (config, sel, nMut)
      val cross     = Crossover(config, sel, nCross)

      val golems    = Vector.fill(numGolem1)(mkIndividual())

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
      golems.foreach { indiv =>
        pop(i) = indiv
        i += 1
      }

      evalPop()

      iter += 1
      PROG_COUNT  = iter.toLong * pop.length + INIT_COUNT
      progress    = PROG_COUNT * PROG_WEIGHT
      checkAborted()
    }

    pop.toIndexedSeq
  }

  // ---- utility ----

  /* Creates an individual chromosome. */
  @inline
  private def mkIndividual(): Individual = {
    val g = Chromosome.mkGraph(config)
    new Individual(g)
  }

  // ---- fix ups ----

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

  // ---- observable ----

  def reactNow(fun: (S#Tx) => (State[Unit]) => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
    val res = react(fun)
    fun(tx)(state)
    res
  }

  private def progressTx(amt: Double): Unit = if (!_disposed.single.get)
    cursor.step { implicit tx =>
      state = Rendering.Progress(amt)
    }

  private def mkGraphName(graph: SynthGraph): String = s"negatum-${graph.hashCode().toHexString}"

  def DEBUG_FILL(): Unit = cursor.step { implicit tx =>
    fillResult(DEBUG_ARRAY.toVector)
  }

  private def fillResult(popOut: Vec[Individual])(implicit tx: S#Tx): Unit = {
    val folder = populationH()
    folder.clear()  // XXX TODO --- re-use existing procs?
    popOut.foreach { indiv =>
      val gObj  = SynthGraphObj.newConst[S](indiv.graph)
      val p     = Proc[S]
      import proc.Implicits._
      val attr  = p.attr
      p.name    = mkGraphName(indiv.graph)
      p.graph() = gObj
      if (!indiv.fitness.isNaN) {
        attr.put(Negatum.attrFitness, DoubleObj.newConst[S](indiv.fitness))
      }
      // XXX TODO --- should we store fitness or not?
      folder.addLast(p)
    }
  }

  private def completeWith(t: Try[Vec[Individual]]): Unit = {
    DEBUG_ARRAY = null
    if (!_disposed.single.get) {
      cursor.step { implicit tx =>
        import TxnLike.peer
        if (!_disposed()) t match {
          case Success(popOut) =>
            fillResult(popOut)
            state = Rendering.Completed(Success(()))
          case Failure(ex) =>
            state = Rendering.Completed(Failure(ex))
        }
      }
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

  def state(implicit tx: S#Tx): State[Unit] = {
    import TxnLike.peer
    _state()
  }

  protected def state_=(value: State[Unit])(implicit tx: S#Tx): Unit = {
    import TxnLike.peer
    val old = _state.swap(value)
    if (old != value) fire(value)
  }

  def cancel ()(implicit tx: S#Tx): Unit = tx.afterCommit(abort())
  def stop   ()(implicit tx: S#Tx): Unit = tx.afterCommit { _shouldStop = true }
  def dispose()(implicit tx: S#Tx): Unit = cancel()
}