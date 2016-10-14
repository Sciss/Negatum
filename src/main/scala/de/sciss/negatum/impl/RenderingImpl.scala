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

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Sys, TxnLike}
import de.sciss.negatum.Negatum.Rendering.State
import de.sciss.negatum.Negatum.{Config, Rendering}
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.synth.proc.{AudioCue, Folder, SoundProcesses, SynthGraphObj, WorkspaceHandle}
import de.sciss.synth.{SynthGraph, UGenSpec, UndefinedRate}
import de.sciss.topology.Topology

import scala.annotation.tailrec
import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.blocking
import scala.concurrent.stm.Ref
import scala.util.{Failure, Random, Success, Try}

final class RenderingImpl[S <: Sys[S]](config: Config, template: AudioCue,
                                       popIn: Vec[Individual], populationH: stm.Source[S#Tx, Folder[S]], numIter: Int)
                                      (implicit cursor: stm.Cursor[S])
  extends Rendering[S]
    with ObservableImpl[S, Rendering.State]
    with ProcessorImpl[Vec[Individual], Rendering[S]] {

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

    var iter = 0
    while (iter < numIter) {
      iter += 1
    }

    println("TODO: evaluate, select, breed")

    pop.toIndexedSeq
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

  private def completeWith(t: Try[Vec[Individual]]): Unit = if (!_disposed.single.get)
    cursor.step { implicit tx =>
      import TxnLike.peer
      if (!_disposed()) t match {
        case Success(popOut) =>
          val folder = populationH()
          folder.clear()
          popOut.foreach { indiv =>
            val gObj = SynthGraphObj.newConst[S](indiv.graph)
            // XXX TODO --- should we store fitness or not?
            folder.addLast(gObj)
          }
          state = Rendering.Success
        case Failure(ex) =>
          state = Rendering.Failure(ex)
      }
    }

  def startTx()(implicit tx: S#Tx, workspace: WorkspaceHandle[S]): Unit = {
    tx.afterCommit {
      // NB: bad design in `ProcessorImpl`; because we're in the sub-class,
      // we have implicit execution context in scope, but that's the one
      // we want to _set_ here.
      start()(SoundProcesses.executionContext)
      this.andThen {
        case x => completeWith(x)
      }
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