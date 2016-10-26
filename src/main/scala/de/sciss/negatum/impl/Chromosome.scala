/*
 *  Chromosome.scala
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

import de.sciss.negatum.Negatum.Config
import de.sciss.negatum.impl.Util._
import de.sciss.synth.{SynthGraph, UGenSpec, UndefinedRate}
import de.sciss.topology.Topology

import scala.annotation.tailrec
import scala.util.Random

object Chromosome {
  def mkIndividual(config: Config)(implicit random: Random): Individual =
    new Individual(mkGraph(config))

  /* Creates an individual chromosome. */
  def mkGraphT(config: Config)(implicit random: Random): SynthGraphT = {
    import config.gen._
    val num = rrand(minVertices, maxVertices)
    @tailrec def loopGraph(pred: SynthGraphT): SynthGraphT =
      if (pred.vertices.size >= num) pred else loopGraph(Mutation.addVertex(config, pred))

    loopGraph(Topology.empty)
  }

  def mkGraph(config: Config)(implicit random: Random): SynthGraph = {
    val t0  = mkGraphT(config)
    val g   = MkSynthGraph(t0, mono = true, removeNaNs = true, specialOut = true, ranges = true)
    g
  }

  def mkUGen()(implicit random: Random): Vertex.UGen = {
    val spec = choose(UGens.seq)
    Vertex.UGen(spec)
  }

  def mkConstant()(implicit random: Random): Vertex.Constant = Vertex.Constant(mkConstantValue())

  def mkConstantValue()(implicit random: Random): Float = {
    val f0  = exprand(0.001, 10000.001) - 0.001
    val f   = if (coin(0.25)) -f0 else f0
    f.toFloat
  }

  private[this] val CHECK = false

  /** Auxiliary method that returns a deterministically
    * ordered collection of the edge set.
    */
  def sortedEdges(in: SynthGraphT): Vec[Edge] =
    in.vertices.flatMap(sortedEdges(in, _))

  /** Auxiliary method that returns a deterministically
    * ordered collection of the edge set.
    */
  def sortedEdges(in: SynthGraphT, set: Set[Edge]): List[Edge] =
    set.toList.sortBy { e => in.vertices.indexOf(e.sourceVertex) -> e.inlet }

  /** Auxiliary method that returns a deterministically
    * ordered collection of the edge set.
    */
  def sortedEdges(in: SynthGraphT, v: Vertex): List[Edge] =
    in.edgeMap.get(v) match {
      case Some(set)  => set.toList.sortBy(e => e.inlet)
      case None       => List.empty
    }

  def sortedVertices[V <: Vertex](in: SynthGraphT, set: Set[V]): List[V] =
    set.toList.sortBy(v => in.vertices.indexOf(v))

  def checkComplete(succ: SynthGraphT, message: => String): Unit =
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

  def completeUGenInputs(config: Config, t1: SynthGraphT, v: Vertex.UGen)(implicit random: Random): SynthGraphT = {
    import config.gen.probDefault

    val spec      = v.info
    // An edge's source is the consuming UGen, i.e. the one whose inlet is occupied!
    // A topology's edgeMap uses source-vertices as keys. Therefore, we can see
    // if the an argument is connected by getting the edges for the ugen and finding
    // an edge that uses the inlet name.
    val edgeList  = sortedEdges(t1, v) // .edgeMap.getOrElse(v, Set.empty)
    val argsFree  = geArgs(spec).filter { arg => !edgeList.exists(_.inlet == arg.name) }
    val (hasDef, hasNoDef)          = argsFree.partition(_.defaults.contains(UndefinedRate))
    val (_ /* useDef */, useNotDef) = hasDef  .partition(_ => coin(probDefault))
    val findDef   = hasNoDef ++ useNotDef

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

  def geArgs(spec: UGenSpec): Vec[UGenSpec.Argument] =
    spec.args.filter { arg =>
      arg.tpe match {
        case UGenSpec.ArgumentType.Int => false
        case UGenSpec.ArgumentType.GE(UGenSpec.SignalShape.DoneAction, _) => false
        case _ => true
      }
    }

  def findIncompleteUGenInputs(t1: SynthGraphT, v: Vertex.UGen): Vec[String] = {
    val spec      = v.info
    val edgeSet   = t1.edgeMap.getOrElse(v, Set.empty)
    val argsFree  = geArgs(spec).filter { arg => !edgeSet.exists(_.inlet == arg.name) }
    val inc       = argsFree.filterNot(_.defaults.contains(UndefinedRate))
    inc.map(_.name)
  }

  def getArgUsages(top: SynthGraphT, arg: Vertex): List[Edge] = {
    val set = top.edges.filter(_.targetVertex == arg)
    sortedEdges(top, set)
  }
}