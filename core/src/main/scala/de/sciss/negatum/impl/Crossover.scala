/*
 *  Crossover.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum
package impl

import de.sciss.negatum.Negatum.{Config, SynthGraphT}
import de.sciss.negatum.impl.Util.coin
import de.sciss.topology.Topology

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.util.Random

object Crossover {
  private[this] final val DEBUG = false

  /** Produces a sequence of `n` items by crossing each two parents from the input `sel` selection. */
  def apply(config: Config, sq: IndexedSeq[Individual], n: Int)(implicit random: Random): Vec[Individual] = {
    var res = Vector.empty[Individual]
    if (sq.isEmpty) return res

    while (res.size < n) {
      val idx0      = res.size << 1
      val chosen1   = sq( idx0      % sq.size)
      val chosen2   = sq((idx0 + 1) % sq.size)
      val chosen1T  = MkTopology(chosen1.graph)
      val chosen2T  = MkTopology(chosen2.graph)

      val (x1, x2) = step(config, top1 = chosen1T, top2 = chosen2T)

      val hs = if (res.size + 1 == n) x1 :: Nil else {
        x1 :: x2 :: Nil
      }
      hs.foreach { h => res :+= new Individual(MkSynthGraph(h)) }
    }
    res
  }

  def step(config: Config, top1: SynthGraphT, top2: SynthGraphT)
          (implicit random: Random): (SynthGraphT, SynthGraphT) = {
    import config.gen.maxVertices
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
    val edgesHead1s     = top1.edges.filter(e => head1.contains(e.sourceVertex) && head1.contains(e.targetVertex))
    val edgesHead1      = Chromosome.sortedEdges(top1, edgesHead1s)
    val edgesTail1s     = top1.edges.filter(e => tail1.contains(e.sourceVertex) && tail1.contains(e.targetVertex))
    val edgesTail1      = Chromosome.sortedEdges(top1, edgesTail1s)
    val edgesHead2s     = top2.edges.filter(e => head2.contains(e.sourceVertex) && head2.contains(e.targetVertex))
    val edgesHead2      = Chromosome.sortedEdges(top2, edgesHead2s)
    val edgesTail2s     = top2.edges.filter(e => tail2.contains(e.sourceVertex) && tail2.contains(e.targetVertex))
    val edgesTail2      = Chromosome.sortedEdges(top2, edgesTail2s)

    val severedHeads1s  = top1.edges.collect {
      case Edge(source: Vertex.UGen, target, _) if head1.contains(source) && tail1.contains(target) => source
    }
    val severedHeads1   = Chromosome.sortedVertices(top1, severedHeads1s)
    val severedHeads2s  = top2.edges.collect {
      case Edge(source: Vertex.UGen, target, _) if head2.contains(source) && tail2.contains(target) => source
    }
    val severedHeads2   = Chromosome.sortedVertices(top2, severedHeads2s)

    @tailrec def shrinkTop(top: SynthGraphT, target: Int, iteration: Int): SynthGraphT =
      if (top.vertices.size <= target || iteration == maxVertices) top else {
        val (top1, _) = Mutation.removeVertex1(config, top)
        shrinkTop(top1, target = target, iteration = iteration + 1)
      }

    def mkTop(vertices1: Vec[Vertex], edges1: Seq[Edge], vertices2: Vec[Vertex], edges2: Seq[Edge]): SynthGraphT = {
      val t1a = vertices1.foldLeft(Topology.empty[Vertex, Edge])(_ addVertex _)
      val t1b = edges1.foldLeft(t1a)(_.addEdge(_).get._1)  // this is now the first half of the original top

      val (t2a, e2cpy) = vertices2.foldLeft((t1b, edges2)) { case ((t0, e0), v0) =>
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
      e2cpy.foldLeft(t2a) { (t0, e0) =>
        val res = t0.addEdge(e0)
        if (res.isFailure) {
          println("WARNING: Cross-over mkTop - cycle detected!")
        }
        res.toOption.fold(t0)(_._1)
      }
    }

    val topC1a = mkTop(head1, edgesHead1, tail2, edgesTail2)
    val topC2a = mkTop(head2, edgesHead2, tail1, edgesTail1)

    def complete(top: SynthGraphT, inc: Seq[Vertex.UGen]): SynthGraphT = {
      val top1 = if (inc.isEmpty) top else inc.foldLeft(top)((res, v) => Chromosome.completeUGenInputs(config, res, v))
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
}
