/*
 *  Mutation.scala
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

import de.sciss.negatum.Negatum.Config
import de.sciss.negatum.impl.Util._
import de.sciss.synth.SynthGraph

import scala.annotation.switch
import scala.util.Random

object Mutation {
  /* Produces a sequence of `n` items by mutating the input `sel` selection. */
  def apply(config: Config, sq: Vec[Individual], n: Int)(implicit random: Random): Vec[Individual] = {
    var res = Vector.empty[Individual]
    if (sq.isEmpty) return res

    while (res.size < n) {
      val chosen = sq(res.size % sq.size)
      val hOpt: Option[Individual] = {
        val ok = tryMutate(config, chosen.graph)
        if (ok ne chosen.graph) {
          Some(new Individual(ok))
        } else None
      }
      hOpt.foreach { h => res :+= h }
    }
    res
  }

  private def tryMutate(config: Config, chosen: SynthGraph)(implicit random: Random): SynthGraph = {
    import config.breeding.{mutMax, mutMin}

    val chosenT = MkTopology(chosen)

    // TEST
//    try {
//      MkSynthGraph(chosenT)
//    } catch {
//      case NonFatal(ex) =>
//        println("AQUI")
//        MkTopology(chosen)
//    }

    val mutationIter  = rrand(mutMin, mutMax)
    require(mutationIter > 0)
    val res = (chosenT /: (1 to mutationIter)) { case (pred, iter) =>
      val tpe = random.nextInt(7)
      val next: SynthGraphT = (tpe: @switch) match {
        case 0 => addVertex   (config, pred)
        case 1 => removeVertex(config, pred)
        case 2 => changeVertex(config, pred)
        case 3 => changeEdge  (config, pred)
        case 4 => swapEdge    (config, pred)
        case 5 => splitVertex (config, pred)
        case 6 => mergeVertex (config, pred)
      }

      // TEST
//      if (next ne chosenT) MkSynthGraph(next)

      //      if (next ne pred) {
      //        validate1(next)
      //      }

      next
    }

    if (res ne chosenT) MkSynthGraph(res) else chosen
  }

  // ---- mutations ----

  def addVertex(config: Config, pred: SynthGraphT)(implicit random: Random): SynthGraphT = {
    import config.generation.constProb
    val next: SynthGraphT = if (coin(constProb)) {
      val v = Chromosome.mkConstant()
      pred.addVertex(v)

    } else {
      val v   = Chromosome.mkUGen()
      val t1  = pred.addVertex(v)
      Chromosome.completeUGenInputs(config, t1, v)
    }
    next
  }

  def removeVertex(config: Config, c: SynthGraphT)(implicit random: Random): SynthGraphT = {
    import config.generation.minNumVertices
    val vertices    = c.vertices
    val numVertices = vertices.size
    if (numVertices <= minNumVertices) c else {
      val (res, _) = removeVertex1(config, c)
      Chromosome.checkComplete(res, s"removeVertex($c)")
      //      stats(1) += 1
      res
    }
  }

  def removeVertex1(config: Config, top: SynthGraphT)(implicit random: Random): (SynthGraphT, Vertex) = {
    val vertices    = top.vertices
    val numVertices = vertices.size
    val idx         = random.nextInt(numVertices)
    val v           = vertices(idx)
    val targets     = Chromosome.getArgUsages(top, v)
    val top1        = top.removeVertex(v)
    val top3        = (top1 /: targets) { (top2, e) =>
      val x = top2.removeEdge(e)
      assert(x ne top2)
      x
    }
    val succ = (top3 /: targets) { case (top4, Edge(t: Vertex.UGen, _, _)) =>
      Chromosome.completeUGenInputs(config, top4, t)
    }
    (succ, v)
  }

  private def changeVertex(config: Config, top: SynthGraphT)(implicit random: Random): SynthGraphT = {
    val vertices    = top.vertices
    val numVertices = vertices.size

    val idx     = random.nextInt(numVertices)
    val vOld    = vertices(idx)
    val outlet  = Chromosome.getArgUsages(top, vOld)
    val inlets  = top.edgeMap.getOrElse(vOld, Set.empty)
    val top1    = (top  /: outlet)(_ removeEdge _)
    val top2    = (top1 /: inlets)(_ removeEdge _)
    val top3    = top2.removeVertex(vOld)

    val vNew    = vOld match {
      case Vertex.Constant(f) =>
        if (coin())
          Chromosome.mkConstant()   // completely random
        else
          Vertex.Constant(f * exprand(0.9, 1.0/0.9).toFloat) // gradual change
      case _ =>
        Chromosome.mkUGen()
    }

    val oldInletNames: Vec[String] = vOld match {
      case Vertex.UGen(info) => /* ChromosomeImpl.geArgs(info).map(_.name) */ info.inputs.map(_.arg)
      case _ => Vector.empty
    }
    val newInletNames: Vec[String] = vNew match {
      case Vertex.UGen(info) => /* ChromosomeImpl.geArgs(info).map(_.name) */ info.inputs.map(_.arg)
      case _ => Vector.empty
    }

    val top4  = top3.addVertex(vNew)
    val top5  = (top4 /: outlet.map(_.copy(targetVertex = vNew)))((t, e) => t.addEdge(e).get._1)

    // just as many as possible, leaving tail inlets empty
    val newInlets = inlets.collect {
      case e if oldInletNames.indexOf(e.inlet) < newInletNames.size =>
        e.copy(sourceVertex = vNew, inlet = newInletNames(oldInletNames.indexOf(e.inlet)))
    }

    val top6  = (top5 /: newInlets)((t, e) => t.addEdge(e).get._1)
    val top7  = vNew match {
      case vu: Vertex.UGen => Chromosome.completeUGenInputs(config, top6, vu)
      case _ => top6
    }

    val res = top7 // new Chromosome(top7, seed = random.nextLong())
    // stats(2) += 1
    res
  }

  private def changeEdge(config: Config, top: SynthGraphT)(implicit random: Random): SynthGraphT = {
    val vertices    = top.vertices

    val candidates  = vertices.collect {
      case v @ Vertex.UGen(spec) if Chromosome.geArgs(spec).nonEmpty /* spec.inputs.nonEmpty */ => v
    }

    if (candidates.isEmpty) top else {
      val v     = choose(candidates)
      val edges = top.edgeMap.getOrElse(v, Set.empty)
      val top1  = if (edges.isEmpty) top else top.removeEdge(choose(edges))
      val top2  = Chromosome.completeUGenInputs(config, top1, v)
      if (top2 == top) top else {
        val res = top2 // new Chromosome(top2, seed = random.nextLong())
        // stats(3) += 1
        res
      }
    }
  }

  private def swapEdge(config: Config, top: SynthGraphT)(implicit random: Random): SynthGraphT = {
    val vertices    = top.vertices

    val candidates  = vertices.collect {
      case v @ Vertex.UGen(spec) if top.edgeMap.get(v).exists(_.size >= 2) => v
    }

    if (candidates.isEmpty) top else {
      val v     = Util.choose(candidates)
      val edges = top.edgeMap.getOrElse(v, Set.empty)
      val e1    = Util.choose(edges)
      val e2    = Util.choose(edges - e1)
      val top1  = top .removeEdge(e1)
      val top2  = top1.removeEdge(e2)
      val e1New = e1.copy(targetVertex = e2.targetVertex)
      val e2New = e2.copy(targetVertex = e1.targetVertex)
      val top3  = top2.addEdge(e1New).get._1
      val top4  = top3.addEdge(e2New).get._1
      val res   = top4 // new Chromosome(top4, seed = random.nextLong())
      // stats(4) += 1
      res
    }
  }

  // splits a vertex. candidates are vertices with out degree >= 2.
  // candidate is chosen using roulette algorithm (thus more likely,
  // the higher the out degree).
  private def splitVertex(config: Config, top: SynthGraphT)(implicit random: Random): SynthGraphT = {
    val verticesIn  = top.vertices
    val numVertices = verticesIn.size
    if (numVertices >= config.generation.maxNumVertices) return top

    val weighted  = verticesIn.flatMap { v =>
      val set = top.edges.filter(_.targetVertex == v)
      val sz  = set.size
      if (sz > 2) Some(set -> sz) else None
    }
    if (weighted.isEmpty) top else {
      val edges = roulette(weighted)
      val edgesS: Vec[Edge] = scramble(edges.toVector)
      val (_, edgesMove) = edgesS.splitAt(edgesS.size/2)
      val vertexOld = edges.head.targetVertex
      val vertexNew = vertexOld.copy()
      val top1 = (top /: edgesMove)(_ removeEdge _)
      val top2 = top1.addVertex(vertexNew)
      val top3 = (top2 /: edgesMove) { (t, eOld) =>
        val eNew = eOld.copy(targetVertex = vertexNew)
        t.addEdge(eNew).get._1
      }
      val succ = (top3 /: top.edgeMap.getOrElse(vertexOld, Set.empty)) { (t, eOld) =>
        val eNew = eOld.copy(sourceVertex = vertexNew)
        t.addEdge(eNew).get._1
      }
      val res = succ // new Chromosome(succ, seed = random.nextLong())
      Chromosome.checkComplete(succ, s"splitVertex()")
      // stats(5) += 1
      res
    }
  }

  // merges two vertices of the same type
  private def mergeVertex(config: Config, top: SynthGraphT)(implicit random: Random): SynthGraphT = {
    val verticesIn  = top.vertices
    val numVertices = verticesIn.size
    if (numVertices <= config.generation.minNumVertices) return top

    val it = scramble(verticesIn).tails.flatMap {
      case head +: tail =>
        val partners = head match {
          case Vertex.Constant(_) => tail.filter {
            case Vertex.Constant(_) => true // any two constants will do
            case _ => false
          }
          case Vertex.UGen(info) => tail.filter {
            case Vertex.UGen(i1) => i1.name == info.name
            case _ => false
          }
        }
        partners.map(head -> _)

      case _ => None
    }
    if (it.isEmpty) top else {
      val (v1, v2)  = it.next()
      val edgesOld  = top.edges.filter(_.targetVertex == v2)
      val top1      = (top  /: edgesOld)(_ removeEdge _)
      val succ      = (top1 /: edgesOld) { (t, eOld) =>
        val eNew = eOld.copy(targetVertex = v1)
        if (t.canAddEdge(eNew)) t.addEdge(eNew).get._1 else t
      }
      val res = succ // new Chromosome(succ, seed = random.nextLong())
      Chromosome.checkComplete(succ, s"mergeVertex()")
//      stats(6) += 1
      res
    }
  }
}