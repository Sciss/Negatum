/*
 *  Mutation.scala
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
package impl

import de.sciss.negatum.Negatum.Config
import de.sciss.negatum.impl.Util._

import scala.annotation.{switch, tailrec}
import scala.util.Random

object Mutation {
  private[this] final val DEBUG = false

  var INTERRUPT = false

  /* Produces a sequence of `n` items by mutating the input `sel` selection. */
  def apply(config: Config, sq: Vec[Individual], n: Int)(implicit random: Random): Vec[Individual] = {
    var res   = Vector.empty[Individual]
    val sqSz  = sq.size
    if (sqSz == 0) return res

    var off   = 0
    var sq1   = sq

    while (res.size < n && !INTERRUPT) {
      val chosen  = sq1(off /* res.size % sq.size */)
      val chosenT = MkTopology(chosen.graph)
      var attempt = 4
      while (attempt > 0) {
        var out = tryMutate(config, chosenT)
        while (out.vertices.size > config.gen.maxVertices) {
          out = removeVertex(config, out)
        }
        if (out ne chosenT) {
          res   :+= new Individual(MkSynthGraph(out))
          attempt = 0
        } else {
          attempt -= 1
          if (attempt == 0) {
            Console.err.println(s"Mutation - giving up with individual ${off % sq.size}")
            val newIndiv = Chromosome.mkIndividual(config)
            sq1 = sq1.patch(off, newIndiv :: Nil, 1)
//            if (off == sq.size - 1 && res.isEmpty) {
//              Console.err.println("Mutation - WTF - could not mutate even one individual.")
//              return Vector.empty
//            }
          }
        }
      }
      off = (off + 1) % sqSz
    }

    if (INTERRUPT) {
      println(s"Mutation -- interrupted; off was $off")
    }

    res
  }

  private def tryMutate(config: Config, chosenT: SynthGraphT)(implicit random: Random): SynthGraphT = {
    import config.breed.{maxMut, minMut}

    if (DEBUG) try {
      MkSynthGraph(chosenT)
    } catch {
      case ex: MkSynthGraph.Incomplete =>
        println("AQUI")
        println(ex)
//        MkTopology(chosen)
    }

    val mutationIter  = rrand(minMut, maxMut)
    require(mutationIter > 0)
    val res = (1 to mutationIter).foldLeft(chosenT) { case (pred, _ /*iter*/) =>
      val tpe = random.nextInt(7)
      val TEST = if (!DEBUG) 0L else {
        val n = random.nextLong()
        random.setSeed(n)
        n
      }
      val next: SynthGraphT = (tpe: @switch) match {
        case 0 => addVertex   (config, pred)
        case 1 => removeVertex(config, pred)
        case 2 => changeVertex(config, pred)
        case 3 => changeEdge  (config, pred)
        case 4 => swapEdge    (config, pred)
        case 5 => splitVertex (config, pred)
        case 6 => mergeVertex (config, pred)
      }

      if (DEBUG && (next ne pred))
        try {
          MkSynthGraph(next)
        } catch {
          case ex: MkSynthGraph.Incomplete =>
            println(s"AQUI - tpe = $tpe")
            println("----before----")
            println(TopologyDOT(pred))
            println("----after----")
            println(TopologyDOT(next))
            println(ex)
            random.setSeed(TEST)
            /* val again: SynthGraphT = */ (tpe: @switch) match {
              case 0 => addVertex   (config, pred)
              case 1 => removeVertex(config, pred)
              case 2 => changeVertex(config, pred)
              case 3 => changeEdge  (config, pred)
              case 4 => swapEdge    (config, pred)
              case 5 => splitVertex (config, pred)
              case 6 => mergeVertex (config, pred)
            }
            // pred // MkTopology(pred)
        }

      //      if (next ne pred) {
      //        validate1(next)
      //      }

      next
    }

    res // if (res ne chosenT) MkSynthGraph(res) else chosen
  }

  // ---- mutations ----

  def addVertex(config: Config, pred: SynthGraphT)(implicit random: Random): SynthGraphT = {
    import config.gen.probConst
    val next: SynthGraphT = if (coin(probConst)) {
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
    import config.gen.minVertices
    val vertices    = c.vertices
    val numVertices = vertices.size
    if (numVertices <= minVertices) c else {
      val (res, _) = removeVertex1(config, c)
      // Chromosome.checkComplete(res, s"removeVertex($c)")
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
    val top3        = targets.foldLeft(top1) { (top2, e) =>
      val x = top2.removeEdge(e)
      assert(x ne top2)
      x
    }
    val succ = targets.foldLeft(top3) { case (top4, Edge(t: Vertex.UGen, _, _)) =>
      Chromosome.completeUGenInputs(config, top4, t)
    }
    (succ, v)
  }

  private def changeVertex(config: Config, top: SynthGraphT)(implicit random: Random): SynthGraphT = {
    val vertices    = top.vertices
    val numVertices = vertices.size
    if (numVertices == 0) return top

    val idx     = random.nextInt(numVertices)
    val vOld    = vertices(idx)
    val outlet  = Chromosome.getArgUsages(top, vOld)
    val inlets  = Chromosome.sortedEdges (top, vOld)
    val top1    = outlet.foldLeft(top )(_ removeEdge _)
    val top2    = inlets.foldLeft(top1)(_ removeEdge _)
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

    val top4    = top3.addVertex(vNew)
    val outletM = outlet.map(_.copy(targetVertex = vNew))
    val top5    = outletM.foldLeft(top4)((t, e) => t.addEdge(e).get._1)

    // just as many as possible, leaving tail inlets empty
    val newInlets = inlets.collect {
      case e if oldInletNames.indexOf(e.inlet) < newInletNames.size =>
        e.copy(sourceVertex = vNew, inlet = newInletNames(oldInletNames.indexOf(e.inlet)))
    }

    val top6  = newInlets.foldLeft(top5)((t, e) => t.addEdge(e).get._1)
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
      val edges = Chromosome.sortedEdges(top, v)
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
      case v @ Vertex.UGen(_ /*spec*/) if top.edgeMap.get(v).exists(_.size >= 2) => v
    }

    if (candidates.isEmpty) top else {
      val v     = Util.choose(candidates)
      val edges = Chromosome.sortedEdges(top, v)
      val e1    = Util.choose(edges)
      val e2    = Util.choose(edges.filterNot(_ == e1))
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
    if (numVertices >= config.gen.maxVertices) return top

    val weighted: Vec[(List[Edge], Int)] = verticesIn.flatMap { v =>
      val list  = Chromosome.getArgUsages(top, v)
      val sz    = list.size
      if (sz > 2) Some(list -> sz) else None
    }
    if (weighted.isEmpty) top else {
      val edges : List[Edge]  = roulette(weighted)
      val edgesS: List[Edge]  = scramble(edges)
      val (_, edgesMove)      = edgesS.splitAt(edgesS.size/2)
      val vertexOld           = edges.head.targetVertex
      val vertexNew           = vertexOld.copy()
      val top1                = edgesMove.foldLeft(top)(_ removeEdge _)
      val top2                = top1.addVertex(vertexNew)
      val top3                = edgesMove.foldLeft(top2) { (t, eOld) =>
        val eNew = eOld.copy(targetVertex = vertexNew)
        t.addEdge(eNew).get._1
      }
      val edgesSt = Chromosome.sortedEdges(top, vertexOld)
      val succ = edgesSt.foldLeft(top3) { (t, eOld) =>
        val eNew = eOld.copy(sourceVertex = vertexNew)
        t.addEdge(eNew).get._1
      }
      val res = succ // new Chromosome(succ, seed = random.nextLong())
      // Chromosome.checkComplete(succ, s"splitVertex()")
      // stats(5) += 1
      res
    }
  }

  // merges two vertices of the same type
  private def mergeVertex(config: Config, top: SynthGraphT)(implicit random: Random): SynthGraphT = {
    val verticesIn  = top.vertices
    val numVertices = verticesIn.size
    if (numVertices <= config.gen.minVertices) return top

    val it = scramble(verticesIn).tails.flatMap {
      case head +: tail =>
        val partners = head match {
          case Vertex.Constant(_) => tail.filter(_.isConstant)  // any two constants will do
          case Vertex.UGen(info) => tail.filter {
            case Vertex.UGen(i1) => i1.name == info.name
            case _ => false
          }
        }
        partners.map(head -> _)

      case _ => None
    }

    @tailrec
    def loop(): SynthGraphT = if (it.isEmpty) top else {
      // we want to get rid of `v2`, and everywhere it was used,
      // use `v1` as argument instead.
      val (v1, v2)  = it.next()
      val edgesOld: List[Edge] = Chromosome.getArgUsages(top, v2)  // all edges pointing to the old vertex
      val top1      = edgesOld.foldLeft(top)(_ removeEdge _)

      @tailrec
      def inner(pred: SynthGraphT, rem: List[Edge]): Option[SynthGraphT] = rem match {
        case head :: tail =>
          val eNew = head.copy(targetVertex = v1)
          if (pred.canAddEdge(eNew)) {
            val next = pred.addEdge(eNew).get._1
            inner(pred = next, rem = tail)
          } else None
        case _ =>
          val edgesOld2 = Chromosome.sortedEdges(top, v2)
          val next0 = edgesOld2.foldLeft(pred)(_ removeEdge _)
          val next  = next0.removeVertex(v2)
          Some(next)
      }

      val allReplaced = inner(top1, edgesOld)
      allReplaced match {
        case Some(x)    => x
        case None       => loop() // try again with different pair
      }
    }

    val res = loop()
    // Chromosome.checkComplete(res, "mergeVertex()")
    //      stats(6) += 1
    res
  }
}