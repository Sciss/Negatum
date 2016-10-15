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
import scala.util.control.NonFatal

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

    // XXX TODO TEST
    try {
      MkSynthGraph(chosenT)
    } catch {
      case NonFatal(ex) =>
        println("AQUI")
        MkTopology(chosen)
    }

    val mutationIter  = rrand(mutMin, mutMax)
    require(mutationIter > 0)
    val res = (chosenT /: (1 to mutationIter)) { case (pred, iter) =>
      val tpe = random.nextInt(2 /* 7 */)
      val next: SynthGraphT = (tpe: @switch) match {
        case 0 => addVertex   (config, pred)
        case 1 => removeVertex(config, pred)
        case 2 => ??? // changeVertex(pred)
        case 3 => ??? // changeEdge  (pred)
        case 4 => ??? // swapEdge    (pred)
        case 5 => ??? // splitVertex (pred)
        case 6 => ??? // mergeVertex (pred)
      }

      // XXX TODO TEST
      if (next ne chosenT) MkSynthGraph(next)

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
}
