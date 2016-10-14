/*
 *  package.scala
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

import de.sciss.synth.ugen.{BinaryOpUGen, UnaryOpUGen}
import de.sciss.synth.{GE, UGenSpec, UndefinedRate}
import de.sciss.topology.Topology

package object impl {
  type SynthGraphT = Topology[Vertex, Edge]

  type Vec[+A] = scala.collection.immutable.IndexedSeq[A]

  def graphElemName(in: GE): String =
    in match {
      case bin: BinaryOpUGen =>
        s"Bin_${bin.selector.id}"
      case un: UnaryOpUGen =>
        s"Un_${un.selector.id}"
      case _ => in.productPrefix
    }

  def getGraphRoots(top: SynthGraphT): Vec[Vertex.UGen] = {
    val ugens = top.vertices.reverseIterator.collect {
      case ugen: Vertex.UGen => ugen
    }
    val edges = top.edges
    val f = ugens.filter { ugen =>
      edges.forall(_.targetVertex != ugen)
    }
    f.toIndexedSeq
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
}
