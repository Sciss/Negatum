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
import de.sciss.synth.{UGenSpec, UndefinedRate}
import de.sciss.topology.Topology

package object impl {
  type SynthGraphT = Topology[Vertex, Edge]

  type Vec[+A] = scala.collection.immutable.IndexedSeq[A]

  def graphElemName(in: Product): String =
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
}
