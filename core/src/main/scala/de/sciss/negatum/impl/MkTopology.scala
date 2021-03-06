/*
 *  MkTopology.scala
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

import de.sciss.negatum.Negatum.SynthGraphT
import de.sciss.negatum.impl.Util.graphElemName
import de.sciss.synth.ugen.{Constant, GESeq, Mix, NegatumIn, NegatumOut, Protect}
import de.sciss.synth.{GE, SynthGraph, UndefinedRate}
import de.sciss.topology.Topology

object MkTopology {
  /** Maps from a synth-graph's sources index to vertices. */
  type SourceMap = Map[Int, Vertex.UGen]

  def apply(g: SynthGraph): SynthGraphT =
    applyImpl(g, mkMap = false, removeProtect = true)._1

  /** Converts a synth-graph to a topology representation,
    * automatically taking care of filtering elements such
    * as `NegatumIn`, `NegatumOut`, or `Protect`.
    *
    * The edge orientation is such that edges point from a graph element (source)
    * to its input arguments (sink).
    *
    * Returns a tuple consisting of the topology and the map
    * from source indices to vertices. (Note: there will be
    * more vertices in the topology than sources, since the
    * latter does not include constants and zero-output elements).
    */
  def withSourceMap(g: SynthGraph, removeProtect: Boolean): (SynthGraphT, SourceMap) =
    applyImpl(g, mkMap = true, removeProtect = removeProtect)

  private def applyImpl(g: SynthGraph, mkMap: Boolean, removeProtect: Boolean): (SynthGraphT, SourceMap) = {
    var top       = Topology.empty[Vertex, Edge]
    var vertexMap = Map.empty[Product, Vertex]
    var sourceMap = Map.empty[Int, Vertex.UGen]
    g.sources.iterator.zipWithIndex.foreach { case (lz, lzIdx) =>
      lz match {
        // `Nyquist` is not lazy, thus can never appear in the `sources`
        // case Nyquist() =>

        case Mix(GESeq(elems)) =>
          // issue #8 -- since constants are always
          // dropped from the mix when reconstructing the
          // mix, convert them to DCs here. they may be
          // the result of UnaryOpGen or BinaryOpUGen optimisation.
          // (this should be fixed now, but we keep it to fix existing graphs)
          // XXX TODO drop this in future versions
          var dc = 0.0
          elems.foreach {
            case Constant(f) => dc += f
            case _ =>
          }
          if (dc != 0.0) {
            val f     = dc.toFloat
            val c     = Constant(f)
            val name  = "DC"
            val spec  = UGens.mapAll(name)
            val v     = Vertex.UGen(spec)
            top       = top.addVertex(v)
            vertexMap += lz -> v
            if (mkMap) sourceMap += lzIdx -> v

            {
              val inp   = spec.inputs.head
              val vIn   = Vertex.Constant(f)
              top       = top.addVertex(vIn)
              vertexMap += c -> vIn
              val e = Edge(sourceVertex = v, targetVertex = vIn, inlet = inp.arg)
              top   = top.addEdge(e).get._1
            }
          }

        case _: NegatumOut | _: NegatumIn /*| _: Protect */ | _: Mix =>
          // N.B.: `Mix` is only ever used to group the inputs before they go into `NegatumOut`
        case _: Protect if removeProtect =>
        case _ =>
          val name  = graphElemName(lz)
          val spec  = UGens.mapAll(name)
          val v     = Vertex.UGen(spec)
          top       = top.addVertex(v)
          vertexMap += lz -> v
          if (mkMap) sourceMap += lzIdx -> v
          val clazz = lz.getClass
          // XXX TODO --- we'll miss UGenSource.ZeroOut here; cf. UGens issue #49
          val rate  = lz match {
            case ge: GE => ge.rate
            case _      => UndefinedRate
          }
          spec.inputs.foreach { inp =>
            val m       = clazz.getMethod(inp.arg)
            val argVal  = m.invoke(lz) match {
              case Protect(in, _, _, _) if removeProtect  => in
              case p: Product                             => p
            }
            val arg     = spec.argMap(inp.arg)
            val df1     = arg.defaults.get(rate)
            val df2     = if (df1.isDefined || rate == UndefinedRate) df1 else arg.defaults.get(UndefinedRate)
            val dfGE    = df2.map(_.toGE)

            if (!dfGE.contains(argVal)) {
              val vIn = vertexMap.getOrElse(argVal, {
                argVal match {
                  case c @ Constant(f) =>
                    val _vIn  = Vertex.Constant(f)
                    top       = top.addVertex(_vIn)
                    vertexMap += c -> _vIn
                    _vIn

                  case p: Product =>
                    throw new IllegalStateException(s"Found non-lazy argument $p. Don't know what to do")
                }
              })
              val e = Edge(sourceVertex = v, targetVertex = vIn, inlet = inp.arg)
              top   = top.addEdge(e).get._1
            }
          }
        }
    }
    (top, sourceMap)
  }
}
