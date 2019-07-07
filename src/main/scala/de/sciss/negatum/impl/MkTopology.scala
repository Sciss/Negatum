/*
 *  MkTopology.scala
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

import de.sciss.synth.ugen.{Constant, Mix, NegatumIn, NegatumOut, Protect}
import de.sciss.synth.{GE, SynthGraph, UndefinedRate}
import de.sciss.topology.Topology

object MkTopology {
  /** Converts a synth-graph to a topology representation,
    * automatically taking care of filtering elements such
    * as `NegatumIn`, `NegatumOut`, or `Protect`.
    */
  def apply(g: SynthGraph): SynthGraphT = {
    var top       = Topology.empty[Vertex, Edge]
    var vertexMap = Map.empty[Product, Vertex]
    g.sources.foreach {
      // `Nyquist` is not lazy, thus can never appear in the `sources`
      // case Nyquist() =>
      case _: NegatumOut | _: NegatumIn | _: Protect =>
      case _: Mix =>  // N.B.: `Mix` is only ever used to group the inputs before they go into `NegatumOut`
      case lz =>
        val name  = graphElemName(lz)
        val spec  = UGens.mapAll(name)
        val v     = Vertex.UGen(spec)
        top       = top.addVertex(v)
        vertexMap += lz -> v
        val clazz = lz.getClass
        // XXX TODO --- we'll miss UGenSource.ZeroOut here; cf. UGens issue #49
        val rate  = lz match {
          case ge: GE => ge.rate
          case _      => UndefinedRate
        }
        spec.inputs.foreach { inp =>
          val m       = clazz.getMethod(inp.arg)
          val argVal  = m.invoke(lz) match {
            case Protect(in, _, _, _)  => in
            case p: Product            => p
          }
          val arg     = spec.argMap(inp.arg)
          val df1     = arg.defaults.get(rate)
          val df2     = if (df1.isDefined || rate == UndefinedRate) df1 else arg.defaults.get(UndefinedRate)
          val dfGE    = df2.map(_.toGE)

          if (!dfGE.exists(_ == argVal)) {  // .contains is not available for Scala 2.10!
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
    top
  }
}
