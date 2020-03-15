/*
 *  MkSynthGraph.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum
package impl

import de.sciss.lucre.stm.Sys
import de.sciss.negatum.Negatum.SynthGraphT
import de.sciss.negatum.impl.ParamRanges.Dynamic
import de.sciss.negatum.impl.Util.{getGraphRoots, graphElemName}
import de.sciss.synth.ugen.{Constant, Protect}
import de.sciss.synth.{GE, SynthGraph, UGenSource, UGenSpec, UndefinedRate, ugen}

import scala.annotation.tailrec

object MkSynthGraph {
  final case class Incomplete(in: SynthGraphT, vertex: Vertex.UGen, arg: String, argList: Seq[String])
    extends Exception {

    override def toString =
      s"$productPrefix(\n  in = $in,\n  vertex = $vertex,\n  arg = $arg\n,  argList = $argList\n)"
  }

  def isDynamic(in: GE): Boolean = in match {
    case Constant(_) => false
    case _ =>
      ParamRanges.map.get(graphElemName(in)).exists { info =>
        def getArg(name: String): Any = in.getClass.getMethod(name).invoke(in)

        def check(d: Dynamic): Boolean = d match {
          case Dynamic.Always => true
          case Dynamic.And(elems @ _*) => elems.forall(check)
          case Dynamic.Or (elems @ _*) => elems.exists(check)
          case Dynamic.IfOver (param, v) =>
            getArg(param) match {
              case Constant(f) if f >= v => true
              case _ => false // XXX TODO --- could be more complex
            }
          case Dynamic.IfUnder(param, v) =>
            getArg(param) match {
              case Constant(f) if f <= v => true
              case _ => false // XXX TODO --- could be more complex
            }
          case Dynamic.In(param) =>
            getArg(param) match {
              case argGE: GE => isDynamic(argGE)
              case _ => false
            }
        }

        info.dynamic.exists(check)
      }
  }

  /** Creates a synth graph from a chromosome, possibly inserting safety
    * measures such as removal of NaNs or protected against out-of-range parameters.
    *
    * @param c              the chromosome that shall be converted
    * @param specialIO     if `true`, adds a `NegatumIn` and `NegatumOut`
    * @param protect       if `true`, inserts range protection checks for known parameters
    * @param mono          if `true`, adds a `Mix.mono` (only used when `!specialIO`)
    * @param removeNaNs    if `true`, adds a bad-value check and a gate that stops NaNs (only used when `!specialOut`)
    * @param expandProtect if `true`, expands the range protecting UGens that would otherwise be
    *                      encapsulated in a `Protect` graph element.
    * @param expandIO      if `true`, expands the UGens that would otherwise be
    *                      encapsulated in the `NegatumIn` and `NegatumOut` graph element.
    */
  def apply[S <: Sys[S]](c            : SynthGraphT,
                         specialIO    : Boolean = true,
                         protect      : Boolean = true,
                         mono         : Boolean = true,
                         removeNaNs   : Boolean = true,
                         expandProtect: Boolean = false,
                         expandIO     : Boolean = false
                        ): SynthGraph = {
    @tailrec def loop(remRev: List[Vertex], real: Map[Vertex, GE]): Map[Vertex, GE] = remRev match {
      case last :: init =>
        lazy val lastE = c.edgeMap.getOrElse(last, Set.empty) // top.targets(last)

        def getReal(name: String): Option[GE] =
          lastE.flatMap { e =>
            if (e.inlet == name) real.get(e.targetVertex) else None
          } .headOption

        val value: GE = last match {
          case vf: Vertex.Constant => ugen.Constant(vf.f)
          case u: Vertex.UGen =>
            val spec = u.info
            val ins = spec.args.map { arg =>
              val res: (AnyRef, Class[_]) = arg.tpe match {
                case UGenSpec.ArgumentType.Int =>
                  val v = arg.defaults.get(UndefinedRate) match {
                    case Some(UGenSpec.ArgumentValue.Int(i)) => i
                    case _ => 1 // rangeRand(1, 2)
                  }
                  (v.asInstanceOf[AnyRef], classOf[Int])

                case UGenSpec.ArgumentType.GE(_, _) =>
                  val inGEOpt0 = getReal(arg.name)

                  val inGEOpt: Option[GE] = if (!protect) inGEOpt0 else inGEOpt0.map { inGE0 =>
                    ParamRanges.map.get(spec.name).fold(inGE0) { pInfo =>
                      pInfo.params.get(arg.name).fold(inGE0) { pSpec =>
                        val inInfoOpt = ParamRanges.map.get(graphElemName(inGE0))

                        lazy val inLoOpt: Option[Double] = inGE0 match {
                          case Constant(f) => Some(f.toDouble)
                          case _ => inInfoOpt.flatMap(_.outLo)
                        }
                        lazy val inHiOpt: Option[Double] = inGE0 match {
                          case Constant(f) => Some(f.toDouble)
                          case _ => inInfoOpt.flatMap(_.outHi)
                        }
                        val loThresh  = pSpec.lo.fold(Double.NegativeInfinity)(_.value)
                        val hiThresh  = pSpec.hi.fold(Double.PositiveInfinity)(_.value)
                        val loOk      = inLoOpt.exists(_ >= loThresh)
                        val hiOk      = inHiOpt.exists(_ <= hiThresh)

                        val inGE2: GE = if (loOk && hiOk && (!pSpec.dynamic || isDynamic(inGE0))) {
                          inGE0
                        } else if (expandProtect) {
                          Protect.expand(inGE0, lo = loThresh, hi = hiThresh, dynamic = pSpec.dynamic)
                        } else {
                          Protect.apply (inGE0, lo = loThresh, hi = hiThresh, dynamic = pSpec.dynamic)
                        }

                        inGE2
                      }
                    }
                  }

                  val inGE = inGEOpt.getOrElse {
                    val rate = spec.rates.set.max
                    val xOpt = arg.defaults.get(rate).orElse(arg.defaults.get(UndefinedRate))
                    val x    = xOpt.getOrElse {
                      val inc = Chromosome.findIncompleteUGenInputs(c, u)
//                      println("INCOMPLETE:")
//                      inc.foreach(println)
//                      println(c)
                      throw Incomplete(c, u, arg.name, inc)
                    }
                    x match {
                      case UGenSpec.ArgumentValue.Boolean(v)    => ugen.Constant(if (v) 1 else 0)
                      case UGenSpec.ArgumentValue.DoneAction(v) => ugen.Constant(v.id)
                      case UGenSpec.ArgumentValue.Float(v)      => ugen.Constant(v)
                      case UGenSpec.ArgumentValue.Inf           => ugen.Constant(Float.PositiveInfinity)
                      case UGenSpec.ArgumentValue.Int(v)        => ugen.Constant(v)
                      case UGenSpec.ArgumentValue.Nyquist       => ugen.Nyquist()
                      case UGenSpec.ArgumentValue.String(v)     => UGenSource.stringArg(v)
                    }
                  }
                  (inGE, classOf[GE])
              }
              res
            }
            // now solve `lessThan`
            val ins1 = ParamRanges.map.get(spec.name).fold(ins) { info =>
              val opt = info.params.collectFirst {
                case (param, spec0) if spec0.lessThan.isDefined => (param, spec0.lessThan.get)
              }
              opt.fold(ins) { case (param, ref) =>
                val paramIdx  = spec.args.indexWhere(_.name == param)
                val refIdx    = spec.args.indexWhere(_.name == ref  )
                val paramIn   = ins(paramIdx)
                val refIn     = ins(refIdx)

                (paramIn._1, refIn._1) match {
                  case (Constant(paramC), Constant(refC)) =>
                    if (paramC <= refC) ins else ins.updated(paramIdx, refIn)
                  // case (ge, Constant(refC)) if findOutHi(ge) <= refC => ins  // too much effort, drop it
                  case (paramGE: GE, refGE: GE) =>
                    ins.updated(paramIdx, (paramGE min refGE, classOf[GE]))
                  case _ => ins
                }
              }
            }

            u.instantiate(ins1)
        }

        loop(init, real + (last -> value))

      case _ =>  real
    }

    SynthGraph {
      import de.sciss.synth.ugen._
      if (specialIO) {
        if (expandIO) NegatumIn.expand()
        else          NegatumIn.apply()
      }
      val vertices    = c.vertices
      val verticesRev = vertices.reverseIterator.toList
      val map         = loop(verticesRev, Map.empty)
      val ugens       = vertices.collect {
        case ugen: Vertex.UGen => ugen
      }
      if (ugens.nonEmpty) {
        val roots = getGraphRoots(c)
        val sig0: GE = if (roots.isEmpty) map(ugens.head /* choose(ugens) */) else Mix(roots.map(map.apply))
        if (specialIO) {
          if (expandIO) NegatumOut.expand(sig0)
          else          NegatumOut.apply (sig0)
        } else {
          val sig1  = /* if (mono) */ Mix.mono(sig0) /* else sig0 */
          val sig2  = if (!removeNaNs) sig1 else {
            val isOk = CheckBadValues.ar(sig1, post = 0) sig_== 0
            Gate.ar(sig1, isOk)
          }
          val sig3  = Limiter.ar(LeakDC.ar(sig2))
          val sig   = if (mono) sig3 else Pan2.ar(sig3) // SplayAz.ar(numChannels = 2, in = sig3)
          Out.ar(0, sig)
        }
      }
    }
  }
}
