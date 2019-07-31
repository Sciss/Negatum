/*
 *  NegatumOut.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package ugen

//import de.sciss.synth.Ops.stringToControl
import de.sciss.synth.proc.graph.Ops.stringToControl

object NegatumOut {
  /** If `true` (default), adds a `clip2(1.0)` to the input signal */
  var CLIP      = true
  /** If `true` (default), wraps the input signal in a `LeakDC` UGen */
  val LEAK_DC   = true
  /** If `true`, wraps the input signal in a `Normalize` UGen */
  var NORMALIZE = false
  /** If `true`, wraps the output signal in a `Pan2` UGen */
  var PAN2      = false
  /** If `true`, wraps the input signal in a `HPF` UGen */
  var HIGH_PASS = false
  /** If `true`, wraps the input signal in a `Limiter` UGen */
  var LIMITER   = false
  /** If `true` (default), adds an amplitude control named `"gain"` */
  var AMP       = true
  /** If `true`, adds a short fade-in to avoid initial clicks */
  var FADE_IN   = false

  def expand(in: GE): Unit = {
    val sig0  = Mix.mono(in)
    val isOk  = CheckBadValues.ar(sig0, post = 0) sig_== 0
    val sig1  = Gate.ar(sig0, isOk)
    val sig2  = if (!CLIP     ) sig1 else sig1.clip2(1f)
    val sig3  = if (!LEAK_DC  ) sig2 else LeakDC.ar(sig2) * 0.47
    val sig4  = if (!LIMITER  ) sig3 else Limiter.ar(sig3, -0.2.dbAmp)
    val sig5  = if (!HIGH_PASS) sig4 else HPF.ar(sig4, 20)
    val sig6  = if (!NORMALIZE) sig5 else {
      val env = EnvGen.ar(Env.asr, gate = "gate".kr(1f), doneAction = doNothing /* freeSelf */)
      val doneEnv = Done.kr(env)
      val normDur = 2.0
      val tFree = TDelay.kr(doneEnv, normDur)
      FreeSelf.kr(tFree)
      Normalizer.ar(sig5, level = -0.2.dbAmp, dur = normDur) * DelayN.ar(env, normDur, normDur)
    }
    val bus = "out".kr(0f)
    val sig7 = if (!PAN2  ) sig6 else Pan2.ar(sig6)
    val sig8 = if (!AMP   ) sig7 else sig7 * "gain".kr(1f)
    val sig  = if (!FADE_IN) sig8 else {
      val ln0 = Line.ar(start = 0, end = 1, dur = 0.05)
      val ln  = if (!LIMITER) ln0 else DelayN.ar(ln0, 0.1, 0.1)
      sig8 * ln
    }
    Out.ar(bus, sig)
  }
}
final case class NegatumOut(in: GE) extends Lazy.Expander[Unit] {
  protected def makeUGens: Unit = NegatumOut.expand(in)
}