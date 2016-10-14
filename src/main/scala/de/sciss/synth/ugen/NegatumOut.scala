/*
 *  NegatumOut.scala
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

package de.sciss.synth
package ugen

import de.sciss.synth.Ops.stringToControl

object NegatumOut {
  var CLIP      = false
  var NORMALIZE = false
  var PAN2      = false
  var HPF       = false
  var LIMITER   = false
  var AMP       = false
  var FADE_IN   = false
}
final case class NegatumOut(in: GE) extends Lazy.Expander[Unit] {
//  protected def makeUGens: Unit = unwrap(this, in.expand.outputs)

  protected def makeUGens: Unit = {
    val sig0  = Mix.mono(in)
    val isOk  = CheckBadValues.ar(sig0, post = 0) sig_== 0
    val sig1  = Gate.ar(sig0, isOk)
    val sig2  = if (!NegatumOut.CLIP     ) sig1 else sig1.clip2(1)
    val sig3  = if (!NegatumOut.LIMITER  ) sig2 else Limiter.ar(LeakDC.ar(sig1), -0.2.dbamp)
    val sig4  = if (!NegatumOut.HPF      ) sig3 else HPF.ar(sig3, 20)
    val sig5  = if (!NegatumOut.NORMALIZE) sig4 else {
      val env = EnvGen.ar(Env.asr, gate = "gate".kr(1f), doneAction = doNothing /* freeSelf */)
      val doneEnv = Done.kr(env)
      val normDur = 2.0
      val tFree = TDelay.kr(doneEnv, normDur)
      FreeSelf.kr(tFree)
      Normalizer.ar(sig4, level = -0.2.dbamp, dur = normDur) * DelayN.ar(env, normDur, normDur)
    }
    val bus = "out".kr(0f)
    val sig6 = if (!NegatumOut.PAN2  ) sig5 else Pan2.ar(sig5)
    val sig7 = if (!NegatumOut.AMP   ) sig6 else sig6 * "amp".kr(1f)
    val sig  = if (!NegatumOut.FADE_IN) sig7 else {
      val ln0 = Line.ar(start = 0, end = 1, dur = 0.05)
      val ln  = if (!NegatumOut.LIMITER) ln0 else DelayN.ar(ln0, 0.1, 0.1)
      sig7 * ln
    }
    Out.ar(bus, sig)
  }
}