/*
 *  ParamRanges.scala
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

package de.sciss.negatum
package impl

import de.sciss.synth.ugen.{BinaryOpUGen, UnaryOpUGen}

import scala.language.implicitConversions

object ParamRanges {
  implicit def wrapOption[A](in: A): Option[A] = Some(in)
  implicit def wrapMargin(in: Double): Option[Margin] = Some(Margin(in))

  case class Margin(value: Double, hard: Boolean = true) {
    def soft: Boolean = !hard
  }

  // N.B. `lessThan` = technically less-than-or-equal
  case class Spec(lo      : Option[Margin]  = None,
                  hi      : Option[Margin]  = None,
                  dynamic : Boolean         = false,
                  lessThan: Option[String]  = None,
                  scalar  : Boolean         = false
                 )

  case class Info(params  : Map[String, Spec] = Map.empty,
                  outLo   : Option[Double]    = None,
                  outHi   : Option[Double]    = None,
                  dynamic : Option[Dynamic]   = None
                 )

  sealed trait Dynamic
  object Dynamic {
    case object Always extends Dynamic
    case class IfOver (param: String, lo: Double = 10.0) extends Dynamic
    case class IfUnder(param: String, hi: Double =  0.1) extends Dynamic
    case class And(elems: Dynamic*) extends Dynamic
    case class Or (elems: Dynamic*) extends Dynamic
    case class In (param: String  ) extends Dynamic
  }

  implicit def wrapDynamic(in: Boolean): Option[Dynamic] = if (in) Some(Dynamic.Always) else None
  def ifOver (param: String, lo: Double = 10.0): Option[Dynamic] = Some(Dynamic.IfOver (param, lo))
  def ifUnder(param: String, hi: Double =  0.1): Option[Dynamic] = Some(Dynamic.IfUnder(param, hi))

  val unDyn     : Dynamic = Dynamic.In("a")
  val binOrDyn  : Dynamic = Dynamic.Or (Dynamic.In("a"), Dynamic.In("b"))
  val binAndDyn : Dynamic = Dynamic.And(Dynamic.In("a"), Dynamic.In("b"))

  val map: Map[String, Info] = Map(
    // ---- Chaos ----
    "CuspN" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "CuspL" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    // FBSineN, FBSineL, FBSineC,
    "GbmanN" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    "GbmanL" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    "HenonN" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "x0" -> Spec(scalar = true),
      "x1" -> Spec(scalar = true)
    )),
    "HenonL" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "x0" -> Spec(scalar = true),
      "x1" -> Spec(scalar = true)
    )),
    "HenonC" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "x0" -> Spec(scalar = true),
      "x1" -> Spec(scalar = true)
    )),
    "LatoocarfianN" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "a" -> Spec(lo = -3.0, hi = +3.0),
      "b" -> Spec(lo = 0.5, hi = 1.5),
      "c" -> Spec(lo = 0.5, hi = 1.5),
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    "LatoocarfianL" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "a" -> Spec(lo = -3.0, hi = +3.0),
      "b" -> Spec(lo = 0.5, hi = 1.5),
      "c" -> Spec(lo = 0.5, hi = 1.5),
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    "LatoocarfianC" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "a" -> Spec(lo = -3.0, hi = +3.0),
      "b" -> Spec(lo = 0.5, hi = 1.5),
      "c" -> Spec(lo = 0.5, hi = 1.5),
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    "LinCongN" -> Info(dynamic = ifOver("freq"), /* outLo = -1.0, outHi = 1.0, */ params = Map(  // outLo/Hi is a LIE
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "LinCongL" -> Info(dynamic = ifOver("freq"), /* outLo = -1.0, outHi = 1.0, */ params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "LinCongC" -> Info(dynamic = ifOver("freq"), /* outLo = -1.0, outHi = 1.0, */ params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "LorenzL" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "h" -> Spec(lo = 0.0, hi = 0.06), // XXX TODO -- max h ?
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true),
      "zi" -> Spec(scalar = true)
    )),
    "QuadN" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "QuadL" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "QuadC" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "StandardN" -> Info(dynamic = ifOver("freq"), params = Map(  // XXX TODO -- %2pi is a lie, with xi = 24 you get an initial value beyond that
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    "StandardL" -> Info(dynamic = ifOver("freq"), params = Map(  // XXX TODO -- %2pi is a lie, with xi = 24 you get an initial value beyond that
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    // ---- Delay ----
    // ControlRate, SampleRate, SampleDur, ControlDur, SubsampleOffset, RadiansPerSample,
    // NumInputBuses, NumOutputBuses, NumAudioBuses, NumControlBuses,
    // NumBuffers, NumRunningSynths,
    // BufSampleRate, BufRateScale, BufSamples, BufFrames, BufChannels, BufDur,
    // PlayBuf, RecordBuf, BufRd, BufWr, Pitch,
    // BufDelayN, BufDelayL, BufDelayC, BufCombN, BufCombL, BufCombC,
    // BufAllpassN, BufAllpassL, BufAllpassC
    "DelayN" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
    )),
    "DelayL" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
    )),
    "DelayC" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
    )),
    "CombN" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "CombL" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "CombC" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "AllpassN" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "AllpassL" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "AllpassC" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "PitchShift" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "winSize"         -> Spec(lo = 0.001, hi = 2.0, scalar = true),  // arbitrary hi, !! lo > 0 !!
      "pitchRatio"      -> Spec(lo = 0.0, hi = 4.0),
      "pitchDispersion" -> Spec(lo = 0.0, hi = 1.0), // XXX TODO - hi ?
      "timeDispersion"  -> Spec(lo = 0.0, lessThan = "winSize")
    )),
    // TGrains, ScopeOut, ScopeOut2, Pluck, DelTapWr, DelTapRd, SetBuf, ClearBuf
    // ---- Demand ----
    // ---- DiskIO ----
    // ---- DynNoise ----
    "LFDNoise0" -> Info(dynamic = ifOver("freq"), outLo = -1.0, outHi = +1.0, params = Map(
      "freq" -> Spec()
    )),
    "LFDNoise1" -> Info(dynamic = ifOver("freq"), outLo = -1.0, outHi = +1.0, params = Map(
      "freq" -> Spec()
    )),
    "LFDNoise3" -> Info(dynamic = ifOver("freq"), outLo = -1.0, outHi = +1.0, params = Map(
      "freq" -> Spec()
    )),
    "LFDClipNoise" -> Info(dynamic = ifOver("freq"), outLo = -1.0, outHi = +1.0, params = Map(
      "freq" -> Spec()
    )),
    // ---- FFT2 ----
    "RunningSum" -> Info(params = Map(
      "length" -> Spec(lo = 1.0, hi = 44100.0, scalar = true) // arbitrary hi, !!!! lo !!!!
    )),
    // ---- FFT ----
    // ---- Filter ----
    "Ramp" -> Info(dynamic = ifUnder("dur"), params = Map(
      "in" -> Spec(dynamic = true),
      "dur" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "Lag" -> Info(dynamic = ifUnder("time", 1.0), params = Map(
      "in" -> Spec(dynamic = true),
      "time" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "Lag2" -> Info(dynamic = ifUnder("time", 1.0), params = Map(
      "in" -> Spec(dynamic = true),
      "time" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "Lag3" -> Info(dynamic = ifUnder("time", 1.0), params = Map(
      "in" -> Spec(dynamic = true),
      "time" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "LagUD" -> Info(dynamic = Dynamic.And(Dynamic.IfUnder("timeUp", 1.0), Dynamic.IfUnder("timeUp", 1.0)), params = Map(
      "in" -> Spec(dynamic = true),
      "timeUp" -> Spec(lo = 0.0, hi = 30.0), // arbitrary hi
      "timeDown" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "Lag2UD" -> Info(dynamic = Dynamic.And(Dynamic.IfUnder("timeUp", 1.0), Dynamic.IfUnder("timeUp", 1.0)), params = Map(
      "in" -> Spec(dynamic = true),
      "timeUp" -> Spec(lo = 0.0, hi = 30.0), // arbitrary hi
      "timeDown" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "Lag3UD" -> Info(dynamic = Dynamic.And(Dynamic.IfUnder("timeUp", 1.0), Dynamic.IfUnder("timeUp", 1.0)), params = Map(
      "in" -> Spec(dynamic = true),
      "timeUp" -> Spec(lo = 0.0, hi = 30.0), // arbitrary hi
      "timeDown" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "OnePole" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true),
      "coeff" -> Spec(lo = -0.999, hi = +0.999)
    )),
    "OneZero" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true),
      "coeff" -> Spec(lo = -1.0, hi = +1.0)
    )),
    "TwoPole" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true),
      "freq" -> Spec(lo = 10.0, hi = 20000.0), // arbitrary
      "radius" -> Spec(lo = 0.0, hi = 1.0)
    )),
    "TwoZero" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true),
      "freq" -> Spec(lo = 10.0, hi = 20000.0), // arbitrary
      "radius" -> Spec(lo = 0.0, hi = 1.0)
    )),
    "Decay" -> Info(dynamic = ifUnder("time", 1.0), params = Map(
      "in" -> Spec(dynamic = true),
      "time" -> Spec(lo = 0.0, hi = 30.0) // hi arbitrary
    )),
    "Decay2" -> Info(dynamic = Dynamic.And(Dynamic.IfUnder("attack", 1.0), Dynamic.IfUnder("release", 1.0)), params = Map(
      "in" -> Spec(dynamic = true),
      "attack" -> Spec(lo = 0.0, hi = 30.0), // hi arbitrary
      "release" -> Spec(lo = 0.0, hi = 30.0)  // hi arbitrary
    )),
    "Delay1" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "Delay2" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "Integrator" -> Info(dynamic = true /* XXX */, params = Map(
      "in" -> Spec(dynamic = true),
      "coeff" -> Spec(lo = -0.999, hi = +0.999)
    )),
    "LeakDC" -> Info(dynamic = true, params = Map(
      "coeff" -> Spec(lo = 0.8, hi = +0.99) // arbitrary
    )),
    "LPZ1" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "HPZ1" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "LPZ2" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "HPZ2" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "BPZ2" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "BRZ2" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    // APF
    "LPF" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true),
      "freq" -> Spec(lo = 10.0, hi = 20000.0)
    )),
    "HPF" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true),
      "freq" -> Spec(lo = 10.0, hi = 20000.0)
    )),
    "BPF" -> Info(dynamic = true, params = Map(
      "in"    -> Spec(dynamic = true),
      "freq"  -> Spec(lo = 10.0, hi = 20000.0),
      "rq"    -> Spec(lo = 0.01, hi = 100.0)    // arbitrary
    )),
    "BRF" -> Info(dynamic = true, params = Map(
      "in"    -> Spec(dynamic = true),
      "freq"  -> Spec(lo = 10.0, hi = 20000.0),
      "rq"    -> Spec(lo = 0.01, hi = 100.0)    // arbitrary
    )),
    "RLPF" -> Info(dynamic = true, params = Map(
      "in"    -> Spec(dynamic = true),
      "freq"  -> Spec(lo = 10.0, hi = 20000.0),
      "rq"    -> Spec(lo = 0.01, hi = 100.0)    // arbitrary
    )),
    "RHPF" -> Info(dynamic = true, params = Map(
      "in"    -> Spec(dynamic = true),
      "freq"  -> Spec(lo = 10.0, hi = 20000.0),
      "rq"    -> Spec(lo = 0.01, hi = 100.0)    // arbitrary
    )),
    "MidEQ" -> Info(dynamic = true, params = Map(
      "in"    -> Spec(dynamic = true),
      "freq"  -> Spec(lo = 10.0, hi = 20000.0),
      "rq"    -> Spec(lo = 0.01, hi = 100.0),     // arbitrary
      "gain"  -> Spec(lo = -144.0, hi = 144.0)    // arbitrary
    )),
    "Resonz" -> Info(dynamic = true, params = Map(
      "in"    -> Spec(dynamic = true),
      "freq"  -> Spec(lo = 10.0, hi = 20000.0),
      "rq"    -> Spec(lo = 0.01, hi = 100.0)      // arbitrary
    )),
    "Formlet" -> Info(dynamic = true, params = Map(
      "in"    -> Spec(dynamic = true),
      "freq"  -> Spec(lo = 10.0, hi = 20000.0),
      "attack"-> Spec(lo = 0.001, hi = 100.0),     // arbitrary
      "decay" -> Spec(lo = 0.001, hi = 100.0)     // arbitrary
    )),
    "Slew" -> Info(params = Map(
      "in"    -> Spec(),
      "up"    -> Spec(lo = 0.001, hi = 40000.0),
      "down"  -> Spec(lo = 0.001, hi = 40000.0)
    )),
    "Median" -> Info(params = Map(
      "in"      -> Spec(),
      "length"  -> Spec(lo = 3.0, hi = 31.0)
    )),
    "FreqShift" -> Info(dynamic = true, params = Map(
      "in"      -> Spec(dynamic = true),
      "freq"    -> Spec(lo = -20000.0, hi = 20000.0),
      "phase"   -> Spec()
    )),

    // ---- Filter ---- TODO
    // Ringz (mostly identical to Resonz though)
    // FOS, SOS, Compander, Limiter, Normalizer, Amplitude, DetectSilence, Hilbert,
    // MoogFF, BLowPass, BHiPass, BBandPass, BBandStop, BPeakEQ, BAllPass, BLowShelf, BHiShelf

    // ---- Gendyn ---- TODO
    // ---- Grain ----
    // ---- IO ----
    // ---- Keyboard ----

    // ---- LF ----
    //  Vibrato
    "LFPulse" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"  -> Spec(lo = 0.01, hi = 20000.0),
      "iphase" -> Spec(lo = 0.0, hi = 1.0, scalar = true),
      "width" -> Spec(lo = 0.0, hi = 1.0)
    )),
    "LFSaw" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"  -> Spec(lo = 0.01, hi = 20000.0),
      "iphase" -> Spec(lo = -1.0, hi = 1.0, scalar = true),
      "width" -> Spec(lo = 0.0, hi = 1.0)
    )),
    "LFPar" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"  -> Spec(lo = 0.01, hi = 20000.0),
      "iphase" -> Spec(lo = 0.0, hi = 1.0, scalar = true)
    )),
    "LFCub" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"  -> Spec(lo = 0.01, hi = 20000.0),
      "iphase" -> Spec(lo = 0.0, hi = 1.0, scalar = true)
    )),
    "LFTri" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"  -> Spec(lo = 0.01, hi = 20000.0),
      "iphase" -> Spec(lo = 0.0, hi = 4.0, scalar = true)
    )),
    "LFGauss" -> Info(dynamic = ifUnder("dur", 0.1), params = Map(
      "dur"     -> Spec(lo = 5.0e-5, hi = 100.0),
      "width"   -> Spec(lo = 0.0, hi = 1.0),
      "phase"   -> Spec(lo = 0.0, hi = 1.0)  // XXX TODO -- hi correct?
    )),
    "Impulse" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"    -> Spec(lo = 0.1, hi = 20000.0),
      "phase"   -> Spec(lo = 0.0, hi = 1.0)
    )),
    "VarSaw" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"    -> Spec(lo = 0.01, hi = 20000.0),
      "iphase"  -> Spec(lo = 0.0, hi = 1.0, scalar = true),
      "width"   -> Spec(lo = 0.0, hi = 1.0)
    )),
    "SyncSaw" -> Info(dynamic = Dynamic.Or(Dynamic.IfOver("syncFreq"), Dynamic.IfOver("sawFreq")), params = Map(
      "syncFreq"  -> Spec(lo = 0.01, hi = 20000.0),
      "sawFreq"   -> Spec(lo = 0.01, hi = 20000.0)
    )),
    // K2A, A2K, T2K, T2A, DC

    "Line" -> Info(dynamic = true, params = Map(
      "start" -> Spec(),
      "end"   -> Spec(),
      "dur"   -> Spec(lo = 0.001, hi = 2000.0)
    )),

    // ---- LF ---- TODO
    // XLine, Wrap, Fold, Clip
    // AmpComp, AmpCompA
    // InRange, InRect, LinExp, EnvGen, Linen, IEnvGen

    s"Bin_${BinaryOpUGen.Plus     .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Minus    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Times    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Mod      .id}" -> Info(dynamic = binAndDyn),
    s"Bin_${BinaryOpUGen.Eq       .id}" -> Info(outLo = 0.0, outHi = 1.0),
    s"Bin_${BinaryOpUGen.Neq      .id}" -> Info(outLo = 0.0, outHi = 1.0),
    s"Bin_${BinaryOpUGen.Lt       .id}" -> Info(outLo = 0.0, outHi = 1.0),
    s"Bin_${BinaryOpUGen.Gt       .id}" -> Info(outLo = 0.0, outHi = 1.0),
    s"Bin_${BinaryOpUGen.Leq      .id}" -> Info(outLo = 0.0, outHi = 1.0),
    s"Bin_${BinaryOpUGen.Geq      .id}" -> Info(outLo = 0.0, outHi = 1.0),
    s"Bin_${BinaryOpUGen.Min      .id}" -> Info(dynamic = binAndDyn),
    s"Bin_${BinaryOpUGen.Max      .id}" -> Info(dynamic = binAndDyn),
    s"Bin_${BinaryOpUGen.BitAnd   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.BitOr    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.BitXor   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.RoundTo  .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.RoundUpTo.id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Trunc    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Atan2    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Hypot    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Hypotx   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Pow      .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Ring1    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Ring2    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Ring3    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Ring4    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Difsqr   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Sumsqr   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Sqrsum   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Sqrdif   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Absdif   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Thresh   .id}" -> Info(dynamic = binAndDyn),
    s"Bin_${BinaryOpUGen.Amclip   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Scaleneg .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Clip2    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Excess   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Fold2    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Wrap2    .id}" -> Info(dynamic = binOrDyn),
    // Div,

    s"Un_${UnaryOpUGen.Neg        .id}" -> Info(dynamic = unDyn),
    s"Un_${UnaryOpUGen.Not        .id}" -> Info(dynamic = unDyn, outLo = 0.0, outHi = 1.0),
    s"Un_${UnaryOpUGen.Abs        .id}" -> Info(dynamic = unDyn),
    s"Un_${UnaryOpUGen.Frac       .id}" -> Info(dynamic = unDyn, outLo = 0.0, outHi = 1.0),
    s"Un_${UnaryOpUGen.Signum     .id}" -> Info(dynamic = unDyn, outLo = -1.0, outHi = 1.0),
    s"Un_${UnaryOpUGen.Squared    .id}" -> Info(dynamic = unDyn),
    s"Un_${UnaryOpUGen.Cubed      .id}" -> Info(dynamic = unDyn),
    s"Un_${UnaryOpUGen.Sqrt       .id}" -> Info(dynamic = unDyn),
    s"Un_${UnaryOpUGen.Exp        .id}" -> Info(dynamic = unDyn),
    s"Un_${UnaryOpUGen.Midicps    .id}" -> Info(dynamic = unDyn),
    s"Un_${UnaryOpUGen.Cpsmidi    .id}" -> Info(dynamic = unDyn),
    s"Un_${UnaryOpUGen.Log        .id}" -> Info(dynamic = unDyn),
    s"Un_${UnaryOpUGen.Sin        .id}" -> Info(dynamic = unDyn, outLo = -1.0, outHi = 1.0),
    s"Un_${UnaryOpUGen.Cos        .id}" -> Info(dynamic = unDyn, outLo = -1.0, outHi = 1.0),
    s"Un_${UnaryOpUGen.Tan        .id}" -> Info(dynamic = unDyn, outLo = -math.Pi, outHi = math.Pi),
    s"Un_${UnaryOpUGen.Sinh       .id}" -> Info(dynamic = unDyn),
    s"Un_${UnaryOpUGen.Cosh       .id}" -> Info(dynamic = unDyn),
    s"Un_${UnaryOpUGen.Tanh       .id}" -> Info(dynamic = unDyn),
    s"Un_${UnaryOpUGen.Distort    .id}" -> Info(dynamic = unDyn, outLo = -1.0, outHi = 1.0),
    s"Un_${UnaryOpUGen.Softclip   .id}" -> Info(dynamic = unDyn, outLo = -1.0, outHi = 1.0),
    s"Un_${UnaryOpUGen.HannWindow .id}" -> Info(dynamic = unDyn, outLo = 0.0, outHi = 1.0),
    s"Un_${UnaryOpUGen.TriWindow  .id}" -> Info(dynamic = unDyn, outLo = 0.0, outHi = 1.0),
    s"Un_${UnaryOpUGen.WelchWindow.id}" -> Info(dynamic = unDyn, outLo = 0.0, outHi = 1.0),
    s"Un_${UnaryOpUGen.Ramp       .id}" -> Info(dynamic = unDyn, outLo = 0.0, outHi = 1.0),
    s"Un_${UnaryOpUGen.Scurve     .id}" -> Info(dynamic = unDyn, outLo = 0.0, outHi = 1.0),

    // ---- OSC ----
    // DegreeToKey, Select, TWindex, Index, IndexL, FoldIndex, WrapIndex,
    // IndexInBetween, DetectIndex, Shaper,
    // FSinOsc, SinOscFB, VOsc, VOsc3, Osc, OscN, COsc,
    // Klang, Klank

    // XXX TODO: Blip, Saw

    "SinOsc" -> Info(dynamic = ifOver("freq"), outLo = -1.0, outHi = +1.0, params = Map(
      "freq"  -> Spec(lo = 0.01, hi = 20000.0),
      "phase" -> Spec()  // wraps around 2pi
    )),
    //    "Formant" -> Info(dynamic = true, params = Map(
    //      "fundFreq"  -> Spec(lo = 10.0, hi = 18000.0), // XXX TODO -- not sure this accepts very low frequencies
    //      "formFreq"  -> Spec(lo = 10.0, hi = 18000.0),
    //      "bw"        -> Spec(lo = 10.0, hi =  4000.0, greaterThan = "fundFreq")
    //    )),

    "Blip" -> Info(dynamic = true, outLo = -1.0, outHi = +1.0, params = Map(
      "freq"    -> Spec(lo = 10.0, hi = 20000.0),
      "numHarm" -> Spec(lo = 1.0)
    )),
    "Saw" -> Info(dynamic = true, params = Map( // Unfortunately can seriously exceed outLo = -1.0, outHi = +1.0
      "freq"    -> Spec(lo = 10.0, hi = 20000.0)
    )),
    "Pulse" -> Info(dynamic = true, params = Map( // Unfortunately can seriously exceed outLo = -1.0, outHi = +1.0
      "freq"  -> Spec(lo = 10.0, hi = 20000.0),
      "width" -> Spec(lo = 0.0, hi = 1.0)
    )),

    "GVerb" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "roomSize"      -> Spec(lo = 0.55, lessThan = "maxRoomSize"),  // lo!
      "revTime"       -> Spec(lo = 0.0, hi = 100.0 /* soft */),
      "damping"       -> Spec(lo = 0.0, hi = 1.0),
      "inputBW"       -> Spec(lo = 0.0, hi = 1.0),
      "spread"        -> Spec(lo = 0.0, hi = 43.0), // hi!
      // "dryLevel"      -> Spec(),
      // "earlyRefLevel" -> Spec(),
      // "tailLevel"     -> Spec(),
      "maxRoomSize"   -> Spec(lo = 0.55, hi = 300.0 /* soft */, scalar = true)
    ))
  )
}