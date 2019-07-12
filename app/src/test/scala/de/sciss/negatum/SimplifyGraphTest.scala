package de.sciss.negatum

import de.sciss.lucre.synth
import de.sciss.lucre.synth.{InMemory, Synth, Txn}
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.AuralSystem

object SimplifyGraphTest {
  def graphOrig1: SynthGraph = SynthGraph {
    import de.sciss.synth._
    import de.sciss.synth.ugen._
    // source code automatically extracted

    NegatumIn()
    val linCongC        = LinCongC.ar(freq = Nyquist() /* could not parse! */, a = 9.4327135,
      c = 0.6464241, m = 9.4327135, xi = 0.6464241)
    val a_0             = linCongC min 0.6464241
    val min_0           = a_0 min 0.44575435
    val in_0            = Protect(min_0, -inf, inf, true)
    val time            = Protect(linCongC, 0.0, 30.0, false)
    val lag             = Lag.ar(in_0, time = time)
    val min_1           = linCongC min lag
    val xi_0            = Protect(min_0, -inf, inf, false)
    val b_0             = StandardN.ar(freq = Nyquist() /* could not parse! */, k = 0.44575435, xi = xi_0,
      yi = 0.0)
    val freq_0          = Protect(b_0, -inf, inf, false)
    val henonL_0        = HenonL.ar(freq = freq_0, a = 19.648922, b = b_0, x0 = 0.6464241, x1 = 0.6464241)
    val freq_1          = Protect(henonL_0, -inf, inf, false)
    val henonL_1        = HenonL.ar(freq = freq_1, a = 44.110653, b = 0.18760166, x0 = 0.44575435,
      x1 = 0.6464241)
    val in_1            = Protect(henonL_1, -inf, inf, true)
    val lPZ2            = LPZ2.ar(in_1)
    val b_1             = 0.0 min henonL_1
    val min_2           = b_1 min 0.44575435
    val in_2            = min_2 min 9.4327135
    val coeff_0         = Protect(min_1, 0.8, 0.99, false)
    val a_1             = LeakDC.ar(in_2, coeff = coeff_0)
    val freq_2          = Protect(min_2, -inf, inf, false)
    val x1_0            = Protect(a_0, -inf, inf, false)
    val b_2             = HenonN.ar(freq = freq_2, a = a_0, b = b_1, x0 = 44.110653, x1 = x1_0)
    val in_3            = Protect(0.021992043, -inf, inf, true)
    val hPZ1            = HPZ1.ar(in_3)
    val min_3           = b_2 min hPZ1
    val freq_3          = Protect(lPZ2, -inf, inf, false)
    val a_2             = Protect(min_3, -3.0, 3.0, false)
    val b_3             = Protect(0.44575435, 0.5, 1.5, false)
    val c_0             = Protect(9.4327135, 0.5, 1.5, false)
    val latoocarfianC   = LatoocarfianC.ar(freq = freq_3, a = a_2, b = b_3, c = c_0, d = 9.4327135,
      xi = 0.0, yi = 9.4327135)
    val ring4           = 44.110653 ring4 b_0
    val in_4            = Protect(201.25365, -inf, inf, true)
    val attack_0        = Protect(ring4, 0.0, 30.0, false)
    val release_0       = Protect(44.110653, 0.0, 30.0, false)
    val decay2_0        = Decay2.ar(in_4, attack = attack_0, release = release_0)
    val min_4           = decay2_0 min latoocarfianC
    val min_5           = min_4 min 15.73315
    val min_6           = min_5 min 9.4327135
    val b_4             = min_6 min min_4
    val min_7           = min_6 min b_4
    val in_5            = Protect(b_0, -inf, inf, true)
    val pitchDispersion = Protect(linCongC, 0.0, 1.0, false)
    val pitchShift      = PitchShift.ar(in_5, winSize = 0.18760166, pitchRatio = 0.44575435,
      pitchDispersion = pitchDispersion, timeDispersion = 0.18760166)
    val amclip          = pitchShift amClip min_7
    val b_5             = amclip min 0.6464241
    val in_6            = Protect(9.4327135, -inf, inf, true)
    val coeff_1         = Protect(min_7, -0.999, 0.999, false)
    val onePole         = OnePole.ar(in_6, coeff = coeff_1)
    val in_7            = Protect(onePole, -inf, inf, true)
    val ramp_0          = Ramp.ar(in_7, dur = 0.1)
    val min_8           = a_1 min ramp_0
    val min_9           = min_4 min min_8
    val min_10          = min_9 min 9.4327135
    val min_11          = min_10 min linCongC
    val in_8            = Protect(min_11, -inf, inf, true)
    val freq_4          = Protect(min_6, 10.0, 20000.0, false)
    val a_3             = HPF.ar(in_8, freq = freq_4)
    val freq_5          = Protect(a_3, 0.01, 20000.0, false)
    val iphase          = Protect(9.4327135, 0.0, 1.0, false)
    val c_1             = LFPar.ar(freq = freq_5, iphase = iphase)
    val freq_6          = Protect(a_3, -inf, inf, false)
    val quadN           = QuadN.ar(freq = freq_6, a = a_3, b = b_4, c = c_1, xi = 0.0)
    val x0_0            = Protect(c_1, -inf, inf, false)
    val x1_1            = Protect(ramp_0, -inf, inf, false)
    val henonN          = HenonN.ar(freq = 15.73315, a = 0.0, b = b_5, x0 = x0_0, x1 = x1_1)
    val min_12          = min_5 min 15.73315
    val min_13          = min_12 min 9.4327135
    val min_14          = min_3 min min_13
    val min_15          = min_14 min 9.4327135
    val min_16          = c_1 min min_10
    val min_17          = a_3 min min_6
    val min_18          = min_17 min min_16
    val min_19          = min_18 min min_17
    val in_9            = Protect(min_19, -inf, inf, true)
    val hPZ2            = HPZ2.ar(in_9)
    val in_10           = Protect(min_18, -inf, inf, true)
    val bPZ2            = BPZ2.ar(in_10)
    val min_20          = min_18 min bPZ2
    val sqrdif          = min_17 sqrDif min_20
    val min_21          = hPZ2 min sqrdif
    val tailLevel       = min_20 min sqrdif
    val min_22          = bPZ2 min min_15
    val min_23          = min_22 min min_6
    val min_24          = min_23 min c_1
    val min_25          = min_24 min min_14
    val min_26          = min_25 min tailLevel
    val min_27          = min_15 min henonN
    val min_28          = b_5 min ramp_0
    val min_29          = b_1 min min_2
    val min_30          = min_29 min min_28
    val in_11           = Protect(min_30, -inf, inf, true)
    val dur_0           = Protect(min_27, 0.0, 30.0, false)
    val ramp_1          = Ramp.ar(in_11, dur = dur_0)
    val min_31          = ramp_1 min 9.4327135
    val gt              = ramp_1 > min_8
    val plus            = latoocarfianC + gt
    val in_12           = Protect(min_24, -inf, inf, true)
    val attack_1        = Protect(henonN, 0.0, 30.0, false)
    val release_1       = Protect(amclip, 0.0, 30.0, false)
    val decay2_1        = Decay2.ar(in_12, attack = attack_1, release = release_1)
    val min_32          = min_26 min plus
    val min_33          = 9.4327135 min min_32
    val min_34          = decay2_1 min min_33
    val freq_7          = Protect(min_4, -inf, inf, false)
    val xi_1            = Protect(min_34, -inf, inf, false)
    val quadC           = QuadC.ar(freq = freq_7, a = a_1, b = b_2, c = -0.75, xi = xi_1)
    val dryLevel        = ramp_1 min bPZ2
    val min_35          = 10.0 min min_23
    val min_36          = min_35 min min_23
    val min_37          = min_36 min min_23
    val min_38          = min_37 min min_23
    val min_39          = min_38 min min_23
    val min_40          = min_37 min b_5
    val min_41          = min_40 min a_3
    val in_13           = Protect(b_5, -inf, inf, true)
    val freq_8          = Protect(min_8, 10.0, 20000.0, false)
    val hPF_0           = HPF.ar(in_13, freq = freq_8)
    val earlyRefLevel   = min_33 min decay2_1
    val in_14           = Protect(min_41, -inf, inf, true)
    val protect_0       = Protect(min_39, 0.55, inf, false)
    val revTime         = Protect(min_15, 0.0, 100.0, false)
    val damping         = Protect(amclip, 0.0, 1.0, false)
    val inputBW         = Protect(lag, 0.0, 1.0, false)
    val spread          = Protect(hPF_0, 0.0, 43.0, false)
    val maxRoomSize     = Protect(min_23, 0.55, 300.0, false)
    val roomSize        = protect_0 min maxRoomSize
    val gVerb           = GVerb.ar(in_14, roomSize = roomSize, revTime = revTime, damping = damping,
      inputBW = inputBW, spread = spread, dryLevel = dryLevel,
      earlyRefLevel = earlyRefLevel, tailLevel = tailLevel, maxRoomSize = maxRoomSize)
    val min_42          = min_34 min earlyRefLevel
    val min_43          = min_42 min sqrdif
    val min_44          = min_43 min henonN
    val sqrsum          = 9.4327135 sqrSum min_43
    val min_45          = min_41 min sqrsum
    val min_46          = min_45 min min_28
    val min_47          = min_46 min min_41
    val roundTo         = min_47 roundTo quadN
    val min_48          = min_47 min min_41
    val min_49          = min_48 min min_41
    val min_50          = min_49 min min_41
    val min_51          = min_50 min min_41
    val in_15           = Protect(min_32, -inf, inf, true)
    val maxDelayTime    = Protect(min_41, 0.0, 20.0, false)
    val protect_1       = Protect(min_51, 0.0, inf, false)
    val delayTime       = protect_1 min maxDelayTime
    val delayL          = DelayL.ar(in_15, maxDelayTime = maxDelayTime, delayTime = delayTime)
    val min_52          = min_15 min min_26
    val max             = gt max min_43
    val in_16           = Protect(max, -inf, inf, true)
    val freq_9          = Protect(min_40, 10.0, 20000.0, false)
    val hPF_1           = HPF.ar(in_16, freq = freq_9)
    val min_53          = hPF_1 min delayL
    val min_54          = min_53 min min_53
    val min_55          = min_34 min max
    val in_17           = Protect(9.4327135, -inf, inf, true)
    val coeff_2         = Protect(min_28, -1.0, 1.0, false)
    val oneZero_0       = OneZero.ar(in_17, coeff = coeff_2)
    val in_18           = Protect(oneZero_0, -inf, inf, true)
    val coeff_3         = Protect(min_43, -1.0, 1.0, false)
    val oneZero_1       = OneZero.ar(in_18, coeff = coeff_3)
    val freq_10         = Protect(min_7, -inf, inf, false)
    val lFDClipNoise    = LFDClipNoise.ar(freq_10)
    val length          = Protect(min_28, 1.0, 44100.0, false)
    val runningSum      = RunningSum.ar(9.4327135, length = length)
    val min_56          = lFDClipNoise min runningSum
    val min_57          = 0.18760166 min min_29
    val mix             = Mix(
      Seq[GE](min_21, min_31, quadC, gVerb, min_44, roundTo, min_52, min_54, min_55, oneZero_1, min_56, min_57))
    NegatumOut(mix)
  }

  def graphOrig2: SynthGraph = SynthGraph {
    import de.sciss.synth._
    import de.sciss.synth.ugen._
    NegatumIn()
    val quadC_0       = QuadC.ar(freq = 1006.5003, a = 7.526999, b = 0.2, c = 7.526999, xi = 7.526999)
    val in_0          = Protect(0.2, -inf, inf, true)
    val timeDown      = Protect(1006.5003, 0.0, 30.0, false)
    val lagUD         = LagUD.ar(in_0, timeUp = 0.2, timeDown = timeDown)
    val freq_0        = Protect(lagUD, -inf, inf, false)
    val lFDNoise0     = LFDNoise0.ar(freq_0)
    val c_0           = quadC_0 min lFDNoise0
    val quadC_1       = QuadC.ar(freq = 1006.5003, a = 0.2, b = 0.012651972, c = c_0, xi = 0.0)
    val gbmanL        = GbmanL.ar(freq = 0.2, xi = 0.2, yi = 0.2)
    val min_0         = gbmanL min 0.012651972
    val freq_1        = 7.526999 min min_0
    val min_1         = freq_1 min quadC_1
    val in_1          = Protect(1006.5003, -inf, inf, true)
    val coeff_0       = Protect(quadC_0, -0.999, 0.999, false)
    val a_0           = OnePole.ar(in_1, coeff = coeff_0)
    val b_0           = min_1 min lagUD
    val x0_0          = Protect(quadC_0, -inf, inf, false)
    val henonC        = HenonC.ar(freq = 0.2, a = a_0, b = b_0, x0 = x0_0, x1 = 0.2)
    val in_2          = Protect(c_0, -inf, inf, true)
    val time          = Protect(henonC, 0.0, 30.0, false)
    val lag2          = Lag2.ar(in_2, time = time)
    val min_2         = 7.526999 min lag2
    val wrap2         = freq_1 wrap2 min_1
    val d             = c_0 min 0.2
    val xi_0          = Protect(gbmanL, -inf, inf, false)
    val yi_0          = Protect(min_1, -inf, inf, false)
    val standardN     = StandardN.ar(freq = freq_1, k = 0.2, xi = xi_0, yi = yi_0)
    val min_3         = d min standardN
    val excess        = min_3 excess 0.012651972
    val min_4         = min_3 min quadC_0
    val min_5         = min_4 min lagUD
    val min_6         = min_5 min wrap2
    val a_1           = lFDNoise0 max 7.526999
    val min_7         = quadC_0 min a_1
    val min_8         = min_0 min lag2
    val min_9         = min_8 min min_7
    val in_3          = Protect(min_1, -inf, inf, true)
    val freq_2        = Protect(a_0, 10.0, 20000.0, false)
    val rq            = Protect(standardN, 0.01, 100.0, false)
    val rHPF          = RHPF.ar(in_3, freq = freq_2, rq = rq)
    val min_10        = min_8 min rHPF
    val min_11        = 1006.5003 min min_10
    val min_12        = min_7 min min_11
    val freq_3        = Protect(lagUD, -inf, inf, false)
    val a_2           = Protect(quadC_1, -3.0, 3.0, false)
    val b_1           = Protect(min_3, 0.5, 1.5, false)
    val xi_1          = Protect(min_12, -inf, inf, false)
    val yi_1          = Protect(min_7, -inf, inf, false)
    val latoocarfianC = LatoocarfianC.ar(freq = freq_3, a = a_2, b = b_1, c = 0.5, d = d, xi = xi_1,
      yi = yi_1)
    val freq_4        = Protect(latoocarfianC, -inf, inf, false)
    val x0_1          = Protect(c_0, -inf, inf, false)
    val x1_0          = Protect(henonC, -inf, inf, false)
    val henonN        = HenonN.ar(freq = freq_4, a = a_1, b = 0.3, x0 = x0_1, x1 = x1_0)
    val freq_5        = Protect(henonN, 0.01, 20000.0, false)
    val lFTri         = LFTri.ar(freq = freq_5, iphase = 0.0)
    val min_13        = min_12 min min_0
    val in_4          = Protect(lag2, -inf, inf, true)
    val coeff_1       = Protect(lag2, -0.999, 0.999, false)
    val integrator    = Integrator.ar(in_4, coeff = coeff_1)
    val min_14        = min_13 min integrator
    val min_15        = lagUD min lag2
    val trunc         = 1006.5003 trunc c_0
    val thresh        = trunc thresh min_15
    val min_16        = thresh min lag2
    val hypotx        = thresh hypotApx min_16
    val xi_2          = Protect(rHPF, -inf, inf, false)
    val gbmanN        = GbmanN.ar(freq = 7.526999, xi = xi_2, yi = 2.1)
    val in_5          = Protect(lag2, -inf, inf, true)
    val freq_6        = Protect(hypotx, 10.0, 20000.0, false)
    val radius        = Protect(gbmanN, 0.0, 1.0, false)
    val twoZero       = TwoZero.ar(in_5, freq = freq_6, radius = radius)
    val mix           = Mix(Seq[GE](min_2, excess, min_6, min_9, lFTri, min_14, twoZero))
    NegatumOut(mix)
  }

  def graphReduce3: SynthGraph = SynthGraph {
    import de.sciss.synth._
    import de.sciss.synth.ugen._

    NegatumIn()
    val linCongC        = LinCongC.ar(freq = Nyquist() /* could not parse! */, a = 9.4327135,
      c = 0.6464241, m = 9.4327135, xi = 0.6464241)
    val a_0             = linCongC min 0.6464241
    val min_0           = a_0 min 0.44575435
    val in_0            = Protect(min_0, -inf, inf, true)
    val time            = 0.0 // Protect(linCongC, 0.0, 30.0, false)
    val lag             = Lag.ar(in_0, time = time)
    val min_1           = linCongC min lag
    val xi_0            = Protect(min_0, -inf, inf, false)
    val b_0             = -0.539296 // StandardN.ar(freq = Nyquist() /* could not parse! */, k = 0.44575435, xi = xi_0, yi = 0.0)
    val freq_0          = -0.539296 // Protect(b_0, -inf, inf, false)
    val henonL_0        = HenonL.ar(freq = freq_0, a = 19.648922, b = b_0, x0 = 0.6464241, x1 = 0.6464241)
    val freq_1          = Protect(henonL_0, -inf, inf, false)
    val henonL_1        = HenonL.ar(freq = freq_1, a = 44.110653, b = 0.18760166, x0 = 0.44575435,
      x1 = 0.6464241)
    val in_1            = Protect(henonL_1, -inf, inf, true)
    val lPZ2            = LPZ2.ar(in_1)
    val b_1             = 0.0 min henonL_1
    val min_2           = b_1 min 0.44575435
    val in_2            = min_2 min 9.4327135
    val coeff_0         = Protect(min_1, 0.8, 0.99, false)
    val a_1             = LeakDC.ar(in_2, coeff = coeff_0)
    val freq_2          = Protect(min_2, -inf, inf, false)
    val x1_0            = Protect(a_0, -inf, inf, false)
    val b_2             = HenonN.ar(freq = freq_2, a = a_0, b = b_1, x0 = 44.110653, x1 = x1_0)
    val in_3            = Protect(0.021992043, -inf, inf, true)
    val hPZ1            = HPZ1.ar(in_3)
    val min_3           = b_2 min hPZ1
    val freq_3          = Protect(lPZ2, -inf, inf, false)
    val a_2             = Protect(min_3, -3.0, 3.0, false)
    val b_3             = Protect(0.44575435, 0.5, 1.5, false)
    val c_0             = Protect(9.4327135, 0.5, 1.5, false)
    val latoocarfianC   = LatoocarfianC.ar(freq = freq_3, a = a_2, b = b_3, c = c_0, d = 9.4327135,
      xi = 0.0, yi = 9.4327135)
    val ring4           = 44.110653 ring4 b_0
    val in_4            = Protect(201.25365, -inf, inf, true)
    val attack_0        = Protect(ring4, 0.0, 30.0, false)
    val release_0       = Protect(44.110653, 0.0, 30.0, false)
    val decay2_0        = Decay2.ar(in_4, attack = attack_0, release = release_0)
    val min_4           = decay2_0 min latoocarfianC
    val min_5           = min_4 min 15.73315
    val min_6           = min_5 min 9.4327135
    val b_4             = min_6 min min_4
    val min_7           = min_6 min b_4
    val in_5            = Protect(b_0, -inf, inf, true)
    val pitchDispersion = Protect(linCongC, 0.0, 1.0, false)
    val pitchShift      = PitchShift.ar(in_5, winSize = 0.18760166, pitchRatio = 0.44575435,
      pitchDispersion = pitchDispersion, timeDispersion = 0.18760166)
    val amclip          = pitchShift amClip min_7
    val b_5             = amclip min 0.6464241
    val in_6            = Protect(9.4327135, -inf, inf, true)
    val coeff_1         = Protect(min_7, -0.999, 0.999, false)
    val onePole         = OnePole.ar(in_6, coeff = coeff_1)
    val in_7            = Protect(onePole, -inf, inf, true)
    val ramp_0          = Ramp.ar(in_7, dur = 0.1)
    val min_8           = a_1 min ramp_0
    val min_9           = min_4 min min_8
    val min_10          = min_9 min 9.4327135
    val min_11          = min_10 min linCongC
    val in_8            = Protect(min_11, -inf, inf, true)
    val freq_4          = Protect(min_6, 10.0, 20000.0, false)
    val a_3             = HPF.ar(in_8, freq = freq_4)
    val freq_5          = Protect(a_3, 0.01, 20000.0, false)
    val iphase          = Protect(9.4327135, 0.0, 1.0, false)
    val c_1             = LFPar.ar(freq = freq_5, iphase = iphase)
    val freq_6          = Protect(a_3, -inf, inf, false)
    val quadN           = QuadN.ar(freq = freq_6, a = a_3, b = b_4, c = c_1, xi = 0.0)
    val x0_0            = Protect(c_1, -inf, inf, false)
    val x1_1            = Protect(ramp_0, -inf, inf, false)
    val henonN          = HenonN.ar(freq = 15.73315, a = 0.0, b = b_5, x0 = x0_0, x1 = x1_1)
    val min_12          = min_5 min 15.73315
    val min_13          = min_12 min 9.4327135
    val min_14          = min_3 min min_13
    val min_15          = min_14 min 9.4327135
    val min_16          = c_1 min min_10
    val min_17          = a_3 min min_6
    val min_18          = min_17 min min_16
    val min_19          = min_18 min min_17
    val in_9            = Protect(min_19, -inf, inf, true)
    val hPZ2            = HPZ2.ar(in_9)
    val in_10           = Protect(min_18, -inf, inf, true)
    val bPZ2            = BPZ2.ar(in_10)
    val min_20          = min_18 min bPZ2
    val sqrdif          = min_17 sqrDif min_20
    val min_21          = hPZ2 min sqrdif
    val tailLevel       = min_20 min sqrdif
    val min_22          = bPZ2 min min_15
    val min_23          = min_22 min min_6
    val min_24          = min_23 min c_1
    val min_25          = min_24 min min_14
    val min_26          = min_25 min tailLevel
    val min_27          = min_15 min henonN
    val min_28          = b_5 min ramp_0
    val min_29          = b_1 min min_2
    val min_30          = min_29 min min_28
    val in_11           = Protect(min_30, -inf, inf, true)
    val dur_0           = Protect(min_27, 0.0, 30.0, false)
    val ramp_1          = Ramp.ar(in_11, dur = dur_0)
    val min_31          = ramp_1 min 9.4327135
    val gt              = ramp_1 > min_8
    val plus            = latoocarfianC + gt
    val in_12           = Protect(min_24, -inf, inf, true)
    val attack_1        = Protect(henonN, 0.0, 30.0, false)
    val release_1       = Protect(amclip, 0.0, 30.0, false)
    val decay2_1        = Decay2.ar(in_12, attack = attack_1, release = release_1)
    val min_32          = min_26 min plus
    val min_33          = 9.4327135 min min_32
    val min_34          = decay2_1 min min_33
    val freq_7          = Protect(min_4, -inf, inf, false)
    val xi_1            = Protect(min_34, -inf, inf, false)
    val quadC           = QuadC.ar(freq = freq_7, a = a_1, b = b_2, c = -0.75, xi = xi_1)
    val dryLevel        = ramp_1 min bPZ2
    val min_35          = 10.0 min min_23
    val min_36          = min_35 min min_23
    val min_37          = min_36 min min_23
    val min_38          = min_37 min min_23
    val min_39          = min_38 min min_23
    val min_40          = min_37 min b_5
    val min_41          = min_40 min a_3
    val in_13           = Protect(b_5, -inf, inf, true)
    val freq_8          = Protect(min_8, 10.0, 20000.0, false)
    val hPF_0           = HPF.ar(in_13, freq = freq_8)
    val earlyRefLevel   = min_33 min decay2_1
    val in_14           = Protect(min_41, -inf, inf, true)
    val protect_0       = Protect(min_39, 0.55, inf, false)
    val revTime         = Protect(min_15, 0.0, 100.0, false)
    val damping         = Protect(amclip, 0.0, 1.0, false)
    val inputBW         = Protect(lag, 0.0, 1.0, false)
    val spread          = Protect(hPF_0, 0.0, 43.0, false)
    val maxRoomSize     = Protect(min_23, 0.55, 300.0, false)
    val roomSize        = protect_0 min maxRoomSize
    val gVerb           = GVerb.ar(in_14, roomSize = roomSize, revTime = revTime, damping = damping,
      inputBW = inputBW, spread = spread, dryLevel = dryLevel,
      earlyRefLevel = earlyRefLevel, tailLevel = tailLevel, maxRoomSize = maxRoomSize)
    val min_42          = min_34 min earlyRefLevel
    val min_43          = min_42 min sqrdif
    val min_44          = min_43 min henonN
    val sqrsum          = 9.4327135 sqrSum min_43
    val min_45          = min_41 min sqrsum
    val min_46          = min_45 min min_28
    val min_47          = min_46 min min_41
    val roundTo         = min_47 roundTo quadN
    val min_48          = min_47 min min_41
    val min_49          = min_48 min min_41
    val min_50          = min_49 min min_41
    val min_51          = min_50 min min_41
    val in_15           = Protect(min_32, -inf, inf, true)
    val maxDelayTime    = Protect(min_41, 0.0, 20.0, false)
    val protect_1       = Protect(min_51, 0.0, inf, false)
    val delayTime       = protect_1 min maxDelayTime
    val delayL          = DelayL.ar(in_15, maxDelayTime = maxDelayTime, delayTime = delayTime)
    val min_52          = min_15 min min_26
    val max             = gt max min_43
    val in_16           = Protect(max, -inf, inf, true)
    val freq_9          = Protect(min_40, 10.0, 20000.0, false)
    val hPF_1           = HPF.ar(in_16, freq = freq_9)
    val min_53          = hPF_1 min delayL
    val min_54          = min_53 min min_53
    val min_55          = min_34 min max
    val in_17           = Protect(9.4327135, -inf, inf, true)
    val coeff_2         = Protect(min_28, -1.0, 1.0, false)
    val oneZero_0       = OneZero.ar(in_17, coeff = coeff_2)
    val in_18           = Protect(oneZero_0, -inf, inf, true)
    val coeff_3         = Protect(min_43, -1.0, 1.0, false)
    val oneZero_1       = OneZero.ar(in_18, coeff = coeff_3)
    val freq_10         = Protect(min_7, -inf, inf, false)
    val lFDClipNoise    = LFDClipNoise.ar(freq_10)
    val length          = Protect(min_28, 1.0, 44100.0, false)
    val runningSum      = RunningSum.ar(9.4327135, length = length)
    val min_56          = lFDClipNoise min runningSum
    val min_57          = 0.18760166 min min_29
    val mix             = Mix(
      Seq[GE](min_21, min_31, quadC, gVerb, min_44, roundTo, min_52, min_54, min_55, oneZero_1, min_56, min_57))
    NegatumOut(mix)
  }

  def graphReduce1: SynthGraph = SynthGraph {
    import de.sciss.synth._
    import de.sciss.synth.ugen._
    NegatumIn()
    val excess        = DC.ar(-0.923686)
    val lFTri         = LFTri.ar(freq = 0.01, iphase = 0.0)
    val mix           = Mix(Seq[GE](excess, lFTri))
    NegatumOut(mix)
  }

  def graphReduce2: SynthGraph = SynthGraph {
    import de.sciss.synth._
    import de.sciss.synth.ugen._
    NegatumIn()
    val quadC_0       = QuadC.ar(freq = 1006.5003, a = 7.526999, b = 0.2, c = 7.526999, xi = 7.526999)
    // val in_0          = 0.0
    // val timeDown      = 30.0
    val lagUD         = 0.0
    // val freq_0        = 0.0
    val lFDNoise0     = 0.0
    val c_0           = quadC_0 min lFDNoise0 // XXX TODO --- theoretically zero, but actually not
    val quadC_1       = QuadC.ar(freq = 1006.5003, a = 0.2, b = 0.012651972, c = c_0, xi = 0.0)
    val gbmanL        = 0.2
    val min_0         = 0.012651972
    val freq_1        = 0.012651972
    val min_1         = freq_1 min quadC_1
    // val in_1          = 0.0
    // val coeff_0       = -0.999
    val a_0           = 0.0
    val b_0           = 0.0
    val x0_0          = Protect(quadC_0, -inf, inf, false)
    val henonC        = HenonC.ar(freq = 0.2, a = a_0, b = b_0, x0 = x0_0, x1 = 0.2)
    val in_2          = Protect(c_0, -inf, inf, true)
    val time          = Protect(henonC, 0.0, 30.0, false)
    val lag2          = Lag2.ar(in_2, time = time)
    val min_2         = 7.526999 min lag2
    val wrap2         = freq_1 wrap2 min_1
    val d             = c_0 min 0.2
    val xi_0          = Protect(gbmanL, -inf, inf, false)
    val yi_0          = Protect(min_1, -inf, inf, false)
    val standardN     = StandardN.ar(freq = freq_1, k = 0.2, xi = xi_0, yi = yi_0)
    val min_3         = d min standardN
    val excess        = min_3 excess 0.012651972
    val min_4         = min_3 min quadC_0
    val min_5         = min_4 min lagUD
    val min_6         = min_5 min wrap2
    val a_1           = lFDNoise0 max 7.526999
    val min_7         = quadC_0 min a_1
    val min_8         = min_0 min lag2
    val min_9         = min_8 min min_7
    val in_3          = Protect(min_1, -inf, inf, true)
    val freq_2        = Protect(a_0, 10.0, 20000.0, false)
    val rq            = Protect(standardN, 0.01, 100.0, false)
    val rHPF          = RHPF.ar(in_3, freq = freq_2, rq = rq)
    val min_10        = min_8 min rHPF
    val min_11        = 1006.5003 min min_10
    val min_12        = min_7 min min_11
    val freq_3        = Protect(lagUD, -inf, inf, false)
    val a_2           = Protect(quadC_1, -3.0, 3.0, false)
    val b_1           = Protect(min_3, 0.5, 1.5, false)
    val xi_1          = Protect(min_12, -inf, inf, false)
    val yi_1          = Protect(min_7, -inf, inf, false)
    val latoocarfianC = LatoocarfianC.ar(freq = freq_3, a = a_2, b = b_1, c = 0.5, d = d, xi = xi_1,
      yi = yi_1)
    val freq_4        = Protect(latoocarfianC, -inf, inf, false)
    val x0_1          = Protect(c_0, -inf, inf, false)
    val x1_0          = Protect(henonC, -inf, inf, false)
    val henonN        = HenonN.ar(freq = freq_4, a = a_1, b = 0.3, x0 = x0_1, x1 = x1_0)
    val freq_5        = Protect(henonN, 0.01, 20000.0, false)
    val lFTri         = LFTri.ar(freq = freq_5, iphase = 0.0)
    val min_13        = min_12 min min_0
    val in_4          = Protect(lag2, -inf, inf, true)
    val coeff_1       = Protect(lag2, -0.999, 0.999, false)
    val integrator    = Integrator.ar(in_4, coeff = coeff_1)
    val min_14        = min_13 min integrator
    val min_15        = lagUD min lag2
    val trunc         = 1006.5003 trunc c_0
    val thresh        = trunc thresh min_15
    val min_16        = thresh min lag2
    val hypotx        = thresh hypotApx min_16
    val xi_2          = Protect(rHPF, -inf, inf, false)
    val gbmanN        = GbmanN.ar(freq = 7.526999, xi = xi_2, yi = 2.1)
    val in_5          = Protect(lag2, -inf, inf, true)
    val freq_6        = Protect(hypotx, 10.0, 20000.0, false)
    val radius        = Protect(gbmanN, 0.0, 1.0, false)
    val twoZero       = TwoZero.ar(in_5, freq = freq_6, radius = radius)
    val mix           = Mix(Seq[GE](min_2, excess, min_6, min_9, lFTri, min_14, twoZero))
    NegatumOut(mix)
  }

  def main(args: Array[String]): Unit = {
    run()
  }

  def run(): Unit = {
    val g0 = graphReduce3
    val g1 = SynthGraph {
      import de.sciss.synth._
      import de.sciss.synth.ugen._
      val poll = Done.kr(Line.kr(0, 0, dur = 1.0))
      g0.sources.zipWithIndex.foreach {
        case (_: NegatumIn, _) | (_: Mix, _) =>
        case (in: GE, idx) =>
          val mx  = RunningMax.ar(in, poll)
          val mn  = RunningMin.ar(in, poll)
          val df  = mx sig_== mn
          val v   = Reduce.max(df)
          val pr  = poll & v
          mn.poll(pr, f"idx $idx%02d (${in.productPrefix})")

        case _ =>
      }
    }
    val g = g0.copy(sources = g0.sources ++ g1.sources)

    val as = AuralSystem()
    type S = InMemory
    val system: S = InMemory()

    system.step { implicit tx =>
      as.addClient(new AuralSystem.Client {
        def auralStarted(s: synth.Server)(implicit tx: Txn): Unit =
          Synth.play(g, Some("test"))(s.defaultGroup)

        def auralStopped()(implicit tx: Txn): Unit = ()
      })
      as.start()
    }
  }
}
