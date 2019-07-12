package de.sciss.negatum

import de.sciss.synth.{Server, SynthGraph}
import de.sciss.synth
import de.sciss.synth.trace.TraceOps._
import de.sciss.synth.trace.ugen.Trace

import scala.concurrent.ExecutionContext

object FindBadUGens extends App {
  def gIn = SynthGraph {
    import synth._
    import ugen._
    NegatumIn()
    val lFDNoise1_0 = LFDNoise1.ar(3085.6042)
    val lFDNoise1_1 = LFDNoise1.ar(3085.6042)
    val in_0        = Delay1.ar(lFDNoise1_1)
    val hypot       = lFDNoise1_1 hypot in_0
    val length      = Protect(lFDNoise1_1, 1.0, 44100.0, false)
    val runningSum  = RunningSum.ar(in_0, length = length)
    val times       = runningSum * hypot
    val min_0       = times min hypot
    val min_1       = min_0 min hypot
    val min_2       = min_1 min hypot
    val a_0         = min_2 min hypot
    val min_3       = a_0 min hypot
    val min_4       = min_3 min hypot
    val min_5       = min_4 min hypot
    val cuspL       = CuspL.ar(freq = lFDNoise1_1, a = 3085.6042, b = lFDNoise1_1, xi = 3085.6042)
    val in_1        = Protect(hypot, -inf, inf, true)
    val hPZ1_0      = HPZ1.ar(in_1)
    val in_2        = Protect(3085.6042, -inf, inf, true)
    val protect     = Protect(min_5, 0.55, inf, false)
    val revTime     = Protect(cuspL, 0.0, 100.0, false)
    val damping     = Protect(in_0, 0.0, 1.0, false)
    val inputBW     = Protect(lFDNoise1_1, 0.0, 1.0, false)
    val spread      = Protect(cuspL, 0.0, 43.0, false)
    val maxRoomSize = Protect(hypot, 0.55, 300.0, false)
    val roomSize    = protect min maxRoomSize
    val gVerb       = GVerb.ar(in_2, roomSize = roomSize, revTime = revTime, damping = damping,
      inputBW = inputBW, spread = spread, dryLevel = hPZ1_0,
      earlyRefLevel = lFDNoise1_1, tailLevel = hPZ1_0, maxRoomSize = maxRoomSize)
    val ring2       = times ring2 gVerb
    val min_6       = runningSum min ring2
    val min_7       = min_6 min ring2
    val b_0         = hypot min ring2
    val min_8       = min_6 min lFDNoise1_0
    val gt          = hypot > min_8
    val min_9       = gt min cuspL
    val in_3        = Protect(runningSum, -inf, inf, true)
    val freq_0      = Protect(ring2, 10.0, 20000.0, false)
    val rq          = Protect(min_9, 0.01, 100.0, false)
    val bPF         = BPF.ar(in_3, freq = freq_0, rq = rq)
    val min_10      = 3085.6042 min min_8
    val min_11      = min_10 min bPF
    val min_12      = min_11 min min_11
    val excess      = ring2 excess lFDNoise1_1
    val in_4        = Protect(gt, -inf, inf, true)
    val freq_1      = Protect(min_9, 10.0, 20000.0, false)
    val radius_0    = Protect(excess, 0.0, 1.0, false)
    val twoZero_0   = TwoZero.ar(in_4, freq = freq_1, radius = radius_0)
    val min_13      = twoZero_0 min min_12
    val min_14      = b_0 min min_13
    val min_15      = min_11 min min_10
    val in_5        = Protect(min_15, -inf, inf, true)
    val freq_2      = Protect(min_14, 10.0, 20000.0, false)
    val radius_1    = Protect(min_14, 0.0, 1.0, false)
    val twoZero_1   = TwoZero.ar(in_5, freq = freq_2, radius = radius_1)
    val min_16      = ring2 min twoZero_1
    val in_6        = Protect(min_16, -inf, inf, true)
    val hPZ1_1      = HPZ1.ar(in_6)
    val freq_3      = Protect(runningSum, -inf, inf, false)
    val x0_0        = Protect(min_14, -inf, inf, false)
    val x1_0        = Protect(min_16, -inf, inf, false)
    val henonC      = HenonC.ar(freq = freq_3, a = a_0, b = b_0, x0 = x0_0, x1 = x1_0)
    val roundUpTo   = min_0 roundUpTo henonC
    val b_1         = roundUpTo min min_4
    val in_7        = Protect(roundUpTo, -inf, inf, true)
    val delay2      = Delay2.ar(in_7)
    val in_8        = Protect(roundUpTo, -inf, inf, true)
    val a_1         = HPZ1.ar(in_8)
    val freq_4      = Protect(delay2, -inf, inf, false)
    val xi_0        = Protect(henonC, -inf, inf, false)
    val cuspN       = CuspN.ar(freq = freq_4, a = a_1, b = b_1, xi = xi_0)
    val min_17      = cuspN min min_14
    val a_2         = min_17 min a_1
    val min_18      = min_17 min min_0
    val min_19      = min_18 min min_13
    val min_20      = min_19 min gt
    val ring4       = min_20 ring4 in_0
    val min_21      = min_20 min ring4
    val min_22      = min_21 min b_0
    val min_23      = min_22 min min_18
    val min_24      = min_17 min min_0
    val min_25      = min_24 min gVerb
    val min_26      = min_25 min min_8
    val min_27      = a_2 min min_26
    val min_28      = min_27 min min_19
    val min_29      = min_28 min min_6
    val b_2         = min_29 min ring4
    val freq_5      = Protect(b_1, -inf, inf, false)
    val x0_1        = Protect(min_7, -inf, inf, false)
    val x1_1        = Protect(min_26, -inf, inf, false)
    val henonN      = HenonN.ar(freq = freq_5, a = a_2, b = b_2, x0 = x0_1, x1 = x1_1)
    val min_30      = henonN min henonN
    val min_31      = min_30 min min_7
    val mix         = Mix(Seq[GE](hPZ1_1, min_23, min_31))
    NegatumOut(mix)
  }

  val tg = traceGraph {
    import synth._
    import ugen._
    gIn.sources.zipWithIndex.foreach {
      case (x: Protect, _)  => SynthGraph.builder.addLazy(x)
      case (src: GE, idx)   => Trace(src, label = s"${src.productPrefix}_$idx")
      case (x, _)           => SynthGraph.builder.addLazy(x)
    }
  }

  Server.run { _ =>
    val fut1 = tg.traceFor(numFrames = 2)
    val fut2 = tg.traceFor(numFrames = 2)

    import ExecutionContext.Implicits.global

    for {
      data1 <- fut1
      data2 <- fut2
    } {
      (data1 zip data2).foreach { case (d1, d2) =>
        val labels = d1.traceMap.keysIterator.toSeq.sortBy { lb =>
          val i = lb.lastIndexOf('_')
          lb.substring(i + 1).toInt
        }
        labels.foreach { lb =>
          val t1 = d1.traceMap(lb)
          val t2 = d2.traceMap(lb)
          if (t1 != t2) {
            println(s"Diverging trace for '$lb': [1] ${t1.flatten.mkString(", ")} -- [2] ${t2.flatten.mkString(", ")}")
          }
        }
      }
      println("Analysis completed.")
      sys.exit()
    }
  }
}