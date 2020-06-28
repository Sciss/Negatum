package de.sciss.negatum

import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.impl.MkSynthGraphSource

import scala.concurrent.ExecutionContext.Implicits._

object OptimizeTest {
  def main(args: Array[String]): Unit = {
//    Application.init(Mellite)
//    Mellite.initTypes()
    run()
  }

  def run(): Unit = {
    val oCfg = Optimize.Config(
      graph         = gIn,
      sampleRate    = 44100,
      analysisDur   = 5.9,
      blockSize     = 64,
      expandProtect = true, // true,
      expandIO      = false, // true,
    )
    val o = Optimize(oCfg)
    o.start()
    o.foreach { res =>
      println(s"Optimization found ${res.numConst} constant replacement and ${res.numEqual} redundant elements.")
      val gOut = MkSynthGraphSource(res.graph)
      println(gOut)
      sys.exit()
    }

    new Thread {
      override def run(): Unit = this.synchronized(this.wait())
      start()
    }
  }

  val gIn: SynthGraph = SynthGraph {
    import de.sciss.synth.ugen._
    import de.sciss.synth.{GE, _}
    val inf = Float.PositiveInfinity

    NegatumIn()
    val lFDClipNoise    = LFDClipNoise.ar(0.025)
    val min             = lFDClipNoise min 0.64
    val median          = Median.ar(2211.0, length = 31.0)
    val freq_0          = Protect(median, -inf, inf, false) // this is causing trouble
    val lFDNoise3       = LFDNoise3.ar(freq_0)
    val lFDNoise1       = LFDNoise1.ar(2538.0)
    val lFPulse         = LFPulse.ar(freq = 337.99133, iphase = 0.0, width = 1.0)
    val mix             = Mix(
      Seq[GE](min, lFDNoise3, 1.0, lFDNoise1, lFPulse))
    NegatumOut(mix)
  }
}
