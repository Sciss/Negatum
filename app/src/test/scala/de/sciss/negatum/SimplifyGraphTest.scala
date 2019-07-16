package de.sciss.negatum

import de.sciss.file.File
import de.sciss.lucre.synth
import de.sciss.lucre.synth.{InMemory, Synth, Txn}
import de.sciss.model.Model
import de.sciss.numbers.Implicits._
import de.sciss.span.Span
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.impl.MkSynthGraphSource
import de.sciss.synth.proc.{AuralSystem, Bounce, Proc, TimeRef, Universe}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext

object SimplifyGraphTest {
  // "original" graph coming out of Negatum.
  // `negatum-94fb0dd8`
  def graphOrig: SynthGraph = SynthGraph {
    import de.sciss.synth._
    import de.sciss.synth.ugen._
    // source code automatically extracted

    NegatumIn()
    val in_0          = Protect(0.2, -inf, inf, true)
    val onePole       = OnePole.ar(in_0, coeff = 0.2)
    val iphase_0      = Protect(onePole, 0.0, 1.0, false)
    val lFCub         = LFCub.ar(freq = 0.2, iphase = iphase_0)
    val min_0         = lFCub min 0.2
    val min_1         = 0.2 min min_0
    val c_0           = min_1 min min_0
    val min_2         = c_0 min min_0
    val linCongC      = LinCongC.ar(freq = 18344.684, a = 0.2, c = c_0, m = 0.2, xi = 0.2)
    val min_3         = lFCub min linCongC
    val min_4         = min_3 min min_2
    val min_5         = min_4 min min_0
    val freq_0        = Protect(min_3, 0.1, 20000.0, false)
    val phase         = Protect(min_4, 0.0, 1.0, false)
    val impulse       = Impulse.ar(freq = freq_0, phase = phase)
    val mod_0         = impulse % 0.2
    val min_6         = min_5 min mod_0
    val in_1          = Protect(mod_0, -inf, inf, true)
    val freq_1        = Protect(min_6, 10.0, 20000.0, false)
    val rq_0          = Protect(min_6, 0.01, 100.0, false)
    val rHPF          = RHPF.ar(in_1, freq = freq_1, rq = rq_0)
    val min_7         = lFCub min min_1
    val ring1         = min_1 ring1 min_0
    val min_8         = mod_0 min min_7
    val min_9         = rHPF min min_7
    val min_10        = min_9 min ring1
    val min_11        = min_10 min ring1
    val min_12        = min_11 min min_8
    val min_13        = min_2 min min_12
    val in_2          = Protect(min_8, -inf, inf, true)
    val delay1        = Delay1.ar(in_2)
    val min_14        = min_12 min delay1
    val min_15        = min_6 min min_11
    val min_16        = delay1 min min_15
    val min_17        = min_14 min min_16
    val min_18        = min_17 min min_13
    val min_19        = min_16 min min_15
    val min_20        = min_18 min min_19
    val min_21        = min_20 min min_16
    val min_22        = min_21 min rHPF
    val in_3          = Protect(min_22, -inf, inf, true)
    val delay2        = Delay2.ar(in_3)
    val min_23        = delay2 min min_14
    val min_24        = min_23 min min_4
    val min_25        = delay2 min min_24
    val min_26        = min_24 min min_24
    val bitXor        = min_17 ^ min_18
    val min_27        = min_19 min min_10
    val min_28        = min_27 min min_26
    val in_4          = Protect(min_28, -inf, inf, true)
    val bPZ2          = BPZ2.ar(in_4)
    val min_29        = bitXor min bPZ2
    val min_30        = linCongC min min_29
    val min_31        = bitXor min min_30
    val freq_2        = Protect(min_31, -inf, inf, false)
    val a_0           = Protect(min_27, -3.0, 3.0, false)
    val b_0           = Protect(min_6, 0.5, 1.5, false)
    val c_1           = Protect(min_22, 0.5, 1.5, false)
    val xi_0          = Protect(min_14, -inf, inf, false)
    val yi_0          = Protect(min_20, -inf, inf, false)
    val latoocarfianL = LatoocarfianL.ar(freq = freq_2, a = a_0, b = b_0, c = c_1, d = 0.5, xi = xi_0,
      yi = yi_0)
    val min_32        = min_3 min latoocarfianL
    val min_33        = min_18 min min_6
    val min_34        = min_32 min min_16
    val min_35        = min_34 min impulse
    val min_36        = min_35 min min_33
    val min_37        = min_6 min latoocarfianL
    val min_38        = min_36 min impulse
    val min_39        = onePole min min_27
    val min_40        = min_39 min min_11
    val min_41        = min_37 min min_40
    val min_42        = min_36 min min_41
    val in_5          = Protect(min_38, -inf, inf, true)
    val freq_3        = Protect(min_17, 10.0, 20000.0, false)
    val rq_1          = Protect(min_3, 0.01, 100.0, false)
    val gain          = Protect(min_41, -144.0, 144.0, false)
    val k             = MidEQ.ar(in_5, freq = freq_3, rq = rq_1, gain = gain)
    val min_43        = rHPF min k
    val absdif        = min_42 absDif bPZ2
    val min_44        = min_43 min ring1
    val min_45        = k min min_44
    val min_46        = min_45 min min_43
    val min_47        = min_46 min min_34
    val min_48        = min_47 min absdif
    val min_49        = min_48 min min_28
    val freq_4        = min_34 trunc min_9
    val min_50        = min_38 min min_17
    val min_51        = min_32 min min_6
    val min_52        = min_51 min min_4
    val scaleneg      = min_43 scaleNeg min_52
    val xi_1          = Protect(scaleneg, -inf, inf, false)
    val yi_1          = Protect(min_8, -inf, inf, false)
    val standardL     = StandardL.ar(freq = freq_4, k = k, xi = xi_1, yi = yi_1)
    val in_6          = Protect(standardL, -inf, inf, true)
    val maxDelayTime  = Protect(min_17, 0.0, 20.0, false)
    val protect_0     = Protect(min_50, 0.0, inf, false)
    val delayTime_0   = protect_0 min maxDelayTime
    val delayC        = DelayC.ar(in_6, maxDelayTime = maxDelayTime, delayTime = delayTime_0)
    val min_53        = min_49 min delayC
    val neg           = min_53.unary_-
    val min_54        = neg min standardL
    val b_1           = min_32 min min_37
    val mod_1         = min_16 % min_26
    val freq_5        = Protect(min_54, -inf, inf, false)
    val x0            = Protect(mod_1, -inf, inf, false)
    val x1            = Protect(min_25, -inf, inf, false)
    val henonC        = HenonC.ar(freq = freq_5, a = 1.4, b = b_1, x0 = x0, x1 = x1)
    val in_7          = b_1 min min_54
    val min_55        = min_44 min delayC
    val min_56        = min_55 min min_31
    val min_57        = min_56 min min_18
    val min_58        = min_57 min 0.2
    val in_8          = Protect(in_7, -inf, inf, true)
    val protect_1     = Protect(min_58, 0.0, inf, false)
    val delayTime_1   = protect_1 min 0.2
    val combC         = CombC.ar(in_8, maxDelayTime = 0.2, delayTime = delayTime_1, decayTime = min_10)
    val roundTo       = mod_0 roundTo combC
    val in_9          = Protect(min_33, -inf, inf, true)
    val attack        = Protect(onePole, 0.0, 30.0, false)
    val release       = Protect(min_49, 0.0, 30.0, false)
    val decay2        = Decay2.ar(in_9, attack = attack, release = release)
    val in_10         = Protect(min_4, -inf, inf, true)
    val coeff_0       = Protect(min_36, -0.999, 0.999, false)
    val integrator    = Integrator.ar(in_10, coeff = coeff_0)
    val min_59        = min_42 min min_43
    val in_11         = Protect(min_37, -inf, inf, true)
    val timeUp_0      = Protect(min_54, 0.0, 30.0, false)
    val timeDown_0    = Protect(k, 0.0, 30.0, false)
    val lag2UD        = Lag2UD.ar(in_11, timeUp = timeUp_0, timeDown = timeDown_0)
    val in_12         = Protect(min_54, -inf, inf, true)
    val freq_6        = Protect(min_12, 10.0, 20000.0, false)
    val rq_2          = Protect(lag2UD, 0.01, 100.0, false)
    val bPF           = BPF.ar(in_12, freq = freq_6, rq = rq_2)
    val min_60        = min_15 min bPF
    val freq_7        = Protect(min_60, 0.01, 20000.0, false)
    val iphase_1      = Protect(min_28, 0.0, 4.0, false)
    val lFTri         = LFTri.ar(freq = freq_7, iphase = iphase_1)
    val min_61        = min_20 min min_31
    val min_62        = min_32 min min_27
    val coeff_1       = Protect(delayC, 0.8, 0.99, false)
    val leakDC        = LeakDC.ar(in_7, coeff = coeff_1)
    val min_63        = b_1 min min_61
    val min_64        = min_63 min min_62
    val roundUpTo     = min_30 roundUpTo min_64
    val in_13         = Protect(min_1, -inf, inf, true)
    val timeUp_1      = Protect(rHPF, 0.0, 30.0, false)
    val timeDown_1    = Protect(roundUpTo, 0.0, 30.0, false)
    val lag3UD        = Lag3UD.ar(in_13, timeUp = timeUp_1, timeDown = timeDown_1)
    val m_0           = lag3UD min min_60
    val freq_8        = Protect(m_0, -inf, inf, false)
    val xi_2          = Protect(bPZ2, -inf, inf, false)
    val linCongL      = LinCongL.ar(freq = freq_8, a = 1.1, c = min_10, m = m_0, xi = xi_2)
    val min_65        = min_40 min min_57
    val mix           = Mix(
      Seq[GE](henonC, roundTo, decay2, integrator, min_59, lFTri, leakDC, linCongL, min_65))
    NegatumOut(mix)
  }

  // first reduction: replace all constant signals by constants,
  // eliminate silence from mix
  def graphReduce1: SynthGraph = SynthGraph {
    import de.sciss.synth._
    import de.sciss.synth.ugen._
    NegatumIn()
    val lFCub         = LFCub.ar(freq = 0.2, iphase = 0.0)
    val min_0         = lFCub min 0.2
    val min_1         = 0.2 min min_0
    val c_0           = min_1 min min_0
    val min_2         = c_0 min min_0
    val linCongC      = LinCongC.ar(freq = 18344.684, a = 0.2, c = c_0, m = 0.2, xi = 0.2)
    val min_3         = lFCub min linCongC
    val min_4         = min_3 min min_2
    val min_5         = min_4 min min_0
    val freq_0        = Protect(min_3, 0.1, 20000.0, false)
    val phase         = Protect(min_4, 0.0, 1.0, false)
    val impulse       = Impulse.ar(freq = freq_0, phase = phase)
    val min_6         = min_5 min 0.0
    val ring1         = min_1 ring1 min_0
    val min_15        = min_6 min 0.0
    val min_16        = 0.0 min min_15
    val min_17        = 0.0 min min_16
    val min_18        = min_17 min 0.0
    val min_19        = min_16 min min_15
    val min_20        = min_18 min min_19
    val min_21        = min_20 min min_16
    val min_22        = min_21 min 0.0
    val in_3          = Protect(min_22, -inf, inf, true)
    val delay2        = Delay2.ar(in_3)
    val min_23        = delay2 min 0.0
    val min_24        = min_23 min min_4
    val min_26        = min_24 min min_24
    val min_27        = min_19 min 0.0
    val min_28        = min_27 min min_26
    val in_4          = Protect(min_28, -inf, inf, true)
    val bPZ2          = BPZ2.ar(in_4)
    val min_29        = 0.0 min bPZ2
    val min_30        = linCongC min min_29
    val min_31        = 0.0 min min_30
    val min_32        = min_3 min 0.0
    val min_33        = min_18 min min_6
    val min_34        = min_32 min min_16
    val min_35        = min_34 min impulse
    val min_36        = min_35 min min_33
    val min_37        = min_6 min 0.0
    val min_38        = min_36 min impulse
    val min_39        = 0.0 min min_27
    val min_40        = min_39 min 0.0
    val min_41        = min_37 min min_40
    val min_42        = min_36 min min_41
    val in_5          = Protect(min_38, -inf, inf, true)
    val rq_1          = Protect(min_3, 0.01, 100.0, false)
    val gain          = Protect(min_41, -144.0, 144.0, false)
    val k             = MidEQ.ar(in_5, freq = 10.0, rq = rq_1, gain = gain)
    val min_43        = 0.0 min k
    val absdif        = min_42 absDif bPZ2
    val min_44        = min_43 min ring1
    val min_45        = k min min_44
    val min_46        = min_45 min min_43
    val min_47        = min_46 min min_34
    val min_48        = min_47 min absdif
    val min_49        = min_48 min min_28
    val freq_4        = min_34 trunc 0.0
    val min_51        = min_32 min min_6
    val min_52        = min_51 min min_4
    val scaleneg      = min_43 scaleNeg min_52
    val xi_1          = Protect(scaleneg, -inf, inf, false)
    val standardL     = StandardL.ar(freq = freq_4, k = k, xi = xi_1, yi = 0.0)
    val in_6          = Protect(standardL, -inf, inf, true)
    val delayC        = DelayC.ar(in_6, maxDelayTime = 0.0, delayTime = 0.0)
    val min_53        = min_49 min delayC
    val neg           = min_53.unary_-
    val min_54        = neg min standardL
    val b_1           = min_32 min min_37
    val in_7          = b_1 min min_54
    val min_55        = min_44 min delayC
    val min_56        = min_55 min min_31
    val min_57        = min_56 min min_18
    val in_10         = Protect(min_4, -inf, inf, true)
    val coeff_0       = Protect(min_36, -0.999, 0.999, false)
    val integrator    = Integrator.ar(in_10, coeff = coeff_0)
    val min_59        = min_42 min min_43
    val in_11         = Protect(min_37, -inf, inf, true)
    val timeDown_0    = Protect(k, 0.0, 30.0, false)
    val lag2UD        = Lag2UD.ar(in_11, timeUp = 0.0, timeDown = timeDown_0)
    val in_12         = Protect(min_54, -inf, inf, true)
    val rq_2          = Protect(lag2UD, 0.01, 100.0, false)
    val bPF           = BPF.ar(in_12, freq = 10.0, rq = rq_2)
    val min_60        = min_15 min bPF
    val lFTri         = LFTri.ar(freq = 0.01, iphase = 0.0)
    val leakDC        = LeakDC.ar(in_7, coeff = 0.8)
    val in_13         = Protect(min_1, -inf, inf, true)
    val lag3UD        = Lag3UD.ar(in_13, timeUp = 0.0, timeDown = 0.0)
    val m_0           = lag3UD min min_60
    val freq_8        = Protect(m_0, -inf, inf, false)
    val xi_2          = Protect(bPZ2, -inf, inf, false)
    val linCongL      = LinCongL.ar(freq = freq_8, a = 1.1, c = 0.0, m = m_0, xi = xi_2)
    val min_65        = min_40 min min_57
    val mix           = Mix(
      Seq[GE](integrator, min_59, lFTri, leakDC, linCongL, min_65))
    NegatumOut(mix)
  }

  // first reduction: replace all constant signals by constants,
  // eliminate silence from mix
  def graphReduce2: SynthGraph = SynthGraph {
    import de.sciss.synth._
    import de.sciss.synth.ugen._
    NegatumIn()
    val lFCub         = LFCub.ar(freq = 0.2, iphase = 0.0)
    val min_0         = lFCub min 0.2 // interesting to replace by `lFCub`
    val linCongC      = LinCongC.ar(freq = 18344.684, a = 0.2, c = min_0, m = 0.2, xi = 0.2)
    val min_3         = lFCub min linCongC
    val min_4         = min_3 min min_0
    val min_6         = min_4 min 0.0
    val in_3          = Protect(min_6, -inf, inf, true)
    val delay2        = Delay2.ar(in_3)
    val min_23        = delay2 min 0.0
    val min_24        = min_23 min min_4
    val in_4          = Protect(min_24, -inf, inf, true)
    val bPZ2          = BPZ2.ar(in_4)
    val min_29        = 0.0 min bPZ2
    val min_30        = linCongC min min_29
    val rq_1          = Protect(min_3, 0.01, 100.0, false)
    val k             = MidEQ.ar(in_3, freq = 10.0, rq = rq_1, gain = min_6)
    val min_43        = 0.0 min k
    val min_44        = min_43  // min_43 min ring1
    val scaleneg      = min_43 scaleNeg min_6
    val standardL     = StandardL.ar(freq = min_6, k = k, xi = scaleneg, yi = 0.0)
    val in_6          = Protect(standardL, -inf, inf, true)
    val in_7          = min_6 min standardL
    val min_55        = min_44 min in_6
    val min_56        = min_55 min min_30
    val in_10         = Protect(min_4, -inf, inf, true)
    val coeff_0       = Protect(min_6, -0.999, 0.999, false)
    val integrator    = Integrator.ar(in_10, coeff = coeff_0)
    val timeDown_0    = Protect(k, 0.0, 30.0, false)
    val lag2UD        = Lag2UD.ar(in_3, timeUp = 0.0, timeDown = timeDown_0)
    val rq_2          = Protect(lag2UD, 0.01, 100.0, false)
    val bPF           = BPF.ar(in_6, freq = 10.0, rq = rq_2)
    val min_60        = min_6 min bPF
    val lFTri         = LFTri.ar(freq = 0.01, iphase = 0.0)
    val leakDC        = LeakDC.ar(in_7, coeff = 0.8)
    val linCongL      = LinCongL.ar(freq = min_60, a = 1.1, c = 0.0, m = min_60, xi = bPZ2)
    val mix           = Mix(
      Seq[GE](integrator, min_6, lFTri, leakDC, linCongL, min_56))
    NegatumOut(mix)
  }

  def main(args: Array[String]): Unit = {
//    mkReduction2()
//    play(graphReduce2)
    testOptimize()
  }

  def testOptimize(): Unit = {
    val graphIn = graphOrig
    val cfg     = Optimize.Config(graphIn, sampleRate = 44100, analysisDur = 2.0)
    val opt     = Optimize(cfg)
    import ExecutionContext.Implicits.global
    opt.start()
    val res     = Await.result(opt, Duration.Inf)
    println(s"numConst = ${res.numConst}, numEqual = ${res.numEqual}")
    val sourceOut = MkSynthGraphSource(res.graph)
    println()
    println(sourceOut)
  }

  def play(g: SynthGraph): Unit = {
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

  def mkReduction1(): Unit = {
    val g0 = graphOrig // graphReduce1
    val g1 = SynthGraph {
      import de.sciss.synth._
      import de.sciss.synth.ugen._
      val poll = T2A.ar(Done.kr(Line.kr(0, 0, dur = 1.0)))
      g0.sources.zipWithIndex.foreach {
        case (_: NegatumIn, _) | (_: Mix, _) =>
        case (in: GE, idx) =>
          val mx  = RunningMax.ar(in, poll)
          val mn  = RunningMin.ar(in, poll)
          val df  = mx sig_== mn
          val v   = Reduce.max(df)
          val pr  = poll & v
          mn.poll(pr, f"idx $idx%03d (${in.productPrefix})")

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

    // println(txt.split("\n").sorted.mkString("\n"))
  }

  def mkReduction2(): Unit = {
    val testDur = 2.0

    val g0 = graphOrig  // graphReduce1
    var numSignals = 0
    val g1 = SynthGraph {
      import de.sciss.synth._
      import de.sciss.synth.ugen._
//      val poll = T2A.ar(Done.kr(Line.kr(0, 0, dur = 1.0)))
      val sigB = List.newBuilder[GE]
      g0.sources.zipWithIndex.foreach {
        case (_: NegatumIn, _) | (_: Mix, _) =>
        case (in: GE, _ /*idx*/) =>
          sigB += in
        case _ =>
      }
      val sig = sigB.result()
      numSignals = sig.size
      (numSignals: GE).poll(0, "numSignals")
      ReplaceOut.ar(0, sig)
    }
    val g = g0.copy(sources = g0.sources ++ g1.sources)

    type S = InMemory
    implicit val system: S = InMemory()

    implicit val u: Universe[S] = system.step { implicit tx =>
      Universe.dummy
    }

    val b = Bounce[S]()
    val bCfg = Bounce.Config[S]()
    bCfg.realtime = false
    bCfg.span     = Span(0L, (testDur * TimeRef.SampleRate).toLong)
    bCfg.server.outputBusChannels = numSignals
    bCfg.server.audioBusChannels  = math.max(128, numSignals.nextPowerOfTwo)
    bCfg.server.sampleRate        = 44100
    bCfg.group    = system.step { implicit tx =>
      val p = Proc[S]
      p.graph() = g
      tx.newHandle(p) :: Nil
    }

    // DO NOT USE THIS -- IT CREATES THREAD STARVATION
//    import de.sciss.synth.proc.SoundProcesses.executionContext
    import ExecutionContext.Implicits.global

    val noObs: Model.Listener[Any] = { case _ => () }
    val r: Future[File] = b.run(bCfg)(noObs)
    Await.ready(r, Duration.Inf)

    def processBounce(f: File): Unit = {
      val af  = AudioFile.openRead(f)
      try {
        println(f"Bounce length: ${af.numFrames / af.sampleRate}%1.1f sec.")
        val buf = af.buffer(af.numFrames.toInt)
        af.read(buf)
        var sameMap = Map.empty[Int, Either[Float, Int]]
        for (ch1 <- buf.indices) {
          val b1 = buf(ch1)
          val v0 = b1(0)
          if (b1.forall(_ == v0)) {
            sameMap += ch1 -> Left(v0)
          } else {
            for (ch2 <- 0 until ch1; if !sameMap.contains(ch1) && !sameMap.contains(ch2)) {
              val b2 = buf(ch2)
              if (b1 sameElements b2) {
                sameMap += ch1 -> Right(ch2)
              }
            }
          }
        }
        val info = sameMap.toList.sortBy(_._1).map {
          case (ch1, Right(ch2))  => (ch1 + 1, Right(ch2 + 1))
          case (ch1, Left(v))     => (ch1 + 1, Left(v))
        }
        println("Constant and redundant channels:")
        info.foreach {
          case (ch1, Right(ch2)) =>
            println(s"idx $ch1 == idx $ch2 (${g0.sources(ch1).productPrefix} == ${g0.sources(ch2).productPrefix})")
          case (ch1, Left(v)) =>
            println(s"idx $ch1 == $v (${g0.sources(ch1).productPrefix})")
        }

      } finally {
        af.close()
        f .delete()
      }
    }

    val done: Try[File] => Unit = {
      case Success(f) =>
//        println(s"Bounce: $f")
        var attempt = 0
        while ({
          val numFrames = Try(AudioFile.readSpec(f)).map(_.numFrames).getOrElse(0L)
          numFrames == 0 && attempt < 10
        }) {
          Thread.sleep(500)
          attempt += 1
        }
        if (attempt == 10) {
          println(s"Bounce $f is empty :(")
          f.delete()
        } else {
          processBounce(f)
        }

      case Failure(ex) =>
        println(ex)
        ex.printStackTrace()
        Thread.sleep(1000)
    }

    // bullshit: execution deferred even when completed, and JVM exits
    if (r.isCompleted) done(r.value.get) else r.onComplete(done)
    // println(txt.split("\n").sorted.mkString("\n"))
  }
}
