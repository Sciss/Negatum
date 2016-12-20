/*
 *  Hibernation.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.file._
import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth
import de.sciss.synth.proc.{Action, Durable, Workspace, graph}
import de.sciss.synth.ugen
import de.sciss.numbers.Implicits._

import scala.concurrent.stm.TxnExecutor

object Hibernation {
  lazy val baseDir        : File = userHome / "Documents" / "projects" / "Imperfect"
  lazy val sessionsDir    : File = baseDir / "anemone"
  lazy val mainSession    : File = sessionsDir / "hibernate.mllt"

  private val fileDateFmt = new SimpleDateFormat("yyMMdd_HHmmss", Locale.US)

  private[this] val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'proc' - ", Locale.US)
  var showCompLog = true

  def logComp(what: => String): Unit =
    if (showCompLog) Console.out.println(s"${logHeader.format(new Date())}$what")

  def logCompErr(what: => String): Unit =
    Console.err.println(s"${logHeader.format(new Date())} ERROR - $what")

  def mkDateString(): String = fileDateFmt.format(new Date)

  val actions: Seq[NamedAction] = Seq(
    ActionHibernateStart, ActionHibernateRecDone, ActionHibernateFScDone
  )

  def registerActions(): Unit = TxnExecutor.defaultAtomic { implicit itx =>
    implicit val tx = TxnLike.wrap(itx)
    actions.foreach { a =>
      Action.registerPredef(a.name, a)
    }
  }

  def createWorkspace(): Workspace.Durable = {
    val dir = mainSession
    val ws: Workspace.Durable = if (dir.exists()) {
      readWorkspace(dir)
    } else {
      val _ws = emptyWorkspace(dir)
      _ws.cursor.step { implicit tx =>
        build(_ws)
      }
      println("Created workspace.")
      _ws
    }
    ws
  }

  def readWorkspace(dir: File): Workspace.Durable = {
    val wsl = Workspace.read(dir, BerkeleyDB.factory(dir))
    wsl match {
      case d: Workspace.Durable => d
      case _ => sys.error(s"Expected durable workspace for '$dir', but got $wsl")
    }
  }

  def emptyWorkspace(dir: File): Workspace.Durable =
    Workspace.Durable.empty(dir, BerkeleyDB.factory(dir))

  def build[S <: SSys[S]](workspace: Workspace[S])(implicit tx: S#Tx): Unit = {
    val dsl = DSL[S]
    import dsl._

    val f = workspace.root

    val locImperfect = artifactLoc(baseDir).in(f)

    val ensMain = ensemble("main").in(f)(initPlay = true)

    val fPool = folder("pool").in(f)
    folder("pool-1").in(fPool)
    folder("pool-2").in(fPool)

    val bSide   = boolean(false)

    val ensHibListen = ensemble("ens-hibernate-listen").in(f)(initPlay = false)
    ensHibListen.addTo(ensMain)

    val ensPlay = ensemble("ens-play").in(f)(initPlay = true)
    ensPlay.addTo(ensMain)

    val pHibListen = proc("hibernate-listen").in(ensHibListen) {
      import graph._
      import Ops._
      import ugen._

      val indexIn     = "bus-in".ir
      val numCh       = 1 // NumChannels(indicesIn)
      val in          = PhysicalIn.ar(indices = indexIn) \ 0
      val isLoud      = Impulse.kr(0)
      val run         = SetResetFF.kr(trig = isLoud, reset = 0)
      val inDly       = in // DelayN.ar(in, maxDelayTime = 0.5, delayTime = 0.5)
      val inHPF       = HPF.ar(inDly, 100) // 80
      val dur         = "rec-dur".ir(60.0) // (8)
      DC.kr(dur).poll(run, "listen-hibernate run for")
      val numFrames   = dur * SampleRate.ir
      val recBuf      = BufferOut(artifact = "file", action = "done", numFrames = numFrames, numChannels = numCh)
      val rec         = RecordBuf.ar(in = inHPF, buf = recBuf, run = run, loop = 0)
      val full        = Done.kr(rec)
      StopSelf(full)
    }

    val fscHlbProcess = fscape("hibernate-fsc").in(f) {
      import de.sciss.fscape._
      import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, _}
      import de.sciss.fscape.lucre.graph._

      (1 :GE).poll(0, "zerophase-rendering")

      val in = AudioFileIn("file-in")
      val inLen = in.numFrames
      inLen.poll(0, "fsc: num-frames in")

      // 'analysis'
      val fftSize     = 131072
      val winStep     = fftSize / 4
      val inW         = Sliding         (in = in , size = fftSize, step = winStep)
      val fft         = Real1FullFFT    (in = inW, size = fftSize)

      case class CepCoef(crr: Int, cri: Int, clr: Int, cli: Int,
                         ccr: Int, cci: Int, car: Int, cai: Int,
                         gain: Double)

      val One = CepCoef(
        crr =  0, cri =  0,
        clr = +1, cli = +1,
        ccr = +1, cci = -1,
        car = +1, cai = -1,
        gain = 1.0/2097152    // XXX TODO --- what's this factor?
      )

//      val Two = CepCoef(
//        crr = +1, cri = +1,
//        clr =  0, cli =  0,
//        ccr = +1, cci = -1,
//        car = +1, cai = -1,
//        gain = 1.0/32         // XXX TODO --- what's this factor?
//      )

      // 'percussion'
      val logC        = fft.complex.log.max(-80)
      val cep         = Complex1IFFT    (in = logC, size = fftSize) / fftSize
      val coef = One
      val cepOut = {
        import coef._
        FoldCepstrum  (in = cep, size = fftSize,
          crr = crr, cri = cri, clr = clr, cli = cli,
          ccr = ccr, cci = cci, car = car, cai = cai)
      }
      val freq        = Complex1FFT   (in = cepOut, size = fftSize) * fftSize
      val fftOut      = freq.complex.exp

      // 'synthesis'
      val outW        = Real1FullIFFT (in = fftOut, size = fftSize)
      val winIn       = GenWindow(size = fftSize, shape = GenWindow.Hann)
      val winOut      = outW * winIn
      val lap         = OverlapAdd(in = winOut, size = fftSize, step = winStep)

      def mkLoop(fadeLen: Int, in: GE, inLen: GE): GE = {
        val fadeIn  = in.take(fadeLen) * Line(0, 1, len = fadeLen).sqrt
        val susLen  = inLen - 2 * fadeLen
        val fadeOut = in.drop(fadeLen) *
          (DC(1).take(susLen) ++ Line(1, 0, len = fadeLen).sqrt)
        val out     = fadeOut + (DC(0).take(susLen) ++ BufferDisk(fadeIn))
        out
      }

      val inLen1 = inLen + (winStep - 1)
      val lapLen = inLen1 - (inLen1 % winStep)

      val fadeLen = 44100
      val outLen  = lapLen - fadeLen
      val faded   = mkLoop(fadeLen = fadeLen, in = lap, inLen = lapLen)

      Progress(Timer(DC(0)).matchLen(faded) / (2 * outLen), Metro(44100), "process")

      def normalize(in: GE): GE = {
        val abs       = in.abs
        val run       = RunningMax(abs)
        val minAmp    = -60f.dbamp: GE    // bloody IntelliJ highlight bug
        val max       = run.last.max(minAmp)
        val headroom  = -0.2.dbamp
        val gain      = max.reciprocal * headroom
        val buf       = BufferDisk(in)
        val sig       = buf * gain
        sig
      }

      val sig = normalize(faded)
      val out = AudioFileOut("file-out", in = sig)
      Progress(out / (2 * outLen), Metro(44100), "normalize")

      OnComplete("done")
    }

    val aHibStart = action("hibernate-start").in(f)(ActionHibernateStart)
    aHibStart("context")    = ensHibListen
    aHibStart("dir")        = locImperfect
    aHibStart("proc")       = pHibListen
    aHibStart("side")       = bSide
    aHibStart("ens-play")   = ensPlay
    aHibStart.addTo(ensMain)

    val aHibRecDone = action("hibernate-rec-done").in(f)(ActionHibernateRecDone)
    aHibRecDone("context")  = ensHibListen
    aHibRecDone("fscape")   = fscHlbProcess
    aHibRecDone("dir")      = locImperfect

    val aHlbFScDone = action("hibernate-fsc-done").in(f)(ActionHibernateFScDone)
    aHlbFScDone("side")     = bSide
    aHlbFScDone("restart")  = aHibStart
    aHlbFScDone("ens-play") = ensPlay
    aHlbFScDone("pool")     = fPool

    fscHlbProcess("done")   = aHlbFScDone

    aHibStart ("done")      = aHibRecDone
    pHibListen("done")      = aHibRecDone
  }
}