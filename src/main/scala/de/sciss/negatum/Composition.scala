/*
 *  Composition.scala
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
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{BooleanObj, IntObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{NoSys, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.ugen
import de.sciss.synth.proc.Action.Universe
import de.sciss.synth.proc._

import scala.util.{Failure, Success}

object Composition {
  private[this] lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'proc' - ", Locale.US)
  var showCompLog = true

  def logComp(what: => String): Unit =
    if (showCompLog) Console.out.println(s"${logHeader.format(new Date())}$what")

  def logCompErr(what: => String): Unit =
    Console.err.println(s"${logHeader.format(new Date())} ERROR - $what")

  def apply[S <: SSys[S]](workspace: Workspace[S])(implicit tx: S#Tx): Unit = {
    val dsl = DSL[S]
    import dsl._

    val f = workspace.root

    val locImperfect = artifactLoc(userHome/"Documents"/"projects"/"Imperfect").in(f)

    val ensNegListen = ensemble("ens-negatum-listen").in(f)(initPlay = false)

    val pNegListen = proc("negatum-listen").in(ensNegListen) {
      import ugen._
      import graph._
      import Ops._

      val indexIn     = "bus-in".ir
      val numCh       = 1 // NumChannels(indicesIn)
      val in          = PhysicalIn.ar(indices = indexIn) \ 0
      val fftSize     = 1024
      val fftBuf      = LocalBuf(numFrames = fftSize, numChannels = numCh)
      val fft         = FFT(buf = fftBuf, in = in, hop = 0.5, winType = 1)
      val loudness    = Loudness(fft)
      val threshStart = "thresh-start".ir( 10f)
      val threshEnd   = "thresh-end"  .ir(  3f)
      val threshDur   = "thresh-dur"  .ir(120f)
      val thresh      = Line.kr(threshStart, threshEnd, threshDur)
      val isLoud      = loudness > thresh
      val run         = SetResetFF.kr(trig = isLoud, reset = 0)

      val inDly     = DelayN.ar(in, maxDelayTime = 0.5, delayTime = 0.5)
      val inHPF     = HPF.ar(inDly, 80)
      val dur       = "rec-dur".ir // (8)
      DC.kr(dur).poll(run, "listen-negatum run for")
      val numFrames = dur * SampleRate.ir
      val recBuf    = BufferOut(artifact = "file", action = "done", numFrames = numFrames, numChannels = numCh)
      val rec       = RecordBuf.ar(in = inHPF, buf = recBuf, run = run, loop = 0)
      val full      = Done.kr(rec)
      StopSelf(full)
    }

//    pNegListen("bus-in")  = int(2)
//    val artCaptureNeg = pNegListen.update("file", locImperfect / "anemone/rec/capture-negatum.aif")
//    pNegListen("rec-dur") = double(6)

//    val neg = negatum("negatum").in(f)(cue = ...)

    val aNegRecDone = action("negatum-rec-done").at(pNegListen -> "done")(ActionNegatumRecDone)
    aNegRecDone("context")    = ensNegListen
    aNegRecDone("negatum")    = ??? // neg
    aNegRecDone("file")       = ??? // artCaptureNeg
    aNegRecDone("iterations") = int(2)
    aNegRecDone("som")        = ???     // SOM
    aNegRecDone("svm")        = ???     // SVMModel
  }

  trait NoSys extends SSys[NoSys]

  object ActionNegatumRec extends Action.Body {
    def apply[S <: Sys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
      type T = NoSys
      // yes, that's ugly
      begin[T](universe.asInstanceOf[Universe[T]])(tx.asInstanceOf[T#Tx])
    }

    def begin[S <: SSys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
      val dsl = DSL[S]
      import dsl._
      import universe._
      logComp("negatum-rec-begin")

      val attr          = self.attr
      val Some(ens)     = attr.$[Ensemble]        ("context")
      val Some(p)       = attr.$[Proc]            ("proc")
      val Some(dir)     = attr.$[ArtifactLocation]("dir")

      import Util._
      import DefaultRandom._
      val recDur = rrand(5.0, 7.0)
      val micBus = rrand(0, 3)
      p.adjustDouble("rec-dur", recDur)
      p.adjustInt   ("bus-in" , micBus)
    }
  }

  object ActionNegatumRecDone extends Action.Body {
    // step 1
    def apply[S <: Sys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
      import universe._
      logComp("negatum-rec-done")
      val attr          = self.attr
      val Some(ens)     = attr.$[Ensemble]("context")
      val Some(neg)     = attr.$[Negatum ]("negatum")
      val Some(artObj)  = attr.$[Artifact]("file")

      ens.playing match {
        case BooleanObj.Var(vr) => vr() = false
        case _ => println("Ensemble negatum listen - playing not mutable")
      }

      val numIter = attr.$[IntObj]("iterations").map(_.value).getOrElse(10)

      val art        = artObj.value
      val negCfg     = de.sciss.negatum.Negatum.attrToConfig(neg)
      val tempSpec   = de.sciss.synth.io.AudioFile.readSpec(art)
      val tempCue    = AudioCue(art, tempSpec, offset = 0L, gain = 1.0)
      val tempCueObj = AudioCue.Obj.newConst[S](tempCue)

      neg.template() = tempCueObj
      neg.population.clear()

      logComp("Starting Negatum rendering...")
      val renderNeg = neg.run(negCfg, iter = numIter)
      val selfH = tx.newHandle(self)
      var lastProg = 0
      renderNeg.reactNow { implicit tx => {
        case Rendering.Progress(amt) =>
          val amtI = (amt * 100 + 0.5).toInt
          val amtM = amtI - (amtI % 10)
          if (amtM != lastProg) {
            logComp(s"Negatum progress: $amtM %")
            lastProg = amtM
          }
        case Rendering.Completed(Success(_)) =>
          negatumDone(selfH)

        case Rendering.Completed(Failure(ex)) =>
          logCompErr("!! Negatum failed:")
          ex.printStackTrace()
      }}
    }

    // step 2
    def negatumDone[S <: Sys[S]](selfH: stm.Source[S#Tx, Action[S]])
                                (implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
      val self = selfH()
      val attr = self.attr
      logComp("Negatum done.")

      val Some(neg) = attr.$[Negatum] ("negatum")
      val Some(svm) = attr.$[SVMModel]("svm")

      logComp("Starting SVM selection...")
      val renderSVM = svm.predict(neg)
      renderSVM.reactNow { implicit tx => {
        case Rendering.Progress(amt) =>
        case Rendering.Completed(scala.util.Success(n)) =>
          svmDone(selfH, svmNum = n)

        case de.sciss.negatum.Rendering.Completed(scala.util.Failure(ex)) =>
          logCompErr("!! SVM failed:")
          ex.printStackTrace()
      }}
    }

    // step 3
    def svmDone[S <: Sys[S]](selfH: stm.Source[S#Tx, Action[S]], svmNum: Int)
                            (implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
      val self = selfH()
      val attr = self.attr
      logComp(s"SVM done ($svmNum).")

      val Some(neg) = attr.$[de.sciss.negatum.Negatum]("negatum")
      val Some(som) = attr.$[de.sciss.negatum.SOM    ]("som")

      println("Starting SOM addition...")
      val renderSOM = som.addAll(neg.population, selected = true)
      renderSOM.reactNow { implicit tx => {
        case de.sciss.negatum.Rendering.Progress(amt) =>
        case de.sciss.negatum.Rendering.Completed(scala.util.Success(n)) =>
          somDone(selfH, somNum = n)

        case de.sciss.negatum.Rendering.Completed(scala.util.Failure(ex)) =>
          logCompErr("!! SOM failed:")
          ex.printStackTrace()
      }}
    }

    // step 4
    def somDone[S <: Sys[S]](selfH: stm.Source[S#Tx, Action[S]], somNum: Int)
                            (implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
//      val self = selfH()
//      val attr = self.attr
      logComp(s"SOM addition done ($somNum).")
      println("CONTINUE HERE")
    }
  }
}