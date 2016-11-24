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

import de.sciss.file._
import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.expr.{BooleanObj, IntObj}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.ugen
import de.sciss.synth.proc.Action.Universe
import de.sciss.synth.proc._

object Composition {
  def apply[S <: SSys[S]](workspace: Workspace[S])(implicit tx: S#Tx): Unit = {
    val dsl = DSL[S]
    import dsl._

    val f = workspace.root

    val locImperfect = artifactLoc(userHome/"Documents"/"projects"/"Imperfect").in(f)

    val pNegListen = proc("negatum-listen").in(f) {
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

    pNegListen("bus-in")  = int(2)
    pNegListen("file")    = locImperfect / "anemone/rec/capture-negatum.aif"
    pNegListen("rec-dur") = double(6)

    val aNegRecDone = action("negatum-rec-done").at(pNegListen -> "done")(ActionNegatumRecDone)
    aNegRecDone("context")    = ???
    aNegRecDone("negatum")    = ???
    aNegRecDone("file")       = ???
    aNegRecDone("iterations") = ???
    aNegRecDone("som")        = ???
    aNegRecDone("svm")        = ???
  }

  object ActionNegatumRecDone extends Action.Body {
    def apply[S <: Sys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
      import universe._
      println("rec-done")
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

      println("Starting Negatum rendering...")
      val renderNeg = neg.run(negCfg, iter = numIter)
      val selfH = tx.newHandle(self)
      var lastProg = 0
      renderNeg.reactNow { implicit tx => {
        case de.sciss.negatum.Rendering.Progress(amt) =>
          val amtI = (amt * 100 + 0.5).toInt
          val amtM = amtI - (amtI % 10)
          if (amtM != lastProg) {
            println(s"Negatum progress: $amtM %")
            lastProg = amtM
          }
        case de.sciss.negatum.Rendering.Completed(scala.util.Success(_)) =>
          val self = selfH()
          val attr = self.attr
          println("Negatum done.")
          for {
            neg  <- attr.$[de.sciss.negatum.Negatum]("negatum")
            svm  <- attr.$[de.sciss.negatum.SVMModel]("svm")
          } {
            println("Starting SVM selection...")
            val renderSVM = svm.predict(neg)
            renderSVM.reactNow { implicit tx => {
              case de.sciss.negatum.Rendering.Progress(amt) =>
              case de.sciss.negatum.Rendering.Completed(scala.util.Success(n)) =>
                val self = selfH()
                val attr = self.attr
                println(s"SVM done ($n).")
                for {
                  neg  <- attr.$[de.sciss.negatum.Negatum]("negatum")
                  som  <- attr.$[de.sciss.negatum.SOM    ]("som")
                } {
                  println("Starting SOM addition...")
                  val renderSOM = som.addAll(neg.population, selected = true)
                  renderSOM.reactNow { implicit tx => {
                    case de.sciss.negatum.Rendering.Progress(amt) =>
                    case de.sciss.negatum.Rendering.Completed(scala.util.Success(n)) =>
                      val self = selfH()
                      val attr = self.attr
                      println(s"SOM addition done ($n).")
                      println("CONTINUE HERE")

                    case de.sciss.negatum.Rendering.Completed(scala.util.Failure(ex)) =>
                      println("!! SOM failed:")
                      ex.printStackTrace()
                  }}
                }

              case de.sciss.negatum.Rendering.Completed(scala.util.Failure(ex)) =>
                println("!! SVM failed:")
                ex.printStackTrace()
            }}
          }

        case de.sciss.negatum.Rendering.Completed(scala.util.Failure(ex)) =>
          println("!! Negatum failed:")
          ex.printStackTrace()
      }}
    }
  }
}
