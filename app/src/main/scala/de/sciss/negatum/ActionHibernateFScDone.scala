/*
 *  ActionHibernateFScDone.scala
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

import de.sciss.fscape.lucre.FScape
import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.expr.{BooleanObj, IntObj, StringObj}
import de.sciss.lucre.stm.Folder
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.ObjTimelineView
import de.sciss.negatum.Hibernation.logComp
import de.sciss.numbers.Implicits._
import de.sciss.span.Span
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.Action.Universe
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Action, ActionRaw, AudioCue, Ensemble, FadeSpec, ObjKeys, Proc, TimeRef, Timeline, graph}
import de.sciss.synth.{Curve, SynthGraph}

object ActionHibernateFScDone extends NamedAction("hibernate-fsc-done") {
  def begin[S <: SSys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
    // val dsl = DSL[S]
    import universe._

    value match {
      case scala.util.Success(_) =>
        logComp("FScape Rendering success.")
        val attr = self.attr
        val Some(pool)      = attr.$[Folder]    ("pool")
        val Some(artIn)     = attr.$[Artifact]  ("file-in")
        val Some(artOut)    = attr.$[Artifact]  ("file-out")
        val Some(side)      = attr.$[BooleanObj]("side")
        val Some(restart)   = attr.$[ActionRaw] ("restart")
        val Some(ensPlay)   = attr.$[Ensemble]  ("ens-play")

        val Some(sideVar)   = BooleanObj.Var.unapply(side)
        val sideValThis    = sideVar().value    // flip here
        val sideValOpp      = !sideValThis
        sideVar()           = sideValOpp

        val artOutVal = artOut.value
        val spec      = AudioFile.readSpec(artOutVal)
        val cue       = AudioCue.Obj(artOut, spec, offset = 0L, gain = 1.0)
        cue.name      = artOutVal.getName

        val poolSides = pool.iterator.collect {
          case f: Folder[S] => f
        } .toIndexedSeq

        val fCuesThis = poolSides(if (sideValThis) 1 else 0)

        val artInVal = artIn.value  // the capture file is not used any longer, don't fill the HD!
        tx.afterCommit {
          if (!artInVal.delete()) artInVal.deleteOnExit()
        }

        // we add the recording to the opposite side
        val fCuesOpp = poolSides(if (sideValOpp) 1 else 0)
        val sz = fCuesOpp.size
        if (sz > 3) {
          val oldCue  = fCuesOpp.removeAt(0).asInstanceOf[AudioCue.Obj[S]]
          val oldFile = oldCue.value.artifact
          tx.afterCommit {
            if (!oldFile.delete()) oldFile.deleteOnExit()
          }
        }
        fCuesOpp.addLast(cue)
        logComp("... added new cue.")

        //       ensList.playing match {
        //         case BooleanObj.Var(vr) => vr() = true
        //         case _ => println("Ensemble listen - playing not mutable")
        //       }

        // we create timeline for playback on this (the old) side
        val (tl, _ /* tlLen */) = mkTimeline(sideVal = sideValThis, cues = fCuesThis)
        // val timeNext = /* tlLen + */ TimeRef.SampleRate.toLong
        // tl.add(Span(timeNext, timeNext + TimeRef.SampleRate.toLong), restart)

        // we have to restart the ensemble, because otherwise
        // it won't reset play position for the timeline to zero!
        ensPlay.stop()
        val fPlay = ensPlay.folder
        fPlay.clear()
        fPlay.addLast(tl)

        implicit val u: Universe[S] = universe
        val restartU = Action.Universe(self = restart)
        restart.execute(restartU)

        // we have to restart the ensemble, because otherwise
        // it won't reset play position for the timeline to zero!
        ensPlay.play()

      case scala.util.Failure(FScape.Rendering.Cancelled()) =>
        println("Cancelled.")
      case scala.util.Failure(ex) =>
        println("FScape Rendering failed:")
        ex.printStackTrace()
      case other =>
        println(s"Expected FScape Rendering.State, but got $other")
    }
  }

  def mkTimeline[S <: SSys[S]](sideVal: Boolean, cues: Folder[S])(implicit tx: S#Tx): (Timeline.Modifiable[S], Long) = {
    val tl      = Timeline[S]
    tl.name     = s"tl-$sideVal-${Hibernation.mkDateString()}"
    var stop    = 0L
    val busOff  = if (sideVal) 4 else 0
    cues.iterator.zipWithIndex.foreach {
      case (c: AudioCue.Obj[S], idx) =>
        val cv        = c.value
        val proc      = Proc[S]
        proc.outputs.add(Proc.mainOut)
        val procG = SynthGraph {
          import de.sciss.synth._
          import graph.Ops._
          import ugen.Out
          val sig   = graph.VDiskIn  .ar(Proc.graphAudio)
          val gain  = 1.0 // ObjKeys.attrGain.kr(1.0)
          // val mute  = ObjKeys.attrMute.kr(0.0)
          val env   = graph.FadeInOut.ar
          val amp   = env * gain // ((1 - mute) * gain)
          val out   = sig * amp
          // (out \ 0).poll(label = "disk")
          // graph.ScanOut(out)
          val bus   = ObjKeys.attrBus.kr(0f)
          Out.ar(bus, out)
        }
        proc.graph()  = procG

        val pAttr     = proc.attr
        val tlFrames  = (cv.spec.numFrames / cv.spec.sampleRate * TimeRef.SampleRate).toLong
        val span      = Span(0L, tlFrames)

        val fadeInLen   = math.min(tlFrames/2, (20 * TimeRef.SampleRate).toLong)
        val fadeOutLen  = math.min(tlFrames/2, (20 * TimeRef.SampleRate).toLong)
//        val fadeInVal   = FadeSpec(numFrames = fadeInLen , curve = Curve.welch)
//        val fadeOutVal  = FadeSpec(numFrames = fadeOutLen, curve = Curve.welch)
        val fadeInVal   = FadeSpec(numFrames = fadeInLen , curve = Curve.exp, floor = -40.dbAmp)
        val fadeOutVal  = FadeSpec(numFrames = fadeOutLen, curve = Curve.exp, floor = -40.dbAmp)

        pAttr.put(Proc.graphAudio     , c)
        pAttr.put(ObjKeys.attrFadeIn  , FadeSpec.Obj.newVar[S](fadeInVal ))
        pAttr.put(ObjKeys.attrFadeOut , FadeSpec.Obj.newVar[S](fadeOutVal))
        val trkIdx = idx + 1
        pAttr.put(ObjTimelineView.attrTrackIndex, IntObj.newVar[S](trkIdx))
        pAttr.put(ObjKeys.attrName, StringObj.newVar[S](cv.artifact.getName))
        val busIdx = busOff + (idx % 4)
        pAttr.put(ObjKeys.attrBus, IntObj.newVar(busIdx))

        tl.add(span, proc)

        stop = math.max(stop, tlFrames)

      case _ =>
    }

    (tl, stop)
  }
}