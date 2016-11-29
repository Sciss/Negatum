/*
 *  ActionSOMTimeline.scala
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

import de.sciss.lucre.expr.{DoubleObj, DoubleVector, IntObj}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.gui.TimelineObjView
import de.sciss.negatum.ScanSOM.Input
import de.sciss.span.Span
import de.sciss.synth.{Curve, SynthGraph, proc, ugen}
import de.sciss.synth.proc.Action.Universe
import de.sciss.synth.proc._
import de.sciss.synth.proc.Implicits._
import Composition.{logComp, mkDateString}

object ActionSOMTimeline extends NamedAction("som-timeline") {
  def begin[S <: SSys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
    val dsl = DSL[S]
    import universe._
    val attr = self.attr

    val Some(ensSOMPlay)  = attr.$[Ensemble]("som-play")
    val Some(fSOM)        = attr.$[Folder]("som-folder")
    ensSOMPlay.folder.clear()

    import Util._
    import DefaultRandom._
    val numSOM = fSOM.size
    if (numSOM == 0) return

    val somIdx = rrand(0, numSOM - 1)
    val Some(som) = fSOM.get(somIdx).collect {
      case _som: SOM[S] => _som
    }

    val tl    = Timeline[S]
    tl.name   = s"timeline-${mkDateString()}"
    val cfg   = ScanSOM.Config()
//    val dur   = 180.0
    val dur   = rrand(150.0, 210.0)
    val start = ( 1.0        * TimeRef.SampleRate).toLong
    val stop  = ((1.0 + dur) * TimeRef.SampleRate).toLong
    val dim   = som.config.dimensions
    assert(dim == 2, s"SOM does not have two dimensions: $dim")

//    val pt1   = List(0.0, 0.0)
//    val pt2   = List(0.0, 1.0)
//      val trj   = Vector(pt1, pt2)
    val trajSOM = randomRectSides(3).map { v2 => List(v2.x.toDouble, v2.y.toDouble) }

    logComp("Generating TL...")

    val pBus = Proc[S]
    val gBus = SynthGraph {
      import de.sciss.numbers.Implicits._
      import proc.graph._
      import Ops._
      import ugen._
      val bus   = ObjKeys.attrBus .kr(0f)
      val gain  = ObjKeys.attrGain.kr(1f)
      val in    = ScanInFix(numChannels = 1)
      // proc.graph.Buffer.kr("traj")
      val bufX  = graph.Buffer("traj-x")
      val bufY  = graph.Buffer("traj-y")
      val trajDur  = "dur".kr
      val trajSize = BufFrames.kr(bufX) - 1
      val phasFreq = trajSize / (ControlRate.ir * trajDur)
      val bufPos = Phasor.kr(speed = phasFreq, lo = 0, hi = trajSize)
//      val px    = "traj-x".kr(0f)
//      val py    = "traj-y".kr(0f)
      val px    = BufRd.kr(numChannels = 1, buf = bufX, index = bufPos, loop = 1, interp = 2)
      val py    = BufRd.kr(numChannels = 1, buf = bufY, index = bufPos, loop = 1, interp = 2)
      val amp   = NegatumDelaunay(px, py)
      val ampL  = Lag.kr(amp, time = 1f)
      val compThresh = -15.dbamp
      val expanderRatio = 0.5f
      val inComp = Compander.ar(in, in, thresh = compThresh, ratioBelow = expanderRatio, ratioAbove = 1.0f)
      val sig   = inComp * gain * ampL
      PhysicalOut.ar(indices = bus, in = sig)
    }
    pBus.graph() = gBus
    val attrBus = pBus.attr
    val trajSpat  = randomRectSides(3)
    val trajSpatX = trajSpat.map(_.x.toDouble)
    val trajSpatY = trajSpat.map(_.y.toDouble)
    attrBus.put("traj-x", DoubleVector.newVar(trajSpatX))
    attrBus.put("traj-y", DoubleVector.newVar(trajSpatY))
    attrBus.put("dur"   , DoubleObj.newVar(dur))
    val fBus = Folder[S]
    import proc.Implicits._
    pBus.attr.put(Proc.mainIn, fBus)
    pBus.name = "main"

    var prevObj = Option.empty[Proc[S]]
    var prevSpan: Span = Span(0L, 0L)

    val trkIdx    = (0 until 4).map(i => IntObj.newConst[S](i * 2))
    val trkHeight = IntObj.newConst[S](2)

    ScanSOM(som, tl, Span(start, stop), trajSOM, cfg) {
      case Input(pIn: Proc[S], pSpan, idx) =>
        //          val cpy   = Copy[S, S]
        //          val pOut  = cpy(pIn)
        //          cpy.finish()
        val pOut = Proc[S]
        val gIn  = pIn.graph().value
        val gOut = SynthGraph {
          import proc.graph._
          import ugen._
          import Ops._
          val b = SynthGraph.builder
          gIn.sources.foreach {
            case NegatumIn() => // remove RandSeed?
            case NegatumOut(in) =>
              val sig0  = Mix.mono(in)
              val isOk  = CheckBadValues.ar(sig0, post = 0) sig_== 0
              val sig1  = Gate.ar(sig0, isOk)
              val sig2  = sig1.clip2(1)
              val sig3  = LeakDC.ar(sig2) * 0.47
              val fade  = FadeInOut.ar
              val gain  = ObjKeys.attrGain.ar(1f)
              val mute  = ObjKeys.attrMute.ar(0f)
              val amp   = fade * gain * (1 - mute)
              val sig   = sig3 * amp
              ScanOut(sig)

            case other =>
              // println(s"ADD ${other.productPrefix}")
              b.addLazy(other)
          }
        }
        // println(s"in ${gIn.sources.size}; out ${gOut.sources.size}")
        pOut.graph() = gOut
        val trk   = idx % 4   // minimise overlap without extensive analysis
        val pAttr = pOut.attr
        pAttr.put(TimelineObjView.attrTrackIndex , IntObj.newVar[S](trkIdx(trk)))
        pAttr.put(TimelineObjView.attrTrackHeight, IntObj.newVar[S](trkHeight))
        pOut.name = pIn.name
        val output = pOut.outputs.add(Proc.mainOut)
        fBus.addLast(output)

        val fadeLen     = math.max((0.1 * TimeRef.SampleRate).toLong, prevSpan.intersect(pSpan).length)
        val fadeInLen   = math.min(fadeLen, pSpan.length/2)
        val fadeIn      = FadeSpec(numFrames = fadeInLen, curve = Curve.welch)
        val fadeInObj   = FadeSpec.Obj.newVar[S](fadeIn)
        pAttr.put(ObjKeys.attrFadeIn, fadeInObj)

        prevObj.foreach { pPrev =>
          val fadeOutLen  = math.min(fadeLen, prevSpan.length/2)
          val fadeOut     = FadeSpec(numFrames = fadeOutLen, curve = Curve.welch)
          val fadeOutObj  = FadeSpec.Obj.newVar[S](fadeOut)
          pPrev.attr.put(ObjKeys.attrFadeOut, fadeOutObj)
        }

        prevSpan = pSpan
        prevObj  = Some(pOut)
        prevObj

      case x =>
        println(s"Not a proc in SOM: ${x.obj}")
        None
    }

    prevObj.foreach { pPrev =>
      val fadeOutLen  = prevSpan.length/2
      val fadeOut     = FadeSpec(numFrames = fadeOutLen, curve = Curve.welch)
      val fadeOutObj  = FadeSpec.Obj.newVar[S](fadeOut)
      pPrev.attr.put(ObjKeys.attrFadeOut, fadeOutObj)
    }

    val timeNext = tl.lastEvent.getOrElse(0L) + (TimeRef.SampleRate * rrand(10, 20)).toLong
    tl.add(Span(timeNext, timeNext + TimeRef.SampleRate.toLong), self)

    logComp("Adding SOM timeline...")
    tl.add(Span.all, pBus)

    ensSOMPlay.folder.addLast(tl)
    ensSOMPlay.stop()
    ensSOMPlay.play()
  }
}