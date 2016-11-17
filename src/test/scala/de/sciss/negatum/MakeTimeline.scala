package de.sciss.negatum

import de.sciss.file._
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.mellite.Mellite
import de.sciss.mellite.gui.TimelineObjView
import de.sciss.negatum.ScanSOM.Input
import de.sciss.span.Span
import de.sciss.synth.proc.{FadeSpec, Folder, ObjKeys, Proc, TimeRef, Timeline, Workspace}
import de.sciss.synth.{Curve, SynthGraph, proc, ugen}

object MakeTimeline extends App {
  val sessionF = userHome / "mellite" / "sessions" / "som_test.mllt"

  Mellite.initTypes()
  Negatum.init()

  Workspace.read(sessionF, BerkeleyDB.factory(sessionF, createIfNecessary = false)) match {
    case workspace: Workspace.Durable => run(workspace)
    case other => println(s"Not a durable workspace: $other")
  }

  def run[S <: Sys[S]](workspace: Workspace[S]): Unit = {
    implicit val cursor: stm.Cursor[S] = workspace.cursor

    cursor.step { implicit tx =>
      val som: SOM[S] = workspace.collectObjects {
        case _som: SOM[S] => _som
      } .headOption.getOrElse(sys.error("Workspace does not contain SOM"))

      val tl    = Timeline[S]
      val cfg   = ScanSOM.Config()
      val start = (  1.0 * TimeRef.SampleRate).toLong
      val stop  = (181.0 * TimeRef.SampleRate).toLong
      val dim   = som.config.dimensions
      assert(dim == 2, s"SOM does not have two dimensions: $dim")

      val pt1   = List(0.0, 0.0)
//      val pt2   = List(1.0, 0.0)
      val pt2   = List(0.0, 1.0)
      val trj   = Vector(pt1, pt2)

      println("Generating TL...")

      val pBus = Proc[S]
      val gBus = SynthGraph {
        import proc.graph._
        import Ops._
        import ugen._
        val bus   = ObjKeys.attrBus .kr(0f)
        val gain  = ObjKeys.attrGain.kr(1f)
        val in    = ScanInFix(numChannels = 1)
        val sig   = in * gain
        PhysicalOut.ar(indices = bus, in = sig)
      }
      pBus.graph() = gBus
      val fBus = Folder[S]
      import proc.Implicits._
      pBus.attr.put(Proc.mainIn, fBus)
      pBus.name = "main"

      var prevObj = Option.empty[Proc[S]]
      var prevSpan: Span = Span(0L, 0L)

      val trkIdx    = (0 until 4).map(i => IntObj.newConst[S](i * 2))
      val trkHeight = IntObj.newConst[S](2)

      ScanSOM(som, tl, Span(start, stop), trj, cfg) {
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
          val fadeIn      = FadeSpec(numFrames = fadeInLen, Curve.welch)
          val fadeInObj   = FadeSpec.Obj.newVar[S](fadeIn)
          pAttr.put(ObjKeys.attrFadeIn, fadeInObj)

          prevObj.foreach { pPrev =>
            val fadeOutLen  = math.min(fadeLen, prevSpan.length/2)
            val fadeOut     = FadeSpec(numFrames = fadeOutLen, Curve.welch)
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
        val fadeOut     = FadeSpec(numFrames = fadeOutLen, Curve.welch)
        val fadeOutObj  = FadeSpec.Obj.newVar[S](fadeOut)
        pPrev.attr.put(ObjKeys.attrFadeOut, fadeOutObj)
      }

      println("Adding TL...")
      tl.add(Span.all, pBus)
      tl.name = "Timeline"
      workspace.root.addLast(tl)
    }

    println("Closing...")
    workspace.close()

    sys.exit()
  }
}
