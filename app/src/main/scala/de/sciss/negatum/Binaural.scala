/*
 *  Binaural.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum

import de.sciss.file._
import de.sciss.lucre.synth.{AudioBus, Buffer, Bus, BusNodeSetter, Group, Node, Synth, Txn}
import de.sciss.negatum.Delaunay.Vector2
import de.sciss.synth.{AddAction, ControlSet, SynthGraph, addBefore, addToHead, addToTail, message}
import de.sciss.{numbers, synth}

import scala.collection.immutable.{IndexedSeq => Vec}

object Binaural {
  var DEBUG = false // true

  final val audioWork: File = userHome / "Documents" / "projects" / "Imperfect" / "audio_work"

  final val MetersPerPixel = 10.20 / 824.0 // 1.5/92 // hScale

  val ChannelToMatrixMap: Map[Spk, Vector2] = Speakers.select.iterator.zipWithIndex.map { case (pt, i) =>
    Spk(i) -> pt
  } .toMap

  object Radians {
    val North = Radians(math.Pi * 0.5)
  }
  final case class Radians(value: Double) extends AnyVal {
    /** Wraps the value to ensure it lies within -Pi ... +Pi */
    def normalize: Radians = {
      import numbers.Implicits._
      Radians(value.wrap2(math.Pi))
    }

    def - (that: Radians): Radians = Radians(this.value - that.value)
    def + (that: Radians): Radians = Radians(this.value + that.value)

    def angleTo(that: Radians): Radians = {
      val thisN = this.normalize
      val thatN = that.normalize
      (thatN - thisN).normalize
    }
  }

  final case class Spk(num: Int) extends AnyVal {
    /** Zero-based channel offset */
    def toIndex: Int = num - 1
  }

  final val Channels: Vec[Spk] = ChannelToMatrixMap.keysIterator.toVector.sortBy(_.num)

  final val ChannelIndices: Vec[Int] = Channels.map(_.toIndex)

  final val NumChans = ChannelIndices.size

  final case class Person(pos: Vector2, azi: Radians)

  // - there are 360 / 15 = 24 azimuth   samples
  // - there are  90 / 15 =  6 elevation samples (downwards elevation not needed)
  // - not all elevation samples are taken for any azimuth
  // -

  final case class IR(t: Int, p: Int) {
    require(t % 15 == 0 && t >= 0 && t < 360)
    require(p % 15 == 0 && ((p >= 0 && p <= 90) || (p >= 315 && p < 360)))

    val angleRadians: Float = t.toRadians

//    def toPolar: Polar = LatLon(lat = if (p < 90) p else p - 360, lon = t).toPolar
//    def toCartesian: Pt3 = toPolar.toCartesian

    def file(id: Int = 1004): File = audioWork / s"IRC_${id}_C" / f"IRC_${id}_C_R0195_T$t%03d_P$p%03d.wav"

    override def toString = f"T$t%03d_P$p%03d"
  }

  //  final val Samples = Vector[IR](
  //    IR(000, 000), IR(000, 015), IR(000, 030), IR(000, 045), IR(000, 060), IR(000, 075), IR(000, 090),
  //    IR(015, 000), IR(015, 015), IR(015, 030), IR(015, 045),
  //    IR(030, 000), IR(030, 015), IR(030, 030), IR(030, 045), IR(030, 060),
  //    IR(045, 000), IR(045, 015), IR(045, 030), IR(045, 045),
  //    IR(060, 000), IR(060, 015), IR(060, 030), IR(060, 045), IR(060, 060), IR(060, 075),
  //    IR(075, 000), IR(075, 015), IR(075, 030), IR(075, 045),
  //    IR(090, 000), IR(090, 015), IR(090, 030), IR(090, 045), IR(090, 060),
  //    IR(105, 000), IR(105, 015), IR(105, 030), IR(105, 045),
  //    IR(120, 000), IR(120, 015), IR(120, 030), IR(120, 045), IR(120, 060), IR(120, 075),
  //    ...
  //  )

  final val Samples: Vec[IR] = (0 until 360 by 15).flatMap { t =>
    val pm = if (t % 360 == 0) 90 else if (t % 60 == 0) 75 else if (t % 30 == 0) 60 else 45
    (0 to pm by 15).map { p =>
      IR(t, p)
    }
  }

//  final val SamplePoints: Vec[(Pt3, Int)] = Samples.map(_.toCartesian).zipWithIndex

  final val SamplePoints: Vec[(Radians, Int)] = Samples.map(x => Radians(x.angleRadians)).zipWithIndex

  final case class Position(index: Int, distance: Double) {
    /** Acoustical delay in seconds, based on `distance`
      * and a speed of sound of 342 m/s
      */
    def delay: Double = distance / 342

    /** Amplitude factor (less than 1) in Forum Stadtpark, based on `distance` and
      * a distance of 1.5 meters corresponding to a factor of 1.0
      */
    def attenuation: Double = {
      import numbers.Implicits._
      val d = distance.clip(1.5, 5.8)
      val decibels = 1.1034 * d.squared - 12.6433 * d + 14.3775
      (decibels + 1.2).dbAmp
    }

    override def toString = f"Position(${Samples(index)}, distance = $distance%1.2f meters"
  }

  def angleTo(a: Vector2, b: Vector2): Radians = {
//    val r = math.atan2(-(b.y - a.y), b.x - b.x)
    val r = math.atan2(-(b.y - a.y), b.x - a.x)
    Radians(r)
  }

  def distanceTo(a: Vector2, b: Vector2): Double = {
    val dx = b.x - a.x
    val dy = b.y - a.y
    math.sqrt(dx * dx + dy * dy)
  }

  /** Calculates the closest HRIR sample index and distance of a person with respect
    * to a given speaker
    *
    * @param listener position and orientation of listener
    * @param spk      speaker index
    * @return a pair of HRIR sample index and distance in meters
    */
  def calc(listener: Person, spk: Spk): Position = {
    val q     = ChannelToMatrixMap(spk) // .toPoint.equalize
    val p     = listener.pos // .equalize
//    val azi0  = listener.azi.value.toDegrees
    val azi0  = angleTo(p, q)
//    val azi   = azi0 - listener.azi
    val azi   = listener.azi - azi0
    val dh    = distanceTo(p, q) * MetersPerPixel
    val dv    = 1.5   // ja?
    val dist  = math.sqrt(dh * dh + dv * dv)
//    val ele   = math.atan2(dv, dh)
//    val ll    = LatLon(lat = ele.toDegrees, lon = azi.value.toDegrees)
//    val r     = ll.toCartesian
//    val idx   = SamplePoints.minBy(_._1 distanceTo r)._2
    val idx: Int = SamplePoints.minBy { case (ang, _) =>
        ang.angleTo(azi).value
//      val dAng  = ang - azi.value
//      val res   = dAng.wrap(0, 180)
//      res
    }._2
    Position(index = idx, distance = dist)
  }

  case class PreparePartConv(sourceBuf: Buffer, fftSize: Int)
    extends message.BufferGen.Command {

    def name: String = "PreparePartConv"

    def isSynchronous: Boolean = true // !

    def args: Seq[Any] = Seq(sourceBuf.id, fftSize)
  }

  private def mkTail(listener: Person, target: Node, addAction: AddAction,
                     delayBus: AudioBus, stereoBus: AudioBus)(implicit tx: Txn): Synth = {
    require(delayBus .numChannels == NumChans)
    require(stereoBus.numChannels == 2)

    val irSize    = 80000
    val fftSize   = 2048
    val numPart   = (irSize * 2.0 / fftSize).ceil.toInt  // 49
    val partSize  = fftSize * numPart  // 100352
    val s         = target.server
    val partBufL  = Buffer(s)(numFrames = partSize)
    val partBufR  = Buffer(s)(numFrames = partSize)
    val fullBufL  = Buffer(s)(numFrames = irSize  )
    val fullBufR  = Buffer(s)(numFrames = irSize  )
    fullBufL.read((audioWork / "ForumVerb-L.aif").absolutePath)
    fullBufR.read((audioWork / "ForumVerb-R.aif").absolutePath)
    // currently no predefined method for this command!
    tx.addMessage(partBufL, message.BufferGen(partBufL.id, PreparePartConv(fullBufL, fftSize)),
      dependencies = fullBufL :: Nil)
    tx.addMessage(partBufR, message.BufferGen(partBufR.id, PreparePartConv(fullBufR, fftSize)),
      dependencies = fullBufL :: Nil)
    // fullBufL.dispose()
    // fullBufR.dispose()

    val tailGraph = SynthGraph {
      import synth._
      import Ops.stringToControl
      import ugen._
//      val in    = In.ar(ChannelIndices)
      val in    = In.ar(0, NumChans)
      val inF   = Flatten(in)
      val dlyT  = "delay".ir(Vec.fill(NumChans)(0f))
      val amp   = "amp"  .kr(Vec.fill(NumChans)(0f))
      val inA   = DelayN.ar(inF, dlyT, dlyT) * amp
      val mix   = Mix(inA)
      // RunningSum.ar(mix).poll(1, "sum")
      val bufL  = "bufL".ir
      val bufR  = "bufR".ir
      val convL = PartConv.ar(mix, fftSize, bufL)
      val convR = PartConv.ar(mix, fftSize, bufR)
      // RunningSum.ar(convL).poll(1, "conv")
      val outR  = "reverb-out".kr
      val outD  = "delay-out".kr
      // out.poll(0, "out")
      Out.ar(outD, inA)

      // todo - correct delay (PartConv versus Convolution2 / binaural-kernel)
      Out.ar(outR, Seq(convL, convR))
    }

    val pos     = Channels.map { spk => calc(listener, spk) }
    val dlySet: ControlSet = "delay" -> pos.map(_.delay      .toFloat)
    val attSet: ControlSet = "amp"   -> pos.map(_.attenuation.toFloat)

    val res         = Synth(s, tailGraph, Some("reverb-tail"))
    val reverbBusW  = BusNodeSetter.writer("reverb-out", stereoBus, res)
    val delayBusW   = BusNodeSetter.writer("delay-out" , delayBus , res)
    val args        = dlySet :: attSet :: List[ControlSet]("bufL" -> partBufL.id, "bufR" -> partBufR.id)
    res.play(target = target, args = args,
      addAction = addAction, dependencies = partBufL :: partBufR :: Nil)
    reverbBusW.add()
    delayBusW .add()
    res.onEndTxn { implicit tx =>
      reverbBusW.remove()
      delayBusW .remove()
      fullBufL  .dispose()
      fullBufR  .dispose()
      partBufL  .dispose()
      partBufR  .dispose()
    }
    res
  }

  def build(target: Node, addAction: AddAction, listener: Person)(implicit tx: Txn): Group = {
    val s         = target.server
    val g         = Group(s)
    g.play(target, addAction)
    val stereoBus = Bus.audio(s, 2)
    val delayBus  = Bus.audio(s, NumChans)
    mkTail(listener, g, addToHead, delayBus = delayBus, stereoBus = stereoBus)

    val rplcGraph = SynthGraph {
      import synth._
      import Ops.stringToControl
      import ugen._
      val in  = "in".kr
      val sig = In.ar(in, 2)
      // Mix(sig).poll(1, "route")
      // ReplaceOut.ar(0, sig)
      val asr   = Env.asr(attack = 0.1, release = 0.1, curve = Curve.lin)
      val fade  = EnvGen.kr(asr, gate = "gate".kr(1f), doneAction = freeGroup)
      val out   = "out".kr
      XOut.ar(out, sig, fade)
    }

    val rplcSynth   = Synth(s, rplcGraph, Some("binaural-mix"))
    val headphones  = 0 // Prefs.headphonesBus.getOrElse(Prefs.defaultHeadphonesBus)
    rplcSynth.play(g, List("out" -> headphones), addToTail, Nil)
    val stereoBusR = BusNodeSetter.reader("in", stereoBus, rplcSynth)
    stereoBusR.add()
    rplcSynth.onEndTxn { implicit tx =>
      stereoBusR.remove()
    }

    var binBufs = Map.empty[Int, (Buffer, Buffer)]

    lazy val chanGraph = SynthGraph {
      import synth._
      import Ops.stringToControl
      import ugen._
      val in      = "in".kr
      val ch      = "chan".kr
      val inSig   = Select.ar(ch, In.ar(in, NumChans)) // todo - not nice. this is simply because delayBus is multi-chan

      val bufL  = "bufL".ir
      val bufR  = "bufR".ir
      val convL = Convolution2.ar(inSig, bufL, frameSize = 512)
      val convR = Convolution2.ar(inSig, bufR, frameSize = 512)
      val outSig: GE = Seq(convL, convR)

      val out   = "out".kr
      Out.ar(out, outSig)
    }

    Channels.zipWithIndex.foreach { case (spk, offset) =>
      val pos = calc(listener, spk)

      if (DEBUG) println(s"$spk - $pos")

      if (pos.distance < 8 /* 6 */) { // use binaural for less than 6 meters distance
        val chanSynth = Synth(s, chanGraph, Some("chan-bin"))
        val (bufL, bufR) = binBufs.getOrElse(pos.index, {
          val ir    = Samples(pos.index)
          val path  = ir.file().absolutePath
          val _bufL = Buffer(s)(numFrames = 512)
          val _bufR = Buffer(s)(numFrames = 512)
          _bufL.readChannel(path, 0 :: Nil)
          _bufR.readChannel(path, 1 :: Nil)
          val tup = (_bufL, _bufR)
          binBufs += pos.index -> tup
          tup
        })

        val args: List[ControlSet] = List("chan" -> offset, "bufL" -> bufL.id, "bufR" -> bufR.id)
        chanSynth.play(rplcSynth, args, addBefore, bufL :: bufR :: Nil)
        val stereoBusW = BusNodeSetter.writer("out", stereoBus, chanSynth)
        val delayBusR  = BusNodeSetter.reader("in" , delayBus , chanSynth)
        stereoBusW.add()
        delayBusR .add()
        chanSynth.onEndTxn { implicit tx =>
          stereoBusW.remove()
          delayBusR .remove()
        }

      } else {
        // ignore - just use the reverb tail and we're fine
      }
    }

    if (DEBUG) println(s"Number of binaural filters: ${binBufs.size}")

    g.onEndTxn { implicit tx =>
      binBufs.valuesIterator.foreach { case (bufL, bufR) =>
        bufL.dispose()
        bufR.dispose()
      }
    }

    g
  }
}
