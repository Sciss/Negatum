package de.sciss.negatum

import de.sciss.file._
import de.sciss.fscape.{GE, Graph, graph, stream}
import de.sciss.fscape.graph.GenWindow
import de.sciss.fscape.gui.SimpleGUI
import de.sciss.numbers
import de.sciss.synth.io.{AudioFile, AudioFileSpec, AudioFileType, SampleFormat}

import scala.swing.Swing

object MorassTest {
  case class MorassConfig(input             : GE,
                          template          : GE,
                          inputWinSize      : Int             = 16384,
                          templateWinSize   : Int             = 16384,
                          analyzeWinType    : GenWindow.Shape = GenWindow.Hann,
                          synthesizeWinType : GenWindow.Shape = GenWindow.Hann,
                          synthesizeWinAmt  : Double          = 1.0,
                          ampModulation     : Double          = 0.0,
                          stepSize          : Int             = 16,
                          radius            : Double          = 1.0,
                          numFrames: Int,
                          keepFileLength: Boolean = true
                         ) {
    require(inputWinSize     >= 2)
    require(templateWinSize  >= 2)
    require(stepSize         >  0 && stepSize <= inputWinSize && stepSize <= templateWinSize )
    require(radius           >= 0 && radius <= 1.0)
    require(synthesizeWinAmt >= 0 && synthesizeWinAmt <= 1.0)
  }

  def complexNormalize(in: GE, headroom: GE): GE = {
    val mag = in.complex.abs
    normalizeImpl(in = in, mag = mag, headroom = headroom)
  }

  def realNormalize(in: GE, headroom: GE): GE =
    normalizeImpl(in = in, mag = in.abs, headroom = headroom)

  def normalizeImpl(in: GE, mag: GE, headroom: GE): GE = {
    import graph._
    val max       = RunningMax(mag).last
    val gain      = max.reciprocal * headroom
    val buf       = BufferDisk(in)
    val sig       = buf * gain
    sig
  }

  def mkFourierFwd(in: File, size: GE, gain: Gain): GE = {
    import graph._
    val disk      = AudioFileIn(file = in, numChannels = 1).take(size)
    val complex   = ZipWindow(disk, DC(0.0))
    val fft       = Fourier(in = complex, size = size, dir = +1.0)
    val sig       =
      if      (gain.isUnity   ) fft
      else if (gain.normalized) complexNormalize(fft, headroom = gain.value)
      else                      fft * gain.value
    sig
  }

  def mkFourierInv(in: GE, size: GE, out: File, spec: AudioFileSpec, gain: Gain): Unit = {
    import graph._
    val iFft  = Fourier(in = in, size = size, dir = -1.0)
    val re    = ChannelProxy(UnzipWindow(iFft), 0)
    val sig   =
      if     (gain.isUnity   ) re
      else if(gain.normalized) realNormalize(re, headroom = gain.value)
      else                     re * gain.value
    val frames = AudioFileOut(file = out, spec = spec, in = sig)
    frames.poll(Metro(44100), "out")
  }

  def mkMorass(config: MorassConfig): GE = {
    import graph._
    import config._

    val winSize       = math.max(inputWinSize, templateWinSize)
    val winAnaIn      = GenWindow(size = inputWinSize   , shape = analyzeWinType)
    val winAnaTemp    = GenWindow(size = templateWinSize, shape = analyzeWinType)
    val winSynth      = if (synthesizeWinAmt == 1.0) {
      GenWindow(size = inputWinSize, shape = synthesizeWinType)
    } else {
      // synthesizeWinAmt * inputWinSize is the actual window function.
      // the left half of the window and the right half of the window
      // will be split and pushed to the inputWinSize buffer's boundaries,
      // and the center will be kept at one. Like so:
      //             ________
      //    /\  >>  /        \
      //   /  \    /          \
      //  /    \  /            \
      //
      val len  = (synthesizeWinAmt * inputWinSize + 0.5).toInt
      val lenH = len >> 1
      val arr = GenWindow(size = len, shape = synthesizeWinType)
      // GenWindow.Rectangle.fill(arr, lenH, inputWinSize - len)
      ???
    }

    val fftSize   = winSize
    val winSizeH  = winSize >> 1
    val radiusI   = math.max(1, math.min(winSizeH - 1, (radius * winSizeH + 0.5).toInt))

    // println(s"fftSize = $fftSize; numFrames = $numFrames; stepSize = $stepSize; inputWinsize = $inputWinSize")

    // val inputPadLen     = inputWinSize - stepSize
    val inputPadLen     = inputWinSize/2
    val templatePadLen  = inputPadLen // templateWinSize - stepSize
    // we pad the input so the when we apply the window, we don't
    // lose the first frames
    val inputPad        = DC(0.0).take(inputPadLen   ) ++ input
    val templatePad     = DC(0.0).take(templatePadLen) ++ template

    val slideA    = Sliding(in = inputPad   , size = inputWinSize   , step = stepSize)
    val slideB    = Sliding(in = templatePad, size = templateWinSize, step = stepSize)
    val winA      = slideA * winAnaIn
    val winB      = slideB * winAnaTemp
    val winARes   = ResizeWindow(in = winA, size = inputWinSize   , start = 0, stop = fftSize - inputWinSize   )
    val winBRes   = ResizeWindow(in = winB, size = templateWinSize, start = 0, stop = fftSize - templateWinSize)

    // XXX TODO --- should use Real1FFT when the DC-packing is solved
    val fftGain   = fftSize/2 // XXX TODO -- why does this have an effect if we normalise in `elemNorm` anyway?
    val fftA0     = Real1FullFFT(in = winARes, size = fftSize)
    val fftA      = fftA0 * fftGain
    val fftB0     = Real1FullFFT(in = winBRes, size = fftSize)
    val fftB      = fftB0 * fftGain

    printLength(fftA, "fftA-len")
    printLength(fftB, "fftB-len")

    //    val TEST = fftA - fftB
    //    TEST.poll(1.0/16, "TEST")

    // cf. https://en.wikipedia.org/wiki/Phase_correlation
    val conjA     = fftA .complex.conj  // A is to be shift against B!
    val conv      = conjA.complex * fftB
    val convMagR  = conv .complex.mag.max(1.0e-06).reciprocal
    val convBuf   = BufferDisk(conv)    // XXX TODO -- measure max delay
    val elemNorm  = convBuf * RepeatWindow(convMagR)
    val iFFT0     = Real1FullIFFT(in = elemNorm, size = fftSize)
    val iFFT      = iFFT0 / fftGain

//    Plot1D(winARes.elastic(), fftSize, "in-a")
//    Plot1D(fftA.complex.mag.elastic(), fftSize/2, "fft-a")
//    Plot1D(winBRes.elastic(), fftSize, "in-b")
//    Plot1D(fftB.complex.mag.elastic()    , fftSize/2, "fft-b")
//    //    Plot1D(conjA   , fftSize * 2, "conjA")
//    Plot1D(conv.elastic(), fftSize, "mul"  )
//    //    Plot1D(elemNorm, fftSize * 2, "norm" )
//    Plot1D(iFFT.complex.mag.elastic(), fftSize/2 , "ifft")

    //    RunningMax(in = elemNorm).poll(1.0/fftSize, "MAX-BEFORE")
    //    RunningMax(in = iFFT    ).poll(1.0/fftSize, "MAX-AFTER ")

    val prod      = PeakCentroid1D(in = iFFT, size = fftSize, radius = radiusI)
    val shiftX    = prod.translate
    //    RepeatWindow(shiftX).poll(0.5, "shift-x")
    val amp       = (ampModulation: GE).linlin(0, 1, 1.0, prod.peak)
    val ampPad    = RepeatWindow(in = amp   , num = inputWinSize /* winSize */)
    val shiftXPad = RepeatWindow(in = shiftX, num = inputWinSize /* winSize */)

    // RepeatWindow(shiftX).poll(Impulse(0.5), label = "shift-x")
    // shiftXPad.poll(Impulse(1.0/inputWinSize), label = "shift-x-p")

    // ---- synthesis ----
    // make sure to insert a large enough buffer
    val slideABuf = BufferDisk(slideA)      // XXX TODO -- measure max delay
    val synth     = slideABuf * winSynth * ampPad
    val lap       = OffsetOverlapAdd(in = synth, size = inputWinSize /* winSize */, step = stepSize, offset = shiftXPad, minOffset = -winSizeH)
    val sig       = if (!keepFileLength) lap.drop(inputPadLen) else lap.drop(inputPadLen + winSizeH).take(numFrames)

    sig
  }

  import numbers.Implicits._

  object Gain {
    val immediate  = Gain( 0.0.dbamp, normalized = false)
    val normalized = Gain(-0.2.dbamp, normalized = true )
  }

  object OutputSpec {
    val aiffFloat = AudioFileSpec(AudioFileType.AIFF, SampleFormat.Float, 1, 44100.0)
    // numCh, sr not used
    val aiffInt   = AudioFileSpec(AudioFileType.AIFF, SampleFormat.Int24, 1, 44100.0)
  }

  case class Gain(value: Double, normalized: Boolean = false) {
    def isUnity: Boolean = !normalized && value == 1.0
  }

  def main(args: Array[String]): Unit = {
    run()
  }

  def run(): Unit = {
    val inDir   = userHome  /"Documents"/"projects"/"Imperfect"/"audio_work"/"hibernation"
    val inA     = inDir     /"esc_outside-L.aif"
    val inB     = inDir     /"esc_outside-R.aif"
    val outDir  = userHome  /"Music"/"work"
    val output  = outDir    /"_killme.aif"
    run(inA = inA, inB = inB, output = output)
  }

  def runOLD(): Unit = {
    val inputDir  = userHome / "Music" / "work"
    val inputs    = inputDir.children(f => f.name.startsWith("mentasm-") && f.ext == "aif")
    val outputDir = userHome / "Documents" / "projects" / "Unlike" / "audio_work"

    println(s"There are ${inputs.size} input files.")
    outputDir.mkdir()

    //    val Seq(inA, inB) = scala.util.Random.shuffle(inputs.combinations(2)).next()
    val inA = inputDir / "mentasm-b1269fa6.aif"
    val inB = inputs.find(_.name.contains("65929a65")).get
    //    val inB = inA
    //    val inB = inA.parent / s"${inA.base}Hlb-2.aif"
    val idA = inA.base.substring(8)
    val idB = inB.base.substring(8)
    val output = outputDir / s"mentasm-$idA-$idB.aif"
    run(inA = inA, inB = inB, output = output)
    // run(inB, inA)
  }

  def printLength(in: GE, label: String): Unit = {
    import graph._
    Length(in).poll(0, label)
  }

  def run(inA: File, inB: File, output: File): Unit = {
    val idA = inA.base.substring(8)
    val idB = inB.base.substring(8)
    if (output.exists()) {
      println(s"Output file '$output' already exists. Not overwriting.")
      return
    }

    println(s"Processing $idA - $idB...")

    val numFramesA = AudioFile.readSpec(inA).numFrames.toInt
    val numFramesB = AudioFile.readSpec(inB).numFrames.toInt
    import numbers.Implicits._
    val truncate  = false
    val fftSizeA  = if (truncate) (numFramesA + 1).nextPowerOfTwo / 2 else numFramesA.nextPowerOfTwo
    val fftSizeB  = if (truncate) (numFramesB + 1).nextPowerOfTwo / 2 else numFramesB.nextPowerOfTwo
    val fftSize   = math.max(fftSizeA, fftSizeB)

    val g = Graph {
      import graph._

      val fftA = mkFourierFwd(in = inA, size = fftSize /* A */, gain = Gain.normalized)
      val fftB = mkFourierFwd(in = inB, size = fftSize /* B */, gain = Gain.normalized)

      val fftAZ0  = UnzipWindow(fftA).elastic(1024) // treat Re and Im as two channels
      val fftBZ0  = UnzipWindow(fftB).elastic(1024) // treat Re and Im as two channels
      val fftAZ   = fftAZ0
      val fftBZ   = fftBZ0

      val numFrames = math.min(fftSizeA, fftSizeB)
      println(s"numFrames = $numFrames")
      assert(numFrames.isPowerOfTwo)

      val config = MorassConfig(input = fftAZ, template = fftBZ,
        synthesizeWinType = GenWindow.Rectangle,
        inputWinSize = 16384 /* 4096 */, templateWinSize = 16384 /* 32768 */, stepSize = 1024 /* 16 */, ampModulation = 0.0675 /* 1.0 */,
        synthesizeWinAmt = 1.0 /* XXX TODO: 0.0625 */,
        numFrames = numFrames)
      val morass0 = mkMorass(config)
      val morass  = morass0 // .take(fftSize << 1)
      val morassZ = ZipWindow(ChannelProxy(morass, 0).elastic(1024), ChannelProxy(morass, 1).elastic(1024))

      mkFourierInv(in = morassZ, size = numFrames, out = output, spec = OutputSpec.aiffInt, gain = Gain.normalized)
    }

    val config = stream.Control.Config()
    // config.blockSize = config.blockSize / 4 // * 2 // /4
    config.useAsync = false // for debugging
    val ctrl = stream.Control(config)

    // showStreamLog = true

    ctrl.run(g)

    Swing.onEDT {
      SimpleGUI(ctrl)
    }

    println("Running.")
  }
}