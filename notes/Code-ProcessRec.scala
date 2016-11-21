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

val Two = CepCoef(
  crr = +1, cri = +1,
  clr =  0, cli =  0,
  ccr = +1, cci = -1,
  car = +1, cai = -1,
  gain = 1.0/32         // XXX TODO --- what's this factor?
)

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

def any2stringadd(s: String): Unit = () // bloody scala.Predef

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
  val max       = run.last
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
