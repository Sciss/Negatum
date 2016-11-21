
// version: 3 (18-Sep-16)
// (1: GE).poll(0, "listen")

val indicesIn = "buses-in".ir
val numCh     = NumChannels(indicesIn)
val in        = PhysicalIn.ar(indices = indicesIn)
val fftSize   = 1024
val fftBuf    = LocalBuf(numFrames = fftSize, numChannels = numCh)
val fft       = FFT(buf = fftBuf, in = in, hop = 0.5, winType = 1)
val loudness  = Loudness(fft)
val thresh    = "thresh".ir(10)
val isLoud    = loudness > thresh

/*---

We want to capture the non-loud parts. We create
"islands" around chunks that transgress a loudness
threshold. To do that, we use Trig and delay the
input signal by half the trig duration.
The analysis signal itself already has a delay
of fftSize.

*/

val islandMargin = 1.0  // seconds
val islandIn  = Impulse.kr(0) + isLoud // make sure we mute at the beginning
val island    = Trig1.kr(in = islandIn, dur = islandMargin * 2)
val inDlyTime = fftSize * SampleDur.ir + islandMargin
val inDly     = DelayN.ar(in, maxDelayTime = inDlyTime, delayTime = inDlyTime)
val inHPF     = HPF.ar(inDly, 80)
val dur       = "rec-dur".ir // (8)
dur.poll(0, "listen - rec-dur")
val numFrames = dur * SampleRate.ir
val recBuf    = BufferOut(artifact = "file", action = "done", numFrames = numFrames, numChannels = numCh)
// BufDur.kr(recBuf).poll(0, "rec-dur")
val rec       = RecordBuf.ar(in = inHPF, buf = recBuf, run = island sig_== 0, loop = 0)
val full      = Done.kr(rec)
StopSelf(full)
