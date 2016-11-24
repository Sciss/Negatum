play {
  val indicesIn = "in".kr(2)
  val numCh     = 1
  val in        = PhysicalIn.ar(indices = indicesIn)
  val fftSize   = 1024
  val fftBuf    = LocalBuf(numFrames = fftSize, numChannels = numCh)
  val fft       = FFT(buf = fftBuf, in = in, hop = 0.5, winType = 1)
  val tr   = Impulse.kr(0.5)
  val tr0  = tr - Impulse.kr(0)
  // sc bug -- loudness starts with crazy high value ?
  val loudness  = Loudness(fft) * SetResetFF.kr(trig = tr0, reset = 0)
  val trD = TDelay.kr(tr, 0.05)
  val max = RunningMax.kr(loudness, trD)
  val min = RunningMin.kr(loudness, trD)
  min.poll(tr, "loud-min")
  max.poll(tr, "loud-max")
  ()
}

// 4 is moderate sounds
// 10 is already quite loud in the room
