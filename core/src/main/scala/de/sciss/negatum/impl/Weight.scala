/*
 *  Weight.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum.impl

import de.sciss.dsp
import de.sciss.file.File
import de.sciss.negatum.impl.Util.{add, dct, energy, mul}
import de.sciss.synth.io.AudioFile

object Weight {
  def apply(f: File, numCoeff: Int = 24 /* 13 */): Option[Weight] = {
    val mCfgB = dsp.MFCC.Config()
    mCfgB.fftSize   = 1024
    mCfgB.minFreq   = 60
    mCfgB.maxFreq   = 16000
    mCfgB.sampleRate= 44100.0
    mCfgB.numCoeff  = numCoeff
    val mCfg        = mCfgB.build
    import mCfg.fftSize
    val mfcc        = dsp.MFCC(mCfg)
    val fftSizeH    = fftSize/2

    val af = AudioFile.openRead(f)
    try {
      val inBuf   = af.buffer(fftSize)
      val winBuf  = new Array[Float](fftSize)
      val win     = dsp.Window.Kaiser6.create(fftSize)
      var off     = 0
      val numFrames = af.numFrames
      var remain  = numFrames
      val mean    = new Array[Double](numCoeff)
      val enSize  = math.max(0, ((remain - fftSize) / fftSizeH).toInt + 2)
      val enBuf   = new Array[Double](enSize)
      var count   = 0
      while (remain > 0) {
        val chunk = math.min(remain, fftSize - off).toInt
        af.read(inBuf, off, chunk)
        val off1 = off + chunk
        System.arraycopy(inBuf(0), 0, winBuf, 0, off1)
        if (off1 < fftSize) java.util.Arrays.fill(winBuf, off1, fftSize, 0f)
        mul(winBuf, 0, win, 0, off1)
        // if (count < fftSize) {
          val e = energy(winBuf, 0, off1)
          enBuf(count) = e
          // println(s"---- ENERGY: $e")
        // }
        val coeff = mfcc.process(winBuf, 0, off1)
        add(mean, 0, coeff, 0, numCoeff)
        remain -= chunk
        System.arraycopy(inBuf(0), fftSizeH, inBuf(0), 0, fftSizeH) // overlap
        off = fftSizeH
        count += 1

//        proc.progress = (((numFrames - remain).toDouble / numFrames) + gIdx) / numGraphs
//        proc.checkAborted()
      }
      if (count > 0) mul(mean, 0, numCoeff, 1.0 / count)

      val temporal = dct(enBuf, off = 0, len = count, numCoeff = numCoeff)
      // println(s"temporal.sum = ${temporal.sum}")

      if (mean.exists(x => x.isNaN || x.isInfinity) || enBuf.exists(_ > 1.0)) {
        println("Dropping chromosome with NaN / exploding features!")
        None
      } else {
        if (temporal(1) > 100) {
          println(s"Temporal exploded !?")
        }

        val weight  = new Weight(spectral = mean, temporal = temporal)
        Some(weight)
      }
    } finally {
      af.cleanUp()
    }
  }
}
final class Weight(val spectral: Array[Double], val temporal: Array[Double]) {
  override def toString: String = spectral.map(d => f"$d%1.3f").mkString("[", ", ", "]")
}
