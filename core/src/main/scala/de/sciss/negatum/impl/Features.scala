/*
 *  Features.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum
package impl

import de.sciss.file._
import de.sciss.filecache
import de.sciss.filecache.{TxnConsumer, TxnProducer}
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{GE, Graph}
import de.sciss.serial.{DataInput, DataOutput, ConstFormat}
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.io.{AudioFile, AudioFileSpec, AudioFileType, SampleFormat}

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.stm.TxnExecutor
import scala.concurrent.{Future, Promise, blocking}

object Features {
  final val norms = Array[Array[Float]](
    Array(0.006015186f,1.4569731f),
    Array(-1.4816481f,3.093808f),
    Array(-1.4089416f,1.267046f),
    Array(-0.860692f,1.4034394f),
    Array(-0.65952975f,1.431201f),
    Array(-0.66072506f,0.8506244f),
    Array(-0.2808966f,0.90672106f),
    Array(-0.29912513f,0.705802f),
    Array(-0.22443223f,0.67802113f),
    Array(-0.1471797f,0.68207365f),
    Array(-0.104354106f,0.6723507f),
    Array(-0.2412649f,0.70821077f),
    Array(-0.16983563f,0.6771785f),
    Array(-0.10048226f,0.64655834f)
  )

  final case class ExtractionFailed(cause: Throwable) extends Exception(cause)

  def extract(input: File, config: Config): Future[(File, AudioFileSpec)] = {
    val key       = (input, config)
    val futMeta   = TxnExecutor.defaultAtomic { implicit tx =>
      cache.acquire(key)
    }
    val res       = futMeta.map { v =>
      val featureF  = v.feature
      val inputSpec = blocking(AudioFile.readSpec(input))
      TxnExecutor.defaultAtomic { implicit tx =>
        cache.release(key)
      }
      (featureF, inputSpec)
    }
    // res.onComplete(_ => TxnExecutor.defaultAtomic { implicit tx => cache.release(key) })
    res
  }

  private[this] final val DEBUG = false

  def correlate(bounceF: File, inputSpec: AudioFileSpec, inputFeatureF: File,
                config: Config, maxBoost: Double, temporalWeight: Double): Future[Double] = {

  // XXX TODO -- would be faster if we could use a Poll during
  // the bounce and instruct the bounce proc to immediately terminate
  // when seeing a particular message in the console?
    val af = AudioFile.openRead(bounceF)
    try {
      val bufSize = 512
      val b       = af.buffer(bufSize)
      var i       = 0L
      while (i < af.numFrames) {
        val len = math.min(bufSize, af.numFrames - i).toInt
        af.read(b, 0, len)
        var ch = 0
        while (ch < af.numChannels) {
          val bc = b(ch)
          var j = 0
          while (j < len) {
            if (bc(j).isNaN || bc(j).isInfinite) {
              if (DEBUG) println("Detected NaNs")
              throw Features.ExtractionFailed(null)
            }
            j += 1
          }
          ch += 1
        }
        i += len
      }
    } finally {
      af.cleanUp()
    }

    val pRes = Promise[Vec[Double]]()

    val g = Graph {
      val specFeat = AudioFile.readSpec(inputFeatureF)
      require (specFeat.numChannels == 1)
      import de.sciss.fscape.graph._
      val (featSize, _ /*sampleRate*/, loudA0, mfccA0) = mkExtraction(bounceF, config)
      val sigB: GE = AudioFileIn(inputFeatureF, numChannels = 1)
//      sigA.poll(sigA.isNaN, "sigA-NaN")
//      sigB.poll(sigB.isNaN, "sigB-NaN")

      val loudA = loudA0 // .elastic(featSize)
      val mfccA = mfccA0.elastic(featSize)  // XXX TODO WTF

      def feat(in: GE): (GE, GE) = {
        val loud = ResizeWindow(in, featSize, stop  = -featSize + 1) // .elastic(featSize)
        val mfcc = ResizeWindow(in, featSize, start = 1).elastic(featSize)  // XXX TODO WTF
        (loud, mfcc)
      }

      val (loudB, mfccB) = feat(sigB)

      // val maxBoostDb = maxBoost.ampDb

      def mkCorr(a: GE, b: GE, isLoud: Boolean): GE = {

//        val label = if (isLoud) "loud" else "mfcc"
//
//        aMean.poll(0, s"aMean-$label")
//        bMean.poll(0, s"bMean-$label")

        // mfcc are more or less DC-less, so we don't calculate the mean
        val aDif  = if (!isLoud) a else {
          val aBuf  = BufferMemory(a, specFeat.numFrames)
          val aMean = RunningSum(a).last / Length(a)
          aBuf - aMean
        }

        val bDif  = if (!isLoud) b else {
          val bBuf  = BufferMemory(b, specFeat.numFrames)
          val bMean = RunningSum(b).last / Length(b)
          bBuf - bMean
        }

        val num   = RunningSum(aDif * bDif).last
        val rmsA  = RunningSum(aDif.squared).last.sqrt
        val rmsB  = RunningSum(bDif.squared).last.sqrt
        val den   = rmsA * rmsB
        val v     = num / den

//        v.poll(0, s"corr-$label")

        if (!isLoud) v else {
          val boost = rmsB / rmsA
//          v.poll(0, "boost")
          v * (boost < maxBoost)
        }
      }

      val corrLoud = mkCorr(loudA, loudB, isLoud = true )
      val mfccLoud = mkCorr(mfccA, mfccB, isLoud = false)

      val res = corrLoud * temporalWeight + mfccLoud * (1.0 - temporalWeight)
      DebugDoublePromise(res, pRes)
    }

    val cfg = Control.Config()
    val ctl = Control(cfg)
    ctl.run(g)
    val res = pRes.future.map { vec =>
      val corr = vec.head // pRes.future.value.get.get.head
//      println(s"corr $corr")
      if (corr.isNaN) 0.0 else corr
    }
    res
  }

  protected def any2stringadd: Any = ()

  object Config {
    implicit object serializer extends ConstFormat[Config] {
      def write(v: Config, out: DataOutput): Unit = {
        import v._
        out.writeInt(stepSize )
        out.writeInt(fftSize  )
        out.writeInt(numMFCC  )
        out.writeInt(numMel   )
        out.writeInt(minFreq  )
        out.writeInt(maxFreq  )
      }

      def read(in: DataInput): Config = Config(
        stepSize = in.readInt(),
        fftSize  = in.readInt(),
        numMFCC  = in.readInt(),
        numMel   = in.readInt(),
        minFreq  = in.readInt(),
        maxFreq  = in.readInt()
      )
    }
  }
  final case class Config(stepSize: Int =   256,
                          fftSize : Int =  2048,
                          numMFCC : Int =    31,
                          numMel  : Int =    62,
                          minFreq : Int =    32,
                          maxFreq : Int = 16000
                         ) {

    require ((fftSize >= stepSize) && (fftSize % stepSize) == 0, s"stepSize $stepSize, fftSize $fftSize")
    require (numMel >= numMFCC && numMel >= 2, s"numMFCC $numMFCC, numMel $numMel")
    require (minFreq >= 8 && maxFreq > minFreq, s"minFreq $minFreq, maxFreq $maxFreq")
  }

  private def mkExtraction(fIn: File, config: Config): (Int, Double, GE, GE) = {
    import config._
    import de.sciss.fscape.graph._

    val specIn      = AudioFile.readSpec(fIn)
    import specIn.{numChannels, numFrames, sampleRate}
    def mkIn()      = AudioFileIn(fIn, numChannels = numChannels)
    val in          = mkIn()
    val numSteps    = (numFrames + stepSize - 1) / stepSize
    //      val numMFCCOut  = numSteps * numMFCC
    val featSize    = 1 + numMFCC // per frame: loudness and MFCC
    val slidLen     = fftSize * numSteps

    val inMono      = Mix.MonoEqP(in)

//    inMono.poll(inMono.isNaN, "inMono-NaN")

    val fftSlid     = Sliding(inMono, fftSize, stepSize) * GenWindow(fftSize, GenWindow.Hann).take(slidLen)
    val fft         = Real1FFT(fftSlid, fftSize, mode = 2)
    val fftMag      = fft.complex.mag
    val mel         = MelFilter(fftMag, fftSize/2, bands = numMel,
      minFreq = 36, maxFreq = 18000, sampleRate = sampleRate)
    val mfcc        = DCT_II(mel.log.max(-320.0), numMel, numMFCC, zero = 0) / numMel // .take(numMFCCOut)
    //      (numSteps: GE).poll(0,"numSteps")
    //      Length(mfcc).poll(0, "mfcc-length")

//    mfcc.poll(mfcc.isNaN, "mfcc-NaN")

    val loud        = Loudness(in = fftSlid, sampleRate = sampleRate, size = fftSize, spl = 90) / 90
    //      Length(loud).poll(0, "loud-length")

//    loud.poll(loud.isNaN, "loud-NaN")


//    loud.poll(sig.isNaN, "sig-NaN")

    (featSize, sampleRate, loud, mfcc)
  }

  def runExtraction(fIn: File, fOut: File, config: Config = Config()): Future[Unit] = {
    import config._
    val g = Graph {
      import de.sciss.fscape.graph._
      val (featSize, sampleRate, loud, mfcc) = mkExtraction(fIn, config)
      // XXX TODO why do we need this amount of buffers? seems a short coming of ResizeWindow
      val sig = ResizeWindow(mfcc.elastic(numMFCC), numMFCC, start = -1) + ResizeWindow(loud, 1, stop = +numMFCC)
      //      val sig: GE = loud +: Vector.tabulate(numMFCC)(ch => WindowApply(mfcc, numMFCC, index = ch))

      val fOutExt = fOut.extL
      val tpeOut  = AudioFileType.writable.collectFirst {
        case tpe if tpe.extensions.contains(fOutExt) => tpe
      } .getOrElse(AudioFileType.AIFF)

      val specOut = AudioFileSpec(
        fileType      = tpeOut,
        sampleFormat  = SampleFormat.Float,
        numChannels   = 1, // featSize,
        sampleRate    = sampleRate / stepSize * featSize
      )
      AudioFileOut(sig, fOut, specOut)
    }

    val cfg = Control.Config()
    val ctl = Control(cfg)
    ctl.run(g)
    ctl.status
  }

  // ---- private ----

  private object CacheValue {
    implicit object serializer extends ConstFormat[CacheValue] {
      def write(v: CacheValue, out: DataOutput): Unit = {
        out.writeLong(v.lastModified)
        out.writeUTF (v.feature.getCanonicalPath)
      }

      def read(in: DataInput): CacheValue = {
        val mod     = in.readLong()
        val feature = file(in.readUTF())
        CacheValue(lastModified = mod, feature = feature)
      }
    }
  }

  private type CacheKey = (File, Config)
  private case class CacheValue(lastModified: Long, feature: File)

  private val cCfg  = {
    val c = filecache.Config[CacheKey, CacheValue]()
    // c.executionContext = SoundProcesses.executionContext
    c.capacity  = filecache.Limit(count = 10)
    c.accept    = { (key        , value) => key._1.lastModified() == value.lastModified }
    c.space     = { (_ /* key */, value) => value.feature.length() }
    c.evict     = { (_ /* key */, value) => value.feature.delete() }
    c.build
  }
  private val cacheP = TxnExecutor.defaultAtomic { implicit tx => TxnProducer(cCfg) }
  private val cache  = TxnConsumer(cacheP)(mkCacheValue)

  private def mkCacheValue(key: (File, Config)): Future[CacheValue] = {
    val (fIn, featCfg) = key
//    val inputSpec         = AudioFile.readSpec(fIn)
    val inputMod          = fIn.lastModified()
//    require(inputSpec.numChannels == 1, s"Input file '${f.name}' must be mono but has ${inputSpec.numChannels} channels")
    val fOut              = File.createTemp(suffix = ".aif")
    val fut               = runExtraction(fIn = fIn, fOut = fOut, config = featCfg)
    fut.map { _ =>
      CacheValue(lastModified = inputMod, feature = fOut)
    }
  }
}