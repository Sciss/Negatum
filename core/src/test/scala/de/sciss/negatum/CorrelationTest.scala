package de.sciss.negatum

import de.sciss.file._
import de.sciss.negatum.impl.Features
import de.sciss.audiofile.AudioFile

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object CorrelationTest {
  def main(args: Array[String]): Unit = {
    val fIn1  = file("/data/projects/Almat/events/scaladays2019/materials/derrida1_1_216500.aif")
    val fOut1 = File.createTemp()
    val fIn2  = fIn1
    val fut1  = Features.runExtraction(fIn = fIn1, fOut = fOut1)
    Await.result(fut1, Duration.Inf)
    val fInSpec1 = AudioFile.readSpec(fOut1)
    val fut2 = Features.correlate(fIn2, fInSpec1, fOut1, Features.Config(), maxBoost = 10.0, temporalWeight = 0.5)
    val corr = Await.result(fut2, Duration.Inf)
    println(s"corr = $corr")
  }
}
