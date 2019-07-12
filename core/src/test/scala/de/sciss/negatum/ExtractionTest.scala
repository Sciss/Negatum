package de.sciss.negatum

import de.sciss.file._
import de.sciss.negatum.impl.Features

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object ExtractionTest {
  def main(args: Array[String]): Unit = {
    val fIn   = file("/data/projects/Almat/events/scaladays2019/materials/derrida1_1_216500.aif")
    val fOut  = file("/data/temp/derrida1_1_216500_featFSc.aif")
    val fut   = Features.runExtraction(fIn = fIn, fOut = fOut)
    Await.result(fut, Duration.Inf)
  }
}
