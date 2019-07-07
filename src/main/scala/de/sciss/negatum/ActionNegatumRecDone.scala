/*
 *  ActionNegatumRecDone.scala
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

package de.sciss.negatum

import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Folder
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.negatum.Composition.{logComp, logCompErr, mkDateString}
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.Action.Universe
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc._

import scala.util.{Failure, Success}

object ActionNegatumRecDone extends NamedAction("negatum-rec-done") {
  // step 1
  def begin[S <: SSys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
    val dsl = DSL[S]
    import Util._
    import DefaultRandom._
    import dsl._
    import universe._

    logComp("negatum-rec-done")
    val attr           = self.attr
    val Some(ens)      = attr.$[Ensemble]("context")
    val Some(fNegatum) = attr.$[Folder  ]("negatum-folder")

    ens.stop()

    val numIter = attr.$[IntObj]("iterations").map(_.value).getOrElse(10)

    val fLastOpt = fNegatum.lastOption
    val neg: Negatum[S] = fLastOpt.collect {
      case n: Negatum[S] if {
        val count = n.attrInt("count", 0)
        val res = count < Composition.MaxNegatum
        res
      } => n
    } .getOrElse {

      println("----1")
      val Some(artObj)  = attr.$[Artifact]("file")
      val art           = artObj.value
      println(s"----2 $art")
      val tempSpec      = AudioFile.readSpec(art)
      println(s"----3 $tempSpec")
      val tempCue       = AudioCue(art, tempSpec, offset = 0L, gain = 1.0)
      val tempCueObj    = AudioCue.Obj.newConst[S](tempCue)

      val _neg          = Negatum(tempCueObj)
      _neg.name = s"negatum-${mkDateString()}"
      println(s"----4 ${_neg.name}")

      _neg.adjustInt   (Negatum.attrBreedElitism   , rrand(0, 3))
      _neg.adjustInt   (Negatum.attrBreedGolem     , rrand(15, 25))
      _neg.adjustInt   (Negatum.attrBreedMaxMut    , rrand(4 , 6))
      _neg.adjustDouble(Negatum.attrBreedProbMut   , rrand(0.73, 0.76))

      _neg.adjustDouble(Negatum.attrEvalMaxBoost   , rrand(11.0, 13.0))
      _neg.adjustDouble(Negatum.attrEvalTimeWeight , rrand(0.3, 0.5))

      _neg.adjustInt   (Negatum.attrGenMinVertices , rrand(8, 12))
      _neg.adjustInt   (Negatum.attrGenMaxVertices , rrand(80, 100))
      _neg.adjustInt   (Negatum.attrGenPopulation  , 1000)
      _neg.adjustDouble(Negatum.attrGenProbConst   , rrand(0.4, 0.5))

      println(s"----5")
      fNegatum.addLast(_neg)
      println(s"----6")
      _neg
    }

    self.attr.put("negatum", neg)

    val negCfg = Negatum.attrToConfig(neg)

    logComp("Starting Negatum rendering...")
    implicit val u: Universe[S] = universe
    val renderNeg = neg.run(negCfg, iter = numIter)
    val selfH = tx.newHandle(self)
    var lastProg = 0
    renderNeg.reactNow { implicit tx => {
      case Rendering.Progress(amt) =>
        val amtI = (amt * 100 + 0.5).toInt
        val amtM = amtI - (amtI % 10)
        if (amtM != lastProg) {
          logComp(s"Negatum progress: $amtM %")
          lastProg = amtM
        }
      case Rendering.Completed(Success(_)) =>
        negatumDone(selfH)

      case Rendering.Completed(Failure(ex)) =>
        logCompErr("!! Negatum failed:")
        ex.printStackTrace()
    }}
  }

  // step 2
  def negatumDone[S <: SSys[S]](selfH: stm.Source[S#Tx, Action[S]])
                               (implicit tx: S#Tx, universe: Universe[S]): Unit = {
    val self = selfH()
    val attr = self.attr
    logComp("Negatum done.")

    val Some(neg) = attr.$[Negatum] ("negatum")
    val Some(svm) = attr.$[SVMModel]("svm")

    logComp("Starting SVM selection...")
    import universe.cursor
    val renderSVM = svm.predict(neg)
    var lastProg = 0
    renderSVM.reactNow { implicit tx => {
      case Rendering.Progress(amt) =>
        val amtI = (amt * 100 + 0.5).toInt
        val amtM = amtI - (amtI % 10)
        if (amtM != lastProg) {
          logComp(s"SVM progress: $amtM %")
          lastProg = amtM
        }
      case Rendering.Completed(scala.util.Success(n)) =>
        svmDone(selfH, svmNum = n)

      case de.sciss.negatum.Rendering.Completed(scala.util.Failure(ex)) =>
        logCompErr("!! SVM failed:")
        ex.printStackTrace()
    }}
  }

  // step 3
  def svmDone[S <: SSys[S]](selfH: stm.Source[S#Tx, Action[S]], svmNum: Int)
                           (implicit tx: S#Tx, universe: Universe[S]): Unit = {
    val dsl = DSL[S]
    import dsl._

    val self = selfH()
    val attr = self.attr
    logComp(s"SVM done ($svmNum).")

    val Some(neg)  = attr.$[Negatum]("negatum")
    val Some(fSOM) = attr.$[Folder ]("som-folder")

    val som: SOM[S] = fSOM.lastOption.collect {
      case _som: SOM[S] if _som.attrInt("count", 0) < Composition.MaxSOM => _som
    } .getOrElse {
      val cfg = SOM.Config()
      cfg.numIterations = Composition.MaxSOM
      val _som = SOM(cfg)
      _som.name = s"som-${mkDateString()}"
      fSOM.addLast(_som)
      _som
    }
    val somH = tx.newHandle(som)

    val negCount = neg.attrInt("count", 0) + svmNum
    neg.adjustInt("count", negCount)
//    val somCount = som.attrInt("count", 0) + svmNum
//    som.adjustInt("count", somCount)

    println("Starting SOM addition...")
    import universe.cursor
    val renderSOM = som.addAll(neg.population, selected = true)
    renderSOM.reactNow { implicit tx => {
      case Rendering.Progress(_) =>
      case Rendering.Completed(scala.util.Success(n)) =>
        somDone(selfH, som = somH(), somNum = n)

      case Rendering.Completed(scala.util.Failure(ex)) =>
        logCompErr("!! SOM failed:")
        ex.printStackTrace()
    }}
  }

  // step 4
  def somDone[S <: SSys[S]](selfH: stm.Source[S#Tx, Action[S]], som: SOM[S], somNum: Int)
                          (implicit tx: S#Tx, universe: Universe[S]): Unit = {
    val dsl = DSL[S]
    import dsl._
    val self = selfH()
    val attr = self.attr

    val somCount = som.attrInt("count", 0) + somNum
    logComp(s"SOM addition done (+$somNum = $somCount).")

    som.adjustInt("count", somCount)

    val Some(ensSOMPlay) = attr.$[Ensemble]("som-play")
    if (ensSOMPlay.folder.isEmpty && somCount >= 100) {
      logComp(s"SOM addition sufficient to start timeline creation")
      val Some(aSOMTimeline) = attr.$[Action]("som-timeline")
      val univ1 = Action.Universe(aSOMTimeline)
      aSOMTimeline.execute(univ1)
    }

    logComp("Restarting negatum process")
    val Some(aNegStart) = attr.$[Action]("restart")
    val univ1 = Action.Universe(aNegStart)
    aNegStart.execute(univ1)
  }
}