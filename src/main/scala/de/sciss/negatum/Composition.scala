/*
 *  Composition.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.file._
import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{Copy, Sys, Txn, TxnLike}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.Mellite
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.Action.Universe
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc._
import de.sciss.synth.ugen

import scala.concurrent.stm.TxnExecutor
import scala.util.{Failure, Success}

object Composition {
  private[this] val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'proc' - ", Locale.US)
  var showCompLog = true

  def logComp(what: => String): Unit =
    if (showCompLog) Console.out.println(s"${logHeader.format(new Date())}$what")

  def logCompErr(what: => String): Unit =
    Console.err.println(s"${logHeader.format(new Date())} ERROR - $what")

  private val fileDateFmt = new SimpleDateFormat("yyMMdd_HHmmss", Locale.US)

  def mkDateString(): String = fileDateFmt.format(new Date)

  val baseDir         = userHome / "Documents" / "projects" / "Imperfect"
  val mainSession     = baseDir / "anemone" / "main.mllt"
  val svmModelSession = baseDir / "anemone" / "svm-model.mllt"

  def readWorkspace(dir: File): Workspace.Durable = {
    val wsl = Workspace.read(dir, BerkeleyDB.factory(dir))
    wsl match {
      case d: Workspace.Durable => d
      case _ => sys.error(s"Expected durable workspace for '$dir', but got $wsl")
    }
  }

  def mkSVM[S <: Sys[S]](wsOut: Workspace[S]): Unit = {
    val wsIn  = readWorkspace(svmModelSession)
    type In   = Durable

    Txn.copy[In, S, Unit] { (_txIn: In#Tx, _txOut: S#Tx) => {
      implicit val txIn  = _txIn
      implicit val txOut = _txOut
      val context = Copy[In, S]
      val svmIn = wsIn.root.iterator.collectFirst {
        case _svm: SVMModel[In] => _svm
      }   .getOrElse(sys.error(s"No SVM model found in '$svmModelSession'"))
      val svmOut = context(svmIn)
      svmOut.name = "svm-model"
      wsOut.root.addLast(svmOut)
    }} (wsIn.cursor, wsOut.cursor)
  }

  def main(args: Array[String]): Unit = {
    Mellite.initTypes()
    Negatum.init()
    registerActions()
    run()
  }

  def registerActions(): Unit = TxnExecutor.defaultAtomic { implicit itx =>
    implicit val tx = TxnLike.wrap(itx)
    actions.foreach { a =>
      Action.registerPredef(a.name, a)
    }
  }

  def run(): Unit = {
    type S = Durable
    val ws: Workspace[S] = if (mainSession.exists()) {
      val _ws = readWorkspace(mainSession)
      val hasSVM = _ws.cursor.step { implicit tx =>
        _ws.root.iterator.collectFirst {
          case m: SVMModel[S] => true
        } .getOrElse(false)
      }
      if (!hasSVM) mkSVM(_ws)
      _ws

    } else {
      val _ws = Workspace.Durable.empty(mainSession, BerkeleyDB.factory(mainSession))
      mkSVM(_ws)
      _ws
    }

    ws.cursor.step { implicit tx =>
      build(ws)
    }

    println("Yo chuck.")
    ws.close()
    sys.exit()
  }

  val actions: Seq[NamedAction] = Seq(
    ActionNegatumStart, ActionNegatumRec, ActionNegatumRecDone
  )

  def build[S <: SSys[S]](workspace: Workspace[S])(implicit tx: S#Tx): Unit = {
    val dsl = DSL[S]
    import dsl._

    val f = workspace.root

    val locImperfect = artifactLoc(baseDir).in(f)

    val svm = f.iterator.collectFirst {
      case m: SVMModel[S] => m
    } .getOrElse (sys.error(s"SVM model not found in workspace"))

    val ensMain = ensemble("main").in(f)(initPlay = true)

    val ensNegListen = ensemble("ens-negatum-listen").in(f)(initPlay = false)
    ensNegListen.addTo(ensMain)

    val pNegListen = proc("negatum-listen").in(ensNegListen) {
      import graph._
      import Ops._
      import ugen._

      val indexIn     = "bus-in".ir
      val numCh       = 1 // NumChannels(indicesIn)
      val in          = PhysicalIn.ar(indices = indexIn) \ 0
      val fftSize     = 1024
      val fftBuf      = LocalBuf(numFrames = fftSize, numChannels = numCh)
      val fft         = FFT(buf = fftBuf, in = in, hop = 0.5, winType = 1)
      val loudness    = Loudness(fft)
      val threshStart = "thresh-start".ir( 10f)
      val threshEnd   = "thresh-end"  .ir(  3f)
      val threshDur   = "thresh-dur"  .ir(120f)
      val thresh      = Line.kr(threshStart, threshEnd, threshDur)
      val isLoud      = loudness > thresh
      val run         = SetResetFF.kr(trig = isLoud, reset = 0)

      val inDly     = DelayN.ar(in, maxDelayTime = 0.5, delayTime = 0.5)
      val inHPF     = HPF.ar(inDly, 80)
      val dur       = "rec-dur".ir // (8)
      DC.kr(dur).poll(run, "listen-negatum run for")
      val numFrames = dur * SampleRate.ir
      val recBuf    = BufferOut(artifact = "file", action = "done", numFrames = numFrames, numChannels = numCh)
      val rec       = RecordBuf.ar(in = inHPF, buf = recBuf, run = run, loop = 0)
      val full      = Done.kr(rec)
      StopSelf(full)
    }

    val fNegatum = folder("negatum").in(f)
    val fSOM     = folder("som"    ).in(f)

    val aNegStart = action("negatum-start").in(f)(ActionNegatumStart)
    aNegStart("negatum-folder") = fNegatum
    aNegStart.addTo(ensMain)

    val aNegRec = action("negatum-rec").in(f)(ActionNegatumRec)
    aNegRec("context")  = ensNegListen
    aNegRec("dir")      = locImperfect
    aNegRec("proc")     = pNegListen
    aNegStart("rec")    = aNegRec

    val aNegRecDone = action("negatum-rec-done").in(f)(ActionNegatumRecDone)
    aNegRecDone("context")        = ensNegListen
    aNegRecDone("negatum-folder") = fNegatum
    aNegRecDone("som-folder")     = fSOM
    aNegRecDone("iterations")     = int(2)  // XXX TODO
    aNegRecDone("svm")            = svm

    aNegStart ("done")            = aNegRecDone
    aNegRec   ("done")            = aNegRecDone
    pNegListen("done")            = aNegRecDone
  }

  trait NoSys extends SSys[NoSys]

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
      val Some(artObj)   = attr.$[Artifact]("file")

      ens.stop()

      val numIter = attr.$[IntObj]("iterations").map(_.value).getOrElse(10)

      val neg: Negatum[S] = fNegatum.lastOption.collect {
        case n: Negatum[S] if n.attrInt("count", 0) < 1000 => n
      } .getOrElse {

        val art        = artObj.value
        val tempSpec   = AudioFile.readSpec(art)
        val tempCue    = AudioCue(art, tempSpec, offset = 0L, gain = 1.0)
        val tempCueObj = AudioCue.Obj.newConst[S](tempCue)

        val _neg        = Negatum(tempCueObj)
        _neg.name = s"negatum-${mkDateString()}"

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

        fNegatum.addLast(_neg)
        _neg
      }

      self.attr.put("negatum", neg)

      val negCfg = Negatum.attrToConfig(neg)

      logComp("Starting Negatum rendering...")
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
                                 (implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
      val self = selfH()
      val attr = self.attr
      logComp("Negatum done.")

      val Some(neg) = attr.$[Negatum] ("negatum")
      val Some(svm) = attr.$[SVMModel]("svm")

      logComp("Starting SVM selection...")
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
                             (implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
      val dsl = DSL[S]
      import dsl._

      val self = selfH()
      val attr = self.attr
      logComp(s"SVM done ($svmNum).")

      val Some(neg)  = attr.$[Negatum]("negatum")
      val Some(fSOM) = attr.$[Folder ]("som-folder")

      val som: SOM[S] = fSOM.lastOption.collect {
        case _som: SOM[S] if _som.attrInt("count", 0) < 10000 => _som
      } .getOrElse {
        val cfg = SOM.Config()
        cfg.numIterations = 10000
        val _som = SOM(cfg)
        _som.name = s"som-${mkDateString()}"
        fSOM.addLast(_som)
        _som
      }

      val negCount = neg.attrInt("count", 0) + svmNum
      neg.adjustInt("count", negCount)
      val somCount = som.attrInt("count", 0) + svmNum
      som.adjustInt("count", somCount)

      println("Starting SOM addition...")
      val renderSOM = som.addAll(neg.population, selected = true)
      renderSOM.reactNow { implicit tx => {
        case Rendering.Progress(amt) =>
        case Rendering.Completed(scala.util.Success(n)) =>
          somDone(selfH, somNum = n)

        case Rendering.Completed(scala.util.Failure(ex)) =>
          logCompErr("!! SOM failed:")
          ex.printStackTrace()
      }}
    }

    // step 4
    def somDone[S <: Sys[S]](selfH: stm.Source[S#Tx, Action[S]], somNum: Int)
                            (implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
//      val self = selfH()
//      val attr = self.attr
      logComp(s"SOM addition done ($somNum).")
      println("CONTINUE HERE")
    }
  }
}