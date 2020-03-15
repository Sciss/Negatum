/*
 *  Composition.scala
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

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.file._
import de.sciss.lucre.stm.{Copy, Sys, Txn, TxnLike}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.Mellite
import de.sciss.synth.proc.{Action, ActionRaw, Durable, Workspace}
import de.sciss.synth.ugen

import scala.concurrent.stm.TxnExecutor
import scala.language.existentials
import scala.util.control.NonFatal

object Composition {
  final val MaxNegatum =  500
  final val MaxSOM     = 5000

  private[this] val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'proc' - ", Locale.US)
  var showCompLog = true

  def logComp(what: => String): Unit =
    if (showCompLog) Console.out.println(s"${logHeader.format(new Date())}$what")

  def logCompErr(what: => String): Unit =
    Console.err.println(s"${logHeader.format(new Date())} ERROR - $what")

  private val fileDateFmt = new SimpleDateFormat("yyMMdd_HHmmss", Locale.US)

  def mkDateString(): String = fileDateFmt.format(new Date)

  lazy val baseDir        : File = userHome / "Documents" / "projects" / "Imperfect"
  lazy val sessionsDir    : File = baseDir / "anemone"
  lazy val mainSession    : File = sessionsDir / "main.mllt"
  lazy val svmModelSession: File = sessionsDir / "svm-model.mllt"

  def createFreshWorkspace(): Workspace.Durable = {
    val sessions = sessionsDir.children { f =>
      val n = f.name
      n.startsWith("main_") && n.endsWith(".mllt")
    }
    val thisSessionF = sessionsDir / s"main_${mkDateString()}.mllt"

    val sessionsFound  = sessions.sortBy(_.name)
    val sessionLastOpt = sessionsFound.lastOption
    sessionLastOpt.fold[Workspace.Durable] {
      run(thisSessionF)
      readWorkspace(thisSessionF)
    } { lastSessionF =>
      print(s" continuing from ${lastSessionF.name}... ")
      try {
        val res  = emptyWorkspace(thisSessionF)
        val prev = readWorkspace(lastSessionF)
        try {
          copyWorkspace(wsIn = prev, wsOut = res)
        } finally {
          try {
            prev.cursor.step { implicit tx =>
              prev.dispose()
            }
          } catch {
            case NonFatal(ex) =>
              Console.err.println("While closing previous session:")
              ex.printStackTrace()
          }
        }
        res
      } catch {
        case ex: Exception =>
          Console.err.println(s"ERROR: Cannot copy session $lastSessionF:")
          ex.printStackTrace()
          Console.err.println("Using fallback session!")
          thisSessionF.renameTo(thisSessionF.replaceExt("BROKEN"))
          val fallbackF = sessionsDir / "main_161209_233713.mllt" // "main_161129_121420.mllt"
          val res       = emptyWorkspace(thisSessionF)
          val prev      = readWorkspace(fallbackF)
          copyWorkspace(wsIn = prev, wsOut = res)
          prev.cursor.step { implicit tx =>
            prev.dispose()
          }
          res
      }
    }
  }

  def readWorkspace(dir: File): Workspace.Durable = {
    val wsl = Workspace.read(dir, BerkeleyDB.factory(dir))
    wsl match {
      case d: Workspace.Durable => d
      case _ => sys.error(s"Expected durable workspace for '$dir', but got $wsl")
    }
  }

  def emptyWorkspace(dir: File): Workspace.Durable =
    Workspace.Durable.empty(dir, BerkeleyDB.factory(dir))

  def copyWorkspace[In <: Sys[In], Out <: Sys[Out]](wsIn: Workspace[In], wsOut: Workspace[Out]): Unit = {
    Txn.copy[In, Out, Unit] { (_txIn: In#Tx, _txOut: Out#Tx) => {
      implicit val txIn : In#Tx   = _txIn
      implicit val txOut: Out#Tx  = _txOut
      val context = Copy[In, Out]
      val rootIn  = wsIn .root
      val rootOut = wsOut.root
      rootIn.iterator.foreach { objIn =>
        val objOut = context(objIn)
        rootOut.addLast(objOut)
      }
      context.finish()
    }} (wsIn.cursor, wsOut.cursor)
  }

  def mkSVM[S <: Sys[S]](wsOut: Workspace[S]): Unit = {
    val wsIn  = readWorkspace(svmModelSession)
    type In   = Durable

    Txn.copy[In, S, Unit] { (_txIn: In#Tx, _txOut: S#Tx) => {
      implicit val txIn : In#Tx = _txIn
      implicit val txOut: S#Tx  = _txOut
      val context = Copy[In, S]
      val rootIn  = wsIn .root
      val rootOut = wsOut.root
      val svmIn = rootIn.iterator.collectFirst {
        case _svm: SVMModel[In] => _svm
      }   .getOrElse(sys.error(s"No SVM model found in '$svmModelSession'"))
      val svmOut = context(svmIn)
      // svmOut.name = "svm-model"
      context.finish()
      rootOut.addLast(svmOut)
    }} (wsIn.cursor, wsOut.cursor)
  }

  def main(args: Array[String]): Unit = {
    Mellite.initTypes()
    Negatum.init()
    registerActions()
    run(mainSession)
    sys.exit()
  }

  def registerActions(): Unit = TxnExecutor.defaultAtomic { implicit itx =>
    implicit val tx: TxnLike = TxnLike.wrap(itx)
    actions.foreach { a =>
      ActionRaw.registerPredef(a.name, a)
    }
  }

  def run(dir: File): Unit = {
    type S = Durable
    val ws: Workspace[S] = if (dir.exists()) {
      val _ws = readWorkspace(dir)
      val hasSVM = _ws.cursor.step { implicit tx =>
        _ws.root.iterator.collectFirst {
          case _: SVMModel[S] => true
        } .getOrElse(false)
      }
      if (!hasSVM) mkSVM(_ws)
      _ws

    } else {
      val _ws = emptyWorkspace(dir)
      mkSVM(_ws)
      _ws
    }

    ws.cursor.step { implicit tx =>
      build(ws)
    }

    println("Created/updated workspace.")
    ws.cursor.step { implicit tx =>
      ws.dispose()
    }
  }

  val actions: Seq[NamedAction] = Seq(
    ActionNegatumStart, ActionNegatumRec, ActionNegatumRecDone, ActionSOMTimeline
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

//    val tlSOM = timeline("tl-som").in(ensMain)

//    val fSOMPlay = folder("som-play").in(ensMain)
    val ensSOMPlay = ensemble("som-play").in(ensMain)(initPlay = false)

    val pNegListen = proc("negatum-listen").in(ensNegListen) {
      import de.sciss.synth.proc.graph._
      import Ops._
      import ugen._

      val indexIn     = "bus-in".ir
      val numCh       = 1 // NumChannels(indicesIn)
      val in          = PhysicalIn.ar(indices = indexIn).out(0)
      val fftSize     = 1024
      val fftBuf      = LocalBuf(numFrames = fftSize, numChannels = numCh)
      val fft         = FFT(buf = fftBuf, in = in, hop = 0.5, winType = 1)
      val loudness    = Loudness(fft)
      val threshStart = "thresh-start".ir( 10f)
      val threshEnd   = "thresh-end"  .ir(  1f)
      val threshDur   = "thresh-dur"  .ir(180f)
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
    aNegRecDone("som-play")       = ensSOMPlay

    aNegStart ("done")            = aNegRecDone
    aNegRec   ("done")            = aNegRecDone
    pNegListen("done")            = aNegRecDone
    aNegRecDone("restart")        = aNegStart

    val aSOMTimeline = action("som-timeline").in(f)(ActionSOMTimeline)
    aSOMTimeline("som-folder")    = fSOM
    aSOMTimeline("som-play")      = ensSOMPlay
    aNegRecDone("som-timeline")   = aSOMTimeline
  }

  trait NoSys extends SSys[NoSys]
}