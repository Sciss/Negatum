/*
 *  NegatumObjView.scala
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

package de.sciss.negatum.gui

import de.sciss.desktop
import de.sciss.desktop.OptionPane
import de.sciss.icons.raphael
import de.sciss.lucre.expr.CellView
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.swing.{View, Window}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.mellite.gui.impl.audiocue.AudioCueObjView
import de.sciss.mellite.gui.impl.objview.ObjListViewImpl.{EmptyRenderer, NonEditable}
import de.sciss.mellite.gui.impl.objview.ObjViewImpl
import de.sciss.mellite.gui.{ObjListView, ObjView}
import de.sciss.negatum.Negatum
import de.sciss.processor.Processor.Aborted
import de.sciss.synth.proc.{AudioCue, Universe}
import javax.swing.Icon

import scala.util.{Failure, Success}

object NegatumObjView extends ObjListView.Factory {
  type E[~ <: stm.Sys[~]] = Negatum[~]
  val icon          : Icon      = ObjViewImpl.raphaelIcon(raphael.Shapes.Biohazard)
  val prefix        : String    = "Negatum"
  def humanName     : String    = prefix
  def tpe           : Obj.Type  = Negatum
  def category      : String    = ObjView.categComposition
  def hasMakeDialog : Boolean   = true

  private[this] lazy val _init: Unit = ObjListView.addFactory(this)

  def init(): Unit = {
    _init
    SVMModelObjView.init()
    SOMObjView     .init()
  }

  def mkListView[S <: Sys[S]](obj: Negatum[S])(implicit tx: S#Tx): NegatumObjView[S] with ObjListView[S] =
    new Impl(tx.newHandle(obj)).initAttrs(obj)

  final case class Config[S <: stm.Sys[S]](name: String, audioCue: AudioCueObjView.Config1[S])

  def canMakeObj: Boolean = true

  override def initMakeCmdLine[S <: Sys[S]](args: List[String])(implicit universe: Universe[S]): MakeResult[S] =
    Failure(new NotImplementedError("Make Negatum from command line"))

  def initMakeDialog[S <: Sys[S]](window: Option[desktop.Window])(done: MakeResult[S] => Unit)
                                 (implicit universe: Universe[S]): Unit = {
    val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
      messageType = OptionPane.Message.Question, initial = prefix)
    opt.title = s"New $prefix"
    val res = opt.show(window)
    res.foreach { name =>
      AudioCueObjView.initMakeDialog[S](window) {
        case Success(cueConfig) =>
          cueConfig.headOption match {
            case Some(audioCue) =>
              val config = Config(name = name, audioCue = audioCue)
              done(Success(config))

            case None =>
              done(Failure(Aborted()))
          }

        case Failure(ex) =>
          done(Failure(ex))
      }
    }
  }

  def makeObj[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = {
    val res1 = AudioCueObjView.makeObj(config.audioCue :: Nil)
    val templateOpt = res1.collectFirst {
      case a: AudioCue.Obj[S] => a
    }
    templateOpt.fold(res1) { template =>
      val obj  = Negatum[S](template)
      import de.sciss.synth.proc.Implicits._
      if (!config.name.isEmpty) obj.name = config.name
      obj :: obj.population :: res1 // expose population until we have a proper editor
    }
  }

  final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, Negatum[S]])
    extends NegatumObjView[S]
      with ObjListView[S]
      with ObjViewImpl.Impl[S]
      with EmptyRenderer[S]
      with NonEditable[S]
      /* with NonViewable[S] */ {

    override def obj(implicit tx: S#Tx): Negatum[S] = objH()

    type E[~ <: stm.Sys[~]] = Negatum[~]

    def factory: ObjView.Factory = NegatumObjView

    def isViewable = true

    def openView(parent: Option[Window[S]])(implicit tx: S#Tx, universe: Universe[S]): Option[Window[S]] = {
      val _obj      = objH()
      val title     = CellView.name(_obj)
      val _view     = NegatumView(_obj)
      val frame: WindowImpl[S] = new WindowImpl[S](title) {
        val view: View[S] = _view
      }
      frame.init()
      Some(frame)
    }
  }
}
trait NegatumObjView[S <: stm.Sys[S]] extends ObjView[S] {
  type Repr = Negatum[S]
}