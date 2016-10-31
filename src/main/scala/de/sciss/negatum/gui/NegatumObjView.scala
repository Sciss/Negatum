/*
 *  NegatumObjView.scala
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

package de.sciss.negatum.gui

import de.sciss.desktop
import de.sciss.desktop.OptionPane
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.ListObjViewImpl.NonEditable
import de.sciss.mellite.gui.impl.{AudioCueObjView, ListObjViewImpl, ObjViewImpl, WindowImpl}
import de.sciss.mellite.gui.{AttrCellView, ListObjView, ObjView}
import de.sciss.negatum.Negatum
import de.sciss.synth.proc.{AudioCue, Workspace}

object NegatumObjView extends ListObjView.Factory {
  type E[~ <: stm.Sys[~]] = Negatum[~]
  val icon          = ObjViewImpl.raphaelIcon(raphael.Shapes.Biohazard)
  val prefix        = "Negatum"
  def humanName     = prefix
  def tpe           = Negatum
  def category      = ObjView.categComposition
  def hasMakeDialog = true

  private[this] lazy val _init: Unit = ListObjView.addFactory(this)

  def init(): Unit = {
    _init
    SVMModelObjView.init()
    SOMObjView     .init()
  }

  def mkListView[S <: Sys[S]](obj: Negatum[S])(implicit tx: S#Tx): NegatumObjView[S] with ListObjView[S] =
    new Impl(tx.newHandle(obj)).initAttrs(obj)

  final case class Config[S <: stm.Sys[S]](name: String, audioCue: AudioCueObjView.Config1[S])

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (ok: Config[S] => Unit)
                                 (implicit cursor: stm.Cursor[S]): Unit = {
    val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
      messageType = OptionPane.Message.Question, initial = prefix)
    opt.title = s"New $prefix"
    val res = opt.show(window)
    res.foreach { name =>
      AudioCueObjView.initMakeDialog(workspace, window) { cueConfig =>
        cueConfig.headOption.foreach { audioCue =>
          val config = Config(name = name, audioCue = audioCue)
          ok(config)
        }
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
      with ListObjView[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.EmptyRenderer[S]
      with NonEditable[S]
      /* with NonViewable[S] */ {

    override def obj(implicit tx: S#Tx) = objH()

    type E[~ <: stm.Sys[~]] = Negatum[~]

    def factory = NegatumObjView

    def isViewable = true

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      val _obj      = objH()
      val title     = AttrCellView.name(_obj)
      val _view     = NegatumView(_obj)
      val frame     = new WindowImpl[S](title) {
        val view = _view
      }
      frame.init()
      Some(frame)
    }
  }
}
trait NegatumObjView[S <: stm.Sys[S]] extends ObjView[S] {
  override def objH: stm.Source[S#Tx , Negatum[S]]
  override def obj(implicit tx: S#Tx): Negatum[S]
}