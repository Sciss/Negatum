/*
 *  NegatumObjView.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
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
import de.sciss.lucre.{Obj, Source, Txn => LTxn}
import de.sciss.lucre.swing.{View, Window}
import de.sciss.lucre.synth.Txn
import de.sciss.mellite.impl.WindowImpl
import de.sciss.mellite.impl.objview.ObjListViewImpl.{EmptyRenderer, NonEditable}
import de.sciss.mellite.impl.objview.ObjViewImpl
import de.sciss.mellite.{AudioCueObjView, ObjListView, ObjView}
import de.sciss.negatum.Negatum
import de.sciss.processor.Processor.Aborted
import de.sciss.proc.{AudioCue, Universe}
import javax.swing.Icon

import scala.util.{Failure, Success}

object NegatumObjView extends ObjListView.Factory {
  type E[~ <: LTxn[~]] = Negatum[~]
  val icon          : Icon      = ObjViewImpl.raphaelIcon(raphael.Shapes.Biohazard)
  val prefix        : String    = "Negatum"
  def humanName     : String    = prefix
  def tpe           : Obj.Type  = Negatum
  def category      : String    = ObjView.categComposition
  def hasMakeDialog : Boolean   = true

  private[this] lazy val _init: Unit = ObjListView.addFactory(this)

  def init(): Unit = {
    _init
//    SVMModelObjView.init()
//    SOMObjView     .init()
  }

  def mkListView[T <: Txn[T]](obj: Negatum[T])(implicit tx: T): NegatumObjView[T] with ObjListView[T] =
    new Impl(tx.newHandle(obj)).initAttrs(obj)

  final case class Config[T <: LTxn[T]](name: String, audioCue: AudioCueObjView.SingleConfig[T])

  def canMakeObj: Boolean = true

  override def initMakeCmdLine[T <: Txn[T]](args: List[String])(implicit universe: Universe[T]): MakeResult[T] =
    Failure(new NotImplementedError("Make Negatum from command line"))

  def initMakeDialog[T <: Txn[T]](window: Option[desktop.Window])(done: MakeResult[T] => Unit)
                                 (implicit universe: Universe[T]): Unit = {
    val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
      messageType = OptionPane.Message.Question, initial = prefix)
    opt.title = s"New $prefix"
    val res = opt.show(window)
    res.foreach { name =>
      AudioCueObjView.initMakeDialog[T](window) {
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

  def makeObj[T <: Txn[T]](config: Config[T])(implicit tx: T): List[Obj[T]] = {
    val res1 = AudioCueObjView.makeObj(config.audioCue :: Nil)
    val templateOpt = res1.collectFirst {
      case a: AudioCue.Obj[T] => a
    }
    templateOpt.fold(res1) { template =>
      val obj  = Negatum[T](template)
      import de.sciss.proc.Implicits._
      if (!config.name.isEmpty) obj.name = config.name
      obj :: obj.population :: res1 // expose population until we have a proper editor
    }
  }

  final class Impl[T <: Txn[T]](val objH: Source[T, Negatum[T]])
    extends NegatumObjView[T]
      with ObjListView[T]
      with ObjViewImpl.Impl[T]
      with EmptyRenderer[T]
      with NonEditable[T]
      /* with NonViewable[T] */ {

    override def obj(implicit tx: T): Negatum[T] = objH()

    type E[~ <: LTxn[~]] = Negatum[~]

    def factory: ObjView.Factory = NegatumObjView

    def isViewable = true

    def openView(parent: Option[Window[T]])(implicit tx: T, universe: Universe[T]): Option[Window[T]] = {
      val _obj      = objH()
      val title     = CellView.name(_obj)
      val _view     = NegatumView(_obj)
      val frame: WindowImpl[T] = new WindowImpl[T](title) {
        val view: View[T] = _view
      }
      frame.init()
      Some(frame)
    }
  }
}
trait NegatumObjView[T <: LTxn[T]] extends ObjView[T] {
  type Repr = Negatum[T]
}