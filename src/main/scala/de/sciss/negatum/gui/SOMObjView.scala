/*
 *  SOMObjView.scala
 *  (SOM)
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
package gui

import javax.swing.TransferHandler.TransferSupport
import javax.swing.{SpinnerNumberModel, TransferHandler}

import de.sciss.desktop
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{CellView, Window, defer, deferTx}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.ListObjViewImpl.NonEditable
import de.sciss.mellite.gui.impl.{ListObjViewImpl, ObjViewImpl, WindowImpl}
import de.sciss.mellite.gui.{AttrCellView, GUI, ListObjView, ObjView, ViewHasWorkspace}
import de.sciss.negatum.SVMConfig.{Kernel, Type}
import de.sciss.processor.Processor
import de.sciss.swingplus.{ComboBox, GroupPanel, ListView, Separator, Spinner}
import de.sciss.synth.proc
import de.sciss.synth.proc.{Folder, Workspace}

import scala.collection.breakOut
import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.ExecutionContext
import scala.concurrent.stm.Ref
import scala.swing.event.SelectionChanged
import scala.swing.{Action, BorderPanel, BoxPanel, CheckBox, Component, FlowPanel, Label, Orientation, ProgressBar, ScrollPane, TextField}
import scala.util.{Failure, Success}

object SOMObjView extends ListObjView.Factory {
  type E[~ <: stm.Sys[~]] = SOM[~]
  val icon          = ObjViewImpl.raphaelIcon(raphael.Shapes.Safari)
  val prefix        = "SOM"
  def humanName     = "Self Organizing Map"
  def tpe           = SOM
  def category      = ObjView.categComposition
  def hasMakeDialog = true

  private[this] lazy val _init: Unit = ListObjView.addFactory(this)

  def init(): Unit = _init

  def mkListView[S <: Sys[S]](obj: SOM[S])(implicit tx: S#Tx): SOMObjView[S] with ListObjView[S] =
    new Impl(tx.newHandle(obj)).initAttrs(obj)

  final case class Config[S <: stm.Sys[S]](name: String, peer: SOM.Config)

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (ok: Config[S] => Unit)
                                 (implicit cursor: stm.Cursor[S]): Unit = {
    cursor.step { implicit tx =>
      implicit val ws: Workspace[S] = workspace
      val _view = new MakeViewImpl[S](ok)
      val frame = new WindowImpl[S](CellView.const[S, String](s"New $prefix")) {
        val view = _view
      }
      _view.init(frame)
      frame.init()
    }
  }

  def makeObj[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = {
    val obj = SOM(config.peer)
    import proc.Implicits._
    if (!config.name.isEmpty) obj.name = config.name
    obj :: Nil
  }

  final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, SOM[S]])
    extends SOMObjView[S]
      with ListObjView[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.EmptyRenderer[S]
      with NonEditable[S]
      /* with NonViewable[S] */ {

    override def obj(implicit tx: S#Tx) = objH()

    type E[~ <: stm.Sys[~]] = SOM[~]

    def factory = SOMObjView

    def isViewable = true

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      val _obj      = objH()
      val title     = AttrCellView.name(_obj)
      val _view     = SOMView(_obj)
      val frame     = new WindowImpl[S](title) {
        val view = _view
      }
      frame.init()
      Some(frame)
    }
  }

  private final class MakeViewImpl[S <: Sys[S]](ok: Config[S] => Unit)
                                               (implicit val workspace: Workspace[S], val cursor: stm.Cursor[S])
    extends ViewHasWorkspace[S] with ComponentHolder[Component] { impl =>

    private[this] var _frame: Window[S] = _

    def init(frame: Window[S])(implicit tx: S#Tx): this.type = {
      _frame = frame
      deferTx(guiInit())
      this
    }

    def close()(implicit tx: S#Tx): Unit = _frame.dispose()

    private final class ListEntry(val name: String, val folderH: stm.Source[S#Tx, Folder[S]]) {
      override def toString = name
    }

    private def guiInit(): Unit = {
      val builder           = SOM.Config()

      val mFeatures         = new SpinnerNumberModel(builder.features, 1, 1024, 1)
      val ggFeatures        = new Spinner(mFeatures)
      val lbFeatures        = new Label("# of Features:")

      val mDimensions       = new SpinnerNumberModel(builder.dimensions, 1, 64, 1)
      val ggDimensions      = new Spinner(mDimensions)
      val lbDimensions      = new Label("Map Dimension:")

      val mExtent           = new SpinnerPowerOfTwoModel(builder.extent, 1, 0x40000000)
      val ggExtent          = new Spinner(mExtent)
      val lbExtent          = new Label("Extent:")
      ggExtent.tooltip      = "Map half side length"

      val mGridStep         = new SpinnerNumberModel(builder.gridStep, 1, 0x40000000, 1)
      val ggGridStep        = new Spinner(mGridStep)
      val lbGridStep        = new Label("Grid Step:")

//      val mMaxNodes         = new SpinnerNumberModel(builder.maxNodes, 1, 0x1000000, 1)
//      val ggMaxNodes        = new Spinner(mMaxNodes)
//      val lbMaxNodes        = new Label("Max. # of Nodes:")

      val mNumIter          = new SpinnerNumberModel(builder.numIterations, 1, 0x1000000, 1)
      val ggNumIter         = new Spinner(mNumIter)
      val lbNumIter         = new Label("Estim. # of Iterations:")

      // XXX TODO --- how to determine the bloody preferred size
      val mLearningCoef     = new SpinnerNumberModel(builder.learningCoef, 0.0001, 1.1111, 0.01)
      val ggLearningCoef    = new Spinner(mLearningCoef)
      val lbLearningCoef    = new Label("Learning Coefficient:")

      val mSeed             = new SpinnerNumberModel
      mSeed.setMinimum(Long.MinValue)
      mSeed.setMinimum(Long.MaxValue)
      mSeed.setValue(0L)
      mSeed.setStepSize(1L)
      val ggSeed            = new Spinner(mSeed)
      val lbSeed            = new Label("RNG Seed:")

      val lbName = new Label("Name:")
      val ggName = new TextField("SOM", 12)

      /*
            features      max-nodes
            dimensions    num-iter
            extent        coef
            grid-step
       */

      val pGroup = new GroupPanel {
        horizontal = Seq(
          Par(lbFeatures, lbExtent, lbGridStep),
          Par(ggFeatures, ggExtent, ggGridStep),
          Par(lbDimensions, lbNumIter, lbLearningCoef),
          Par(ggDimensions, ggNumIter, ggLearningCoef)
        )
        vertical = Seq(
          Par(Baseline)(lbFeatures  , ggFeatures  , lbDimensions  , ggDimensions),
          Par(Baseline)(lbExtent    , ggExtent    , lbNumIter     , ggNumIter),
          Par(Baseline)(lbGridStep  , ggGridStep  , lbLearningCoef, ggLearningCoef)
        )
      }

      lazy val actionCancel: Action = Action("Cancel") {
        impl.cursor.step { implicit tx =>
          close()
        }
      }

      implicit class SpinnerValues(m: SpinnerNumberModel) {
        def toInt   : Int     = m.getNumber.intValue
        def toLong  : Long    = m.getNumber.longValue
        def toDouble: Double  = m.getNumber.doubleValue
      }

      def updateConfig(): Unit = {
        import builder._
        features      = mFeatures     .toInt
        dimensions    = mDimensions   .toInt
        extent        = mExtent       .toInt
        gridStep      = mGridStep     .toInt
//        maxNodes      = mMaxNodes     .toInt
        numIterations = mNumIter      .toInt
        learningCoef  = mLearningCoef .toDouble
        seed          = mSeed         .toLong
      }

      val actionCreate = Action("Ok") {
        updateConfig()
        val name = ggName.text
        ok(Config(name, builder))
        impl.cursor.step { implicit tx =>
          close()
        }
      }

      val ggCancel        = GUI.toolButton(actionCancel, raphael.Shapes.Cross)
      val ggCreate        = GUI.toolButton(actionCreate, raphael.Shapes.Check)
      val pBottom         = new FlowPanel(ggCancel, ggCreate)

      component = new BorderPanel {
        add(new FlowPanel(lbName, ggName), BorderPanel.Position.North)
        add(pGroup , BorderPanel.Position.Center)
        add(pBottom, BorderPanel.Position.South )
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}
trait SOMObjView[S <: stm.Sys[S]] extends ObjView[S] {
  override def objH: stm.Source[S#Tx , SOM[S]]
  override def obj(implicit tx: S#Tx): SOM[S]
}