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
  val icon          = ObjViewImpl.raphaelIcon(Shapes.Category)
  val prefix        = "SOM"
  def humanName     = "Self Organizing Map"
  def tpe           = SOM
  def category      = ObjView.categComposition
  def hasMakeDialog = true

  private[this] lazy val _init: Unit = ListObjView.addFactory(this)

  def init(): Unit = _init

  def mkListView[S <: Sys[S]](obj: SOM[S])(implicit tx: S#Tx): SOMObjView[S] with ListObjView[S] =
    new Impl(tx.newHandle(obj)).initAttrs(obj)

  final case class Config[S <: stm.Sys[S]](name: String, peer: stm.Source[S#Tx, SOM[S]])

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
    val obj = config.peer()
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
    private[this] val processor = Ref(Option.empty[Processor[Int]])

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

//      def features      : Int
//      def dimensions    : Int
//      def extent        : Int
//      def gridStep      : Int
//      def maxNodes      : Int
//      def numIterations : Int
//      def learningCoef  : Double
//      def seed          : Long

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

      val mMaxNodes         = new SpinnerNumberModel(builder.maxNodes, 1, 0x1000000, 1)
      val ggMaxNodes        = new Spinner(mMaxNodes)
      val lbMaxNodes        = new Label("Max. # of Nodes:")

      val mNumIter          = new SpinnerNumberModel(builder.numIterations, 1, 0x1000000, 1)
      val ggNumIter         = new Spinner(mNumIter)
      val lbNumIter         = new Label("Estim. # of Iterations:")

      val mLearningCoef     = new SpinnerNumberModel(builder.learningCoef, 0.0, 1.0, 0.01)
      val ggLearningCoef    = new Spinner(mLearningCoef)
      val lbLearningCoef    = new Label("Learning Coefficient:")

      val mSeed             = new SpinnerNumberModel
      mSeed.setMinimum(Long.MinValue)
      mSeed.setMinimum(Long.MaxValue)
      mSeed.setValue(0L)
      mSeed.setStepSize(1L)
      val ggSeed            = new Spinner(mSeed)
      val lbSeed            = new Label("RNG Seed:")

      val sep1  = Separator()
      val sep2  = Separator()

      val lbName = new Label("Name:")
      val ggName = new TextField("svm-model", 12)

//      val pGroup = new GroupPanel {
//        horizontal = Par(sep1, sep2, Seq(
//          Par(lbType, lbTypeParamNu, lbKernel, lbKernelParamDegree, lbNumFeatures, lbEpsilon, lbCacheSize),
//          Par(ggType, ggTypeParamNu, ggKernel, ggKernelParamDegree, ggNumFeatures, ggEpsilon, ggCacheSize),
//          Par(lbTypeParamC, lbTypeParamP, lbKernelParamGamma, lbKernelParamCoef0, lbNormalize, lbShrinking, lbProbability),
//          Par(ggTypeParamC, ggTypeParamP, ggKernelParamGamma, ggKernelParamCoef0, ggNormalize, ggShrinking, ggProbability)
//        ))
//        vertical = Seq(
//          Par(Baseline)(lbType, ggType, lbTypeParamC, ggTypeParamC),
//          Par(Baseline)(lbTypeParamNu, ggTypeParamNu, lbTypeParamP, ggTypeParamP),
//          sep1,
//          Par(Baseline)(lbKernel, ggKernel, lbKernelParamGamma, ggKernelParamGamma),
//          Par(Baseline)(lbKernelParamDegree, ggKernelParamDegree, lbKernelParamCoef0, ggKernelParamCoef0),
//          sep2,
//          Par(Baseline)(lbNumFeatures, ggNumFeatures, lbNormalize, ggNormalize),
//          Par(Baseline)(lbEpsilon, ggEpsilon, lbShrinking, ggShrinking),
//          Par(Baseline)(lbCacheSize, ggCacheSize, lbProbability, ggProbability)
//        )
//      }

      lazy val actionCancel: Action = Action("Cancel") {
        impl.cursor.step { implicit tx =>
          implicit val itx = tx.peer
          val p = processor.swap(None)
          // aborted() = true
          p.foreach(_.abort())
          if (p.isEmpty) close()
        }
      }

      implicit class SpinnerValues(m: SpinnerNumberModel) {
        def toFloat: Float = m.getNumber.floatValue
        def toInt  : Int   = m.getNumber.intValue
      }

      def updateConfig(): Unit = {
        ???
//        builder.tpe = ggType.selection.index match {
//          case Type.CSVC      .id => Type.CSVC    (c  = mTypeParamC .toFloat)
//          case Type.NuSVC     .id => Type.NuSVC   (nu = mTypeParamNu.toFloat)
//          case Type.OneClass  .id => Type.OneClass(nu = mTypeParamNu.toFloat)
//          case Type.EpsilonSVR.id => Type.EpsilonSVR(c = mTypeParamC.toFloat, p = mTypeParamP.toFloat)
//          case Type.NuSVR     .id => Type.NuSVR(c = mTypeParamC.toFloat, nu = mTypeParamNu.toFloat)
//        }
//        builder.kernel = ggKernel.selection.index match {
//          case Kernel.Linear  .id => Kernel.Linear
//          case Kernel.Poly    .id => Kernel.Poly(degree = mKernelParamDegree.toInt,
//            gamma = mKernelParamGamma.toFloat, coef0 = mKernelParamCoef0.toFloat)
//          case Kernel.Radial  .id => Kernel.Radial(gamma = mKernelParamGamma.toFloat)
//          case Kernel.Sigmoid .id => Kernel.Sigmoid(gamma = mKernelParamGamma.toFloat,
//            coef0 = mKernelParamCoef0.toFloat)
//        }
//        builder.cacheSize   = mCacheSize.toFloat
//        builder.epsilon     = mEpsilon  .toFloat
//        builder.shrinking   = ggShrinking  .selected
//        builder.probability = ggProbability.selected
//        builder.normalize   = ggNormalize  .selected
      }

      val ggProgress = new ProgressBar

      val actionCreate    = Action("Ok") {
        ???
      }

      val ggCancel        = GUI.toolButton(actionCancel, raphael.Shapes.Cross)
      val ggCreate        = GUI.toolButton(actionCreate, raphael.Shapes.Check)
      val pBottom         = new FlowPanel(ggCancel, ggCreate)

      component = new BorderPanel {
        add(new FlowPanel(lbName, ggName), BorderPanel.Position.North)
        // add(pGroup, BorderPanel.Position.Center)
        add(pBottom, BorderPanel.Position.South)
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      processor.swap(None).foreach(_.abort())
    }
  }
}
trait SOMObjView[S <: stm.Sys[S]] extends ObjView[S] {
  override def objH: stm.Source[S#Tx , SOM[S]]
  override def obj(implicit tx: S#Tx): SOM[S]
}