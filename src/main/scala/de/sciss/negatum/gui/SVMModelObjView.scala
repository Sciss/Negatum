/*
 *  SVMModelObjView.scala
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
package gui

import javax.swing.TransferHandler.TransferSupport
import javax.swing.{Icon, SpinnerNumberModel, TransferHandler}

import de.sciss.desktop
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{CellView, View, Window, defer, deferTx}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.ListObjViewImpl.NonEditable
import de.sciss.mellite.gui.impl.{ListObjViewImpl, ObjViewImpl, WindowImpl}
import de.sciss.mellite.gui.{AttrCellView, GUI, ListObjView, ObjView, ViewHasWorkspace}
import de.sciss.negatum.SVMConfig.{Kernel, Type}
import de.sciss.processor.Processor
import de.sciss.swingplus.{ComboBox, GroupPanel, ListView, Separator, Spinner}
import de.sciss.synth.proc
import de.sciss.synth.proc.Workspace

import scala.collection.breakOut
import scala.concurrent.stm.Ref
import scala.swing.event.SelectionChanged
import scala.swing.{Action, BorderPanel, BoxPanel, CheckBox, Component, FlowPanel, Label, Orientation, ProgressBar, ScrollPane, TextField}
import scala.util.{Failure, Success}

object SVMModelObjView extends ListObjView.Factory {
  type E[~ <: stm.Sys[~]] = SVMModel[~]
  val icon: Icon        = ObjViewImpl.raphaelIcon(Shapes.Category)
  val prefix            = "SVMModel"
  def humanName         = "SVM Model"
  def tpe               = SVMModel
  def category: String  = ObjView.categComposition
  def hasMakeDialog     = true

  private[this] lazy val _init: Unit = ListObjView.addFactory(this)

  def init(): Unit = _init

  def mkListView[S <: Sys[S]](obj: SVMModel[S])(implicit tx: S#Tx): SVMModelObjView[S] with ListObjView[S] =
    new Impl(tx.newHandle(obj)).initAttrs(obj)

  final case class Config[S <: stm.Sys[S]](name: String, peer: stm.Source[S#Tx, SVMModel[S]])

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (ok: Config[S] => Unit)
                                 (implicit cursor: stm.Cursor[S]): Unit = {
    cursor.step { implicit tx =>
      implicit val ws: Workspace[S] = workspace
      val _view = new MakeViewImpl[S](ok)
      val frame = new WindowImpl[S](CellView.const[S, String](s"New $prefix")) {
        val view: View[S] = _view
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

  final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, SVMModel[S]])
    extends SVMModelObjView[S]
      with ListObjView[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.EmptyRenderer[S]
      with NonEditable[S]
      /* with NonViewable[S] */ {

    override def obj(implicit tx: S#Tx): SVMModel[S] = objH()

    type E[~ <: stm.Sys[~]] = SVMModel[~]

    def factory = SVMModelObjView

    def isViewable = true

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      val _obj      = objH()
      val title     = AttrCellView.name(_obj)
      val _view     = SVMModelView(_obj)
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
    private[this] val processor = Ref(Option.empty[Processor[SVMModel.Trained[S]]])
    // private[this] val aborted   = Ref(false)

    def init(frame: Window[S])(implicit tx: S#Tx): this.type = {
      _frame = frame
      deferTx(guiInit())
      this
    }

    def close()(implicit tx: S#Tx): Unit = _frame.dispose()

    private final class ListEntry(val name: String, val negatumH: stm.Source[S#Tx, Negatum[S]]) {
      override def toString: String = name
    }

    private def guiInit(): Unit = {
      val builder           = SVMConfig()

      val mList             = ListView.Model.empty[ListEntry]
      val ggList            = new ListView(mList)
      ggList.peer.setTransferHandler(new TransferHandler {
        override def canImport(support: TransferSupport): Boolean = {
          val res = support.isDataFlavorSupported(ListObjView.Flavor)
          if (res) support.setDropAction(TransferHandler.LINK)
          res
        }

        override def importData(support: TransferSupport): Boolean = {
          val drag = support.getTransferable.getTransferData(ListObjView.Flavor).asInstanceOf[ListObjView.Drag[_]]
          drag.workspace == workspace && drag.view.isInstanceOf[NegatumObjView[_]] && {
            val view = drag.view.asInstanceOf[NegatumObjView[S]]
            mList += new ListEntry(view.name, view.objH)
            true
          }
        }
      })

      val scrollList          = new ScrollPane(ggList)
      scrollList.minimumSize  = scrollList.preferredSize
      scrollList.maximumSize  = scrollList.preferredSize

      val mTypeParamC       = new SpinnerNumberModel(1.0, 0.0, 1000, 1.0)
      val ggTypeParamC      = new Spinner(mTypeParamC)
      val lbTypeParamC      = new Label("C:")
      ggTypeParamC.tooltip  = "C (cost) parameter in C-SVC, ε-SVR, ν-SVR"

      val mTypeParamNu      = new SpinnerNumberModel(0.5, 0.0, 1000, 0.1)  // XXX TODO --- what should be min/max?
      val ggTypeParamNu     = new Spinner(mTypeParamNu)
      val lbTypeParamNu     = new Label("ν:")
      ggTypeParamNu.tooltip = "ν parameter in ν-SVC, One Class, ν-SVR"

      val mTypeParamP       = new SpinnerNumberModel(0.1, 0.0, 1000, 0.1)  // XXX TODO --- what should be min/max?
      val ggTypeParamP      = new Spinner(mTypeParamP)
      val lbTypeParamP      = new Label("p:")
      ggTypeParamP.tooltip  = "p or ε parameter in ε-SVR"

      def updateTypeParam(id: Int): Unit = {
        val hasC  = id == Type.CSVC .id || id == Type.EpsilonSVR.id || id == Type.NuSVR.id
        val hasNu = id == Type.NuSVC.id || id == Type.OneClass  .id || id == Type.NuSVR.id
        val hasP  = id == Type.EpsilonSVR.id
        ggTypeParamC .enabled = hasC
        ggTypeParamNu.enabled = hasNu
        ggTypeParamP .enabled = hasP
      }

      updateTypeParam(builder.tpe.id)

      val ggType        = new ComboBox(Seq("C-SVC", "ν-SVC", "One Class", "ε-SVR", "ν-SVR")) {
        selection.index = builder.tpe.id
        listenTo(selection)
        reactions += {
          case SelectionChanged(_) => updateTypeParam(selection.index)
        }
      }
      val lbType        = new Label("Type:")

      val mKernelParamDegree  = new SpinnerNumberModel(1, 0, 100, 1)
      val ggKernelParamDegree = new Spinner(mKernelParamDegree)
      val lbKernelParamDegree = new Label("Degree:")
      ggKernelParamDegree.tooltip  = "Degree of polynomial kernel"

      val mKernelParamGamma   = new SpinnerNumberModel(0.05, 0.0, 10.11, 0.05)
      val ggKernelParamGamma  = new Spinner(mKernelParamGamma)
      val lbKernelParamGamma  = new Label("γ:")
      ggKernelParamGamma.tooltip  = "γ parameter of polynomial, radial, or sigmoid kernel"

      val mKernelParamCoef0   = new SpinnerNumberModel(0.0, 0.0, 10.11, 0.1)
      val ggKernelParamCoef0  = new Spinner(mKernelParamCoef0)
      val lbKernelParamCoef0  = new Label("Coef0:")
      ggKernelParamCoef0.tooltip  = "coef0 coefficient of polynomial or sigmoid kernel"

      def updateKernelParam(id: Int): Unit = {
        val hasDegree = id == Kernel.Poly.id
        val hasGamma  = id == Kernel.Poly.id || id == Kernel.Sigmoid.id || id == Kernel.Radial.id
        val hasCoef0  = id == Kernel.Poly.id || id == Kernel.Sigmoid.id
        ggKernelParamDegree .enabled = hasDegree
        ggKernelParamGamma  .enabled = hasGamma
        ggKernelParamCoef0  .enabled = hasCoef0
      }

      updateKernelParam(builder.kernel.id)

      val ggKernel      = new ComboBox(Seq("Linear", "Polynomial", "Radial Basis Function", "Sigmoid")) {
        selection.index = builder.kernel.id
        listenTo(selection)
        reactions += {
          case SelectionChanged(_) => updateKernelParam(selection.index)
        }
      }
      val lbKernel      = new Label("Kernel:")

      val mCacheSize    = new SpinnerNumberModel(builder.cacheSize, 0.2, 1024, 0.2)
      val ggCacheSize   = new Spinner(mCacheSize)
      val lbCacheSize   = new Label("Cache size [MB]:")
  
      val mEpsilon      = new SpinnerNumberModel(builder.epsilon, 0.0, 1.111, 1.0e-3)
      val ggEpsilon     = new Spinner(mEpsilon)
      val lbEpsilon     = new Label("ε:")
      ggEpsilon.tooltip = "Epsilon (accuracy or radius)"

      val ggShrinking   = new CheckBox
      ggShrinking.selected = builder.shrinking
      val lbShrinking   = new Label("Shrinking:")
      ggShrinking.tooltip = "Whether to train a SVC or SVR model for probability estimates"

      val ggProbability = new CheckBox
      ggProbability.selected = builder.probability
      val lbProbability = new Label("Probability:")
      ggProbability.tooltip = "Whether to use the shrinking heuristics"

      val ggNormalize   = new CheckBox
      ggNormalize.selected = builder.normalize
      val lbNormalize   = new Label("Normalize:")
      ggNormalize.tooltip = "Whether to normalize the components of the feature vector"

      val mNumFeatures  = new SpinnerNumberModel(24 * 2, 2 * 2, 128 * 2, 1 * 2)
      val ggNumFeatures = new Spinner(mNumFeatures)
      val lbNumFeatures = new Label("# of Features:")

      val sep1  = Separator()
      val sep2  = Separator()

      val lbName = new Label("Name:")
      val ggName = new TextField("svm-model", 12)

      val pGroup = new GroupPanel {
        horizontal = Par(sep1, sep2, Seq(
          Par(lbType, lbTypeParamNu, lbKernel, lbKernelParamDegree, lbNumFeatures, lbEpsilon, lbCacheSize),
          Par(ggType, ggTypeParamNu, ggKernel, ggKernelParamDegree, ggNumFeatures, ggEpsilon, ggCacheSize),
          Par(lbTypeParamC, lbTypeParamP, lbKernelParamGamma, lbKernelParamCoef0, lbNormalize, lbShrinking, lbProbability),
          Par(ggTypeParamC, ggTypeParamP, ggKernelParamGamma, ggKernelParamCoef0, ggNormalize, ggShrinking, ggProbability)
        ))
        vertical = Seq(
          Par(Baseline)(lbType, ggType, lbTypeParamC, ggTypeParamC),
          Par(Baseline)(lbTypeParamNu, ggTypeParamNu, lbTypeParamP, ggTypeParamP),
          sep1,
          Par(Baseline)(lbKernel, ggKernel, lbKernelParamGamma, ggKernelParamGamma),
          Par(Baseline)(lbKernelParamDegree, ggKernelParamDegree, lbKernelParamCoef0, ggKernelParamCoef0),
          sep2,
          Par(Baseline)(lbNumFeatures, ggNumFeatures, lbNormalize, ggNormalize),
          Par(Baseline)(lbEpsilon, ggEpsilon, lbShrinking, ggShrinking),
          Par(Baseline)(lbCacheSize, ggCacheSize, lbProbability, ggProbability)
        )
      }

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
        builder.tpe = ggType.selection.index match {
          case Type.CSVC      .id => Type.CSVC    (c  = mTypeParamC .toFloat)
          case Type.NuSVC     .id => Type.NuSVC   (nu = mTypeParamNu.toFloat)
          case Type.OneClass  .id => Type.OneClass(nu = mTypeParamNu.toFloat)
          case Type.EpsilonSVR.id => Type.EpsilonSVR(c = mTypeParamC.toFloat, p = mTypeParamP.toFloat)
          case Type.NuSVR     .id => Type.NuSVR(c = mTypeParamC.toFloat, nu = mTypeParamNu.toFloat)
        }
        builder.kernel = ggKernel.selection.index match {
          case Kernel.Linear  .id => Kernel.Linear
          case Kernel.Poly    .id => Kernel.Poly(degree = mKernelParamDegree.toInt,
            gamma = mKernelParamGamma.toFloat, coef0 = mKernelParamCoef0.toFloat)
          case Kernel.Radial  .id => Kernel.Radial(gamma = mKernelParamGamma.toFloat)
          case Kernel.Sigmoid .id => Kernel.Sigmoid(gamma = mKernelParamGamma.toFloat,
            coef0 = mKernelParamCoef0.toFloat)
        }
        builder.cacheSize   = mCacheSize.toFloat
        builder.epsilon     = mEpsilon  .toFloat
        builder.shrinking   = ggShrinking  .selected
        builder.probability = ggProbability.selected
        builder.normalize   = ggNormalize  .selected
      }

      val ggProgress = new ProgressBar

      lazy val actionCreate: Action = Action("Create") {
        updateConfig()
        val numCoeff = mNumFeatures.getNumber.intValue >> 1
        val procOpt = if (mList.isEmpty) None else impl.cursor.step { implicit tx =>
          implicit val itx = tx.peer
          if (processor().nonEmpty) None else {
            val n: List[Negatum[S]] = mList.map(_.negatumH())(breakOut)
            val p = SVMModel.train(n, builder, numCoeff = numCoeff)
            val _procOpt = Some(p)
            processor() = _procOpt
            _procOpt
          }
        }
        procOpt.foreach { p =>
          actionCreate.enabled = false
          p.addListener {
            case res @ Processor.Progress(_, _) =>
              if (processor.single().contains(p))
                defer(ggProgress.value = res.toInt)
          }
          import de.sciss.synth.proc.SoundProcesses.executionContext
          p.onComplete { tr =>
            impl.cursor.step { implicit tx =>
              implicit val itx = tx.peer
              if (processor().contains(p)) {
                processor() = None
                close()
                deferTx {
                  actionCreate.enabled = true
                  tr match {
                    case Success(m) =>
                      ok(Config(name = ggName.text, peer = m))
                    case Failure(Processor.Aborted()) =>
                    case Failure(ex) => ex.printStackTrace()
                  }
                }
              }
            }
          }
          // p.start()
        }
      }
      val ggCancel        = GUI.toolButton(actionCancel, raphael.Shapes.Cross)
      val ggCreate        = GUI.toolButton(actionCreate, raphael.Shapes.Check)
      val pBottom         = new FlowPanel(ggCancel, ggProgress, ggCreate)

      component = new BorderPanel {
        add(new FlowPanel(lbName, ggName), BorderPanel.Position.North)
        add(pGroup, BorderPanel.Position.Center)
        add(new BoxPanel(Orientation.Vertical) {
          contents += new Label("Drop Negatum instances:")
          contents += scrollList
        }, BorderPanel.Position.East)
        add(pBottom, BorderPanel.Position.South)
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      processor.swap(None).foreach(_.abort())
    }
  }
}
trait SVMModelObjView[S <: stm.Sys[S]] extends ObjView[S] {
  override def objH: stm.Source[S#Tx , SVMModel[S]]
  override def obj(implicit tx: S#Tx): SVMModel[S]
}