package de.sciss.negatum

import javax.swing.SpinnerNumberModel

import de.sciss.desktop
import de.sciss.desktop.OptionPane
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.swing.{View, Window, deferTx}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.ListObjViewImpl.NonEditable
import de.sciss.mellite.gui.impl.{AudioCueObjView, ListObjViewImpl, ObjViewImpl, WindowImpl}
import de.sciss.mellite.gui.{AttrCellView, GUI, ListObjView, ObjView}
import de.sciss.swingplus.Spinner
import de.sciss.synth.proc.{AudioCue, Workspace}

import scala.concurrent.stm.Ref
import scala.swing.{Component, FlowPanel, Label, ProgressBar}

object NegatumObjView extends ListObjView.Factory {
  type E[~ <: stm.Sys[~]] = Negatum[~]
  val icon          = ObjViewImpl.raphaelIcon(raphael.Shapes.Biohazard)
  val prefix        = "Negatum"
  def humanName     = prefix
  def tpe           = Negatum
  def category      = ObjView.categComposition
  def hasMakeDialog = true

  private[this] lazy val _init: Unit = ListObjView.addFactory(this)

  def init(): Unit = _init

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

    private def mkComponent()(implicit cursor: stm.Cursor[S], workspace: Workspace[S]): Component = {
      val renderRef = Ref(Option.empty[Negatum.Rendering[S]])

      val ggProgress: ProgressBar = new ProgressBar
      ggProgress.max = 160

      val actionCancel: swing.Action = new swing.Action(null) {
        def apply(): Unit = cursor.step { implicit tx =>
          renderRef.swap(None)(tx.peer).foreach(_.cancel())
        }
        enabled = false
      }

      val ggCancel = GUI.toolButton(actionCancel, raphael.Shapes.Cross, tooltip = "Abort Rendering")

      val mNumIter  = new SpinnerNumberModel(1, 1, 65536, 1)
      val ggNumIter = new Spinner(mNumIter)

      // XXX TODO --- should use custom view so we can cancel upon `dispose`
      val actionRender = new swing.Action("Render") { self =>
        def apply(): Unit = {
          val numIter = mNumIter.getNumber.intValue()
          val ok = cursor.step { implicit tx =>
            renderRef.get(tx.peer).isEmpty && {
              val obj       = objH()
              val cGen      = Negatum.Generation(population = 10)
              val config    = Negatum.Config(generation = cGen)

              def finished()(implicit tx: S#Tx): Unit = {
                renderRef.set(None)(tx.peer)
                deferTx {
                  actionCancel.enabled  = false
                  self.enabled          = true
                }
              }

              val rendering = obj.run(config, iter = numIter)
              /* val obs = */ rendering.reactNow { implicit tx => {
                case Negatum.Rendering.Success => finished()
                case Negatum.Rendering.Failure(Negatum.Rendering.Cancelled()) => finished()
                case Negatum.Rendering.Failure(ex) =>
                  finished()
                  deferTx(ex.printStackTrace())
                case Negatum.Rendering.Progress(amt) =>
                  deferTx {
                    ggProgress.value = (amt * ggProgress.max).toInt
                  }
              }}
              renderRef.set(Some(rendering))(tx.peer)
              true
            }
          }
          if (ok) {
            actionCancel.enabled = true
            self        .enabled = false
          }
        }
      }
      val ggRender = GUI.toolButton(actionRender, raphael.Shapes.Biohazard)

      //            val ggDebug = Button("Debug") {
      //              renderRef.single.get.foreach { r =>
      //                val ctrl = r.control
      //                println(ctrl.stats)
      //                ctrl.debugDotGraph()
      //              }
      //            }

      val component = new FlowPanel(new Label("Iterations:"), ggNumIter, ggProgress, ggCancel, ggRender)
      component
    }

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      val title     = AttrCellView.name(obj)
      val _view     = View.wrap[S](mkComponent())
      val frame     = new WindowImpl[S](title) {
        val view = _view
      }
      frame.init()
      Some(frame)
    }
  }
}
trait NegatumObjView[S <: stm.Sys[S]] extends ObjView[S] {
  override def obj(implicit tx: S#Tx): Negatum[S]
}