/*
 *  NegatumViewImpl.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum
package impl

import javax.swing.SpinnerNumberModel

import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.mellite.gui.GUI
import de.sciss.negatum.Negatum.Rendering
import de.sciss.swingplus.Spinner
import de.sciss.synth.proc.Workspace

import scala.concurrent.stm.Ref
import scala.swing.{Component, FlowPanel, Label, ProgressBar}

object NegatumViewImpl {
  def apply[S <: Sys[S]](n: Negatum[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                        workspace: Workspace[S]): NegatumView[S] = {
    val res = new Impl[S](tx.newHandle(n))
    res.init()
  }

  private final class Impl[S <: Sys[S]](negatumH: stm.Source[S#Tx, Negatum[S]])
                                       (implicit val cursor: stm.Cursor[S],
                                        val workspace: Workspace[S])
    extends NegatumView[S] with ComponentHolder[Component] {

    def init()(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      this
    }

    private[this] val renderRef = Ref(Option.empty[Negatum.Rendering[S]])

    def negatum  (implicit tx: S#Tx): Negatum[S]            = negatumH()
    def rendering(implicit tx: S#Tx): Option[Rendering[S]]  = renderRef.get(tx.peer)

    private def guiInit(): Unit = {
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
              val obj       = negatumH()
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

      component = new FlowPanel(new Label("Iterations:"), ggNumIter, ggProgress, ggCancel, ggRender)
    }

    def dispose()(implicit tx: S#Tx): Unit = rendering.foreach(_.cancel())
  }
}
