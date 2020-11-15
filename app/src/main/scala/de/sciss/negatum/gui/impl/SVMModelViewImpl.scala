/*
 *  SVMModelViewImpl.scala
 *  (SVMModel)
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
package gui
package impl

import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.LucreSwing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.mellite.{GUI, ObjView}
import de.sciss.proc.Universe
import javax.swing.TransferHandler
import javax.swing.TransferHandler.TransferSupport

import scala.concurrent.stm.Ref
import scala.swing.{Action, Alignment, BorderPanel, Component, FlowPanel, Label, ProgressBar, Swing}
import scala.util.{Failure, Success}

object SVMModelViewImpl {
  def apply[S <: Sys[S]](m: SVMModel[S])(implicit tx: S#Tx, universe: Universe[S]): SVMModelView[S] = {
//    implicit val undo = new UndoManagerImpl
    val res = new Impl[S](tx.newHandle(m))
    res.init(m)
  }

  private final class Impl[S <: Sys[S]](modelH: stm.Source[S#Tx, SVMModel[S]])
                                       (implicit val universe: Universe[S] /* , val undoManager: UndoManager */)
    extends SVMModelView[S] with ComponentHolder[Component] {

    type C = Component

    def init(n: SVMModel[S])(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      this
    }

    private[this] val renderRef = Ref(Option.empty[Rendering[S, Int]])

    def model    (implicit tx: S#Tx): SVMModel[S]               = modelH()
    def rendering(implicit tx: S#Tx): Option[Rendering[S, Int]] = renderRef.get(tx.peer)

    private def guiInit(): Unit = {
      val ggProgress: ProgressBar = new ProgressBar
      ggProgress.max = 160

      val actionCancel: swing.Action = new swing.Action(null) {
        def apply(): Unit = cursor.step { implicit tx =>
          renderRef.swap(None)(tx.peer).foreach(_.cancel())
        }
        enabled = false
      }

      val ggCancel  = GUI.toolButton(actionCancel, raphael.Shapes.Cross, tooltip = "Abort Rendering")

      val ggDrop = new Label("Drop Negatum here", raphael.Icon()(raphael.Shapes.Biohazard), Alignment.Leading)

      ggDrop.peer.setTransferHandler(new TransferHandler {
        override def canImport(support: TransferSupport): Boolean = {
          val res = support.isDataFlavorSupported(ObjView.Flavor) && renderRef.single.get.isEmpty
          if (res) support.setDropAction(TransferHandler.LINK)
          res
        }

        override def importData(support: TransferSupport): Boolean = {
          val drag = support.getTransferable.getTransferData(ObjView.Flavor).asInstanceOf[ObjView.Drag[_]]
          renderRef.single.get.isEmpty && drag.universe.workspace == universe.workspace &&
            drag.view.isInstanceOf[NegatumObjView[_]] && {

            val view = drag.view.asInstanceOf[NegatumObjView[S]]
            ggDrop.text = view.name
            runPrediction(view.objH)
            // mList += new ListEntry(view.name, view.objH)
            true
          }
        }
      })

      def runPrediction(nH: stm.Source[S#Tx, Negatum[S]]): Unit = {
        val ok = cursor.step { implicit tx =>
          renderRef.get(tx.peer).isEmpty && {
            val obj = modelH()

            def finished(num: Int)(implicit tx: S#Tx): Unit = {
              renderRef.set(None)(tx.peer)
              deferTx {
                ggDrop.text           = if (num < 0) "" else s"Selected $num objects."
                actionCancel.enabled  = false
              }
            }

            val n: Negatum[S] = nH()
            val rendering = obj.predict(n)
            /* val obs = */ rendering.reactNow { implicit tx => {
              case Rendering.Completed(Success(num)) => finished(num)
              case Rendering.Completed(Failure(Rendering.Cancelled())) => finished(-1)
              case Rendering.Completed(Failure(ex)) =>
                finished(-1)
                deferTx(ex.printStackTrace())
              case Rendering.Progress(amt) =>
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
        }
      }

      val actionPrintConfig = Action("Print Config") {
        val config = cursor.step { implicit tx =>
          modelH().config
        }
        println(config)
      }
      val ggPrintConfig = GUI.toolButton(actionPrintConfig, raphael.Shapes.Printer)

      val actionPrintStats = Action("Print Stats") {
        val stats = cursor.step { implicit tx =>
          modelH().stats
        }
        println(stats)
      }
      val ggPrintStats = GUI.toolButton(actionPrintStats, raphael.Shapes.Printer)

      val panelDrop = new FlowPanel(ggDrop)
      panelDrop.border = Swing.EmptyBorder(16)

      val panelControl = new FlowPanel(ggProgress, ggCancel, ggPrintConfig, ggPrintStats)
      component = new BorderPanel {
        add(panelDrop   , BorderPanel.Position.Center)
        add(panelControl, BorderPanel.Position.South )
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = rendering.foreach(_.cancel())
  }
}