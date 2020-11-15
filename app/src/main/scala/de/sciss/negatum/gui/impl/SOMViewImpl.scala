/*
 *  SOMViewImpl.scala
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
package gui
package impl

import java.awt.datatransfer.Transferable
import java.awt.geom.Path2D
import java.awt.{Color, RenderingHints}

import de.sciss.icons.raphael
import de.sciss.lucre.geom.IntPoint2D
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Folder
import de.sciss.lucre.swing.LucreSwing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.{DragAndDrop, GUI, ObjListView, ObjView}
import de.sciss.numbers
import de.sciss.proc.Universe
import javax.swing.TransferHandler
import javax.swing.TransferHandler.TransferSupport

import scala.collection.mutable
import scala.concurrent.stm.Ref
import scala.swing.event.{MouseDragged, MouseEvent, MouseMoved, MousePressed, MouseReleased}
import scala.swing.{Action, Alignment, BorderPanel, Button, Component, Dimension, FlowPanel, Graphics2D, Label, Point, ProgressBar, Swing, ToggleButton}
import scala.util.{Failure, Success}

object SOMViewImpl {
  def apply[S <: Sys[S]](map: SOM[S])(implicit tx: S#Tx, universe: Universe[S]): SOMView[S] = {
//    implicit val undo = new UndoManagerImpl
    val res = new Impl[S](tx.newHandle(map), extent = map.config.extent)
    res.init(map)
  }

  private final class Impl[S <: Sys[S]](somH: stm.Source[S#Tx, SOM[S]], extent: Int)
                                       (implicit val universe: Universe[S] /* , val undoManager: UndoManager */)
    extends SOMView[S] with ComponentHolder[Component] { impl =>

    type C = Component

    def init(n: SOM[S])(implicit tx: S#Tx): this.type = {
      deferTx {
        guiInit()
      }
      this
    }

    private[this] val renderRef = Ref(Option.empty[Rendering[S, Int]])

    def map      (implicit tx: S#Tx): SOM[S]                     = somH()
    def rendering(implicit tx: S#Tx): Option[Rendering[S, Int]]  = renderRef.get(tx.peer)

    private def guiInit(): Unit = {

      val ggProgress: ProgressBar = new ProgressBar
      ggProgress.max = 160

      val lbPicked = new Label(" ")

      var viewPicked = Option.empty[ObjListView[S]]

      val actionViewPicked = Action(null) {
        viewPicked.foreach { view =>
          impl.cursor.step { implicit tx =>
            view.openView(None) // XXX TODO --- find frame
          }
        }
      }
      actionViewPicked.enabled = false

      val actionDragPicked = Action(null) {}
      actionDragPicked.enabled = false

      val ggDragPicked: Button = new Button(actionDragPicked) with ButtonCanDrag {
        val iconFun: Path2D => Unit = raphael.Shapes.View
        peer.putClientProperty("styleId", "icon-space")
        icon          = GUI.iconNormal  (iconFun)
        disabledIcon  = GUI.iconDisabled(iconFun)

        val sourceActions: Int = TransferHandler.COPY | TransferHandler.LINK

        protected def sourceAction(modifiers: Int): Int = TransferHandler.LINK

        protected def export(): Option[Transferable] =
          viewPicked.map { view =>
            DragAndDrop.Transferable(ObjView.Flavor) {
              new ObjView.Drag[S](universe, view)
            }
          }
      }

      val ggPick: Component = new Component {

        private var dragPt      = Option.empty[Point]
        private var dndStarted  = false

        private[this] val pointsDiscovered = mutable.Set.empty[IntPoint2D]

        preferredSize = new Dimension(320, 320)
//        minimumSize   = preferredSize
//        maximumSize   = preferredSize

        foreground    = Color.gray // if (Mellite.isDarkSkin) Color.lightGray else Color.darkGray

        override protected def paintComponent(g: Graphics2D): Unit = {
          val w1 = peer.getWidth  - 1
          val h1 = peer.getHeight - 1
          for (i <- 0 to 16) {
            for (j <- 0 to 16) {
              val x = i * w1 / 16
              val y = j * h1 / 16
              g.drawLine(0, y, w1, y)
              g.drawLine(x, 0, x, h1)
            }
          }
          g.setColor(Color.red)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          pointsDiscovered.foreach { pt =>
            import numbers.Implicits._
            val side1 = (extent << 1) - 1
            val x     = (pt.x.clip(0, w1).linLin(0, side1, 0, w1) + 0.5).toInt
            val y     = (pt.y.clip(0, h1).linLin(0, side1, 0, h1) + 0.5).toInt
            g.fillOval(x - 3, y - 3, 6, 6)
          }
        }

        private def drag(e: MouseEvent, pt: Point, mod: Int): Unit =
          dragPt.foreach { dndInit =>
            if (!dndStarted && ((math.abs(pt.x - dndInit.x) > 5) || (math.abs(pt.y - dndInit.y) > 5))) {
              dndStarted = true
              val th = ggDragPicked.peer.getTransferHandler
              th.exportAsDrag(peer, e.peer, TransferHandler.LINK)
            }
          }

        listenTo(mouse.clicks)
        listenTo(mouse.moves)
        reactions += {
          case e @ MouseMoved  (_, pt, mod) => drag(e, pt, mod)
          case e @ MouseDragged(_, pt, mod) => drag(e, pt, mod)
          case MouseReleased(_, _, _, _, _) =>
            dragPt      = None
            dndStarted  = false
          case MousePressed(_, pt, _, _, false) =>
            dragPt      = Some(pt)
            dndStarted  = false

            val (ptOpt, viewOpt) = impl.cursor.step { implicit tx =>
              val som   = somH()
              val w1    = peer.getWidth  - 1
              val h1    = peer.getHeight - 1
              val side1 = (extent << 1) - 1
              import numbers.Implicits._
              val x   = (pt.x.clip(0, w1).linLin(0, w1, 0, side1) + 0.5).toInt
              val y   = (pt.y.clip(0, h1).linLin(0, h1, 0, side1) + 0.5).toInt
              val qp  = Vector.tabulate(som.config.dimensions) { d =>
                if (d == 0) x else if (d == 1) y else 0
              }
              val ptObjOpt  = som.query(qp)
              val _ptOpt    = ptObjOpt.map(_._1)
              val _viewOpt  = ptObjOpt.map { case (_, obj) =>
                ObjListView(obj)
              }
              (_ptOpt, _viewOpt)
            }
            viewPicked    = viewOpt
            lbPicked.icon = viewOpt.map(_.icon).getOrElse(Swing.EmptyIcon)
            lbPicked.text = viewOpt.map(_.name).getOrElse(" ")
            actionViewPicked.enabled = viewOpt.exists(_.isViewable)
            actionDragPicked.enabled = viewOpt.isDefined
            ptOpt.foreach { case Seq(x, y, _ @ _*) =>
              if (pointsDiscovered.add(IntPoint2D(x, y))) repaint()
            }
        }
      }
      ggPick.peer.setTransferHandler(ggDragPicked.peer.getTransferHandler)

      val ggViewPicked  = GUI.toolButton(actionViewPicked, raphael.Shapes.View)
      // val ggDragPicked  = GUI.toolButton(actionDragPicked, raphael.Shapes.Hand)

      val actionCancel: swing.Action = new swing.Action(null) {
        def apply(): Unit = cursor.step { implicit tx =>
          renderRef.swap(None)(tx.peer).foreach(_.cancel())
        }
        enabled = false
      }

      val ggCancel    = GUI.toolButton(actionCancel, raphael.Shapes.Cross, tooltip = "Abort Rendering")
      val ggSelected  = new ToggleButton("Filter Selected")
      ggSelected.selected = true

      val ggDrop = new Label("Drop Folder to Add", raphael.Icon()(raphael.Shapes.Folder), Alignment.Leading)

      ggDrop.peer.setTransferHandler(new TransferHandler {
        override def canImport(support: TransferSupport): Boolean = {
          val res = support.isDataFlavorSupported(ObjView.Flavor) && renderRef.single.get.isEmpty
          if (res) support.setDropAction(TransferHandler.LINK)
          res
        }

        override def importData(support: TransferSupport): Boolean = {
          val drag = support.getTransferable.getTransferData(ObjView.Flavor).asInstanceOf[ObjView.Drag[_]]
          renderRef.single.get.isEmpty && drag.universe.workspace == universe.workspace && drag.view.factory.tpe == Folder && {
            val folderH = drag.view.objH.asInstanceOf[stm.Source[S#Tx, Folder[S]]]
            ggDrop.text = drag.view.name
            startRender(folderH)
            true
          }
        }
      })

      // XXX TODO --- should use custom view so we can cancel upon `dispose`
      def startRender(folderH: stm.Source[S#Tx, Folder[S]]): Unit = {
        val selected = ggSelected.selected
        val ok = cursor.step { implicit tx =>
          renderRef.get(tx.peer).isEmpty && {
            val obj = somH()

            def finished()(implicit tx: S#Tx): Unit = {
              renderRef.set(None)(tx.peer)
              deferTx {
                actionCancel.enabled = false
              }
            }

            val folder    = folderH()
            val rendering = obj.addAll(folder, selected = selected)
            /* val obs = */ rendering.reactNow { implicit tx => {
              case Rendering.Completed(Success(_)) => finished()
              case Rendering.Completed(Failure(Rendering.Cancelled())) => finished()
              case Rendering.Completed(Failure(ex)) =>
                finished()
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

      val panelPicked = new FlowPanel(ggDragPicked, ggViewPicked, lbPicked)

      val panelControl = new FlowPanel(ggDrop, ggSelected, ggProgress, ggCancel)
      component = new BorderPanel {
        add(panelPicked , BorderPanel.Position.North)
        add(ggPick      , BorderPanel.Position.Center)
        add(panelControl, BorderPanel.Position.South )
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = rendering.foreach(_.cancel())
  }
}