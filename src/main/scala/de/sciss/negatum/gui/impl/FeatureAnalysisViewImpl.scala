/*
 *  FeatureAnalysisViewImpl.scala
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
package impl

import java.awt.geom.Line2D
import java.awt.{BasicStroke, Color}
import javax.swing.JTable
import javax.swing.table.{AbstractTableModel, TableCellRenderer}

import de.sciss.audiowidgets.Transport
import de.sciss.file.File
import de.sciss.icons.raphael
import de.sciss.lucre.expr.DoubleObj
import de.sciss.lucre.stm.{Disposable, Obj, TxnLike}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{defer, deferTx, requireEDT}
import de.sciss.lucre.synth.{Synth, Sys, Txn}
import de.sciss.lucre.{expr, stm}
import de.sciss.mellite.Mellite
import de.sciss.mellite.gui.{CodeFrame, GUI}
import de.sciss.negatum.impl.{Evaluation, SOMEval}
import de.sciss.sonogram.SonogramComponent
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{AudioCue, Proc, Workspace}
import de.sciss.synth.{SynthGraph, proc}
import de.sciss.{numbers, sonogram}

import scala.annotation.switch
import scala.collection.{breakOut, mutable}
import scala.concurrent.stm.{Ref, atomic}
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.swing.Table.IntervalMode
import scala.swing.{Action, BorderPanel, Component, FlowPanel, Graphics2D, ProgressBar, ScrollPane, Table}

object FeatureAnalysisViewImpl {
  def apply[S <: Sys[S]](negatum: Negatum[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                         workspace: Workspace[S]): FeatureAnalysisView[S] = {
    new Impl[S](tx.newHandle(negatum), negatum.template.value).init(negatum)
  }

  private final class Impl[S <: Sys[S]](negatumH: stm.Source[S#Tx, Negatum[S]],
                                        template: AudioCue)(implicit val cursor: stm.Cursor[S],
                                        val workspace: Workspace[S])
    extends FeatureAnalysisView[S] with ComponentHolder[Component] {

    def dispose()(implicit tx: S#Tx): Unit = {
      import TxnLike.peer
      synthRef.swap(None).foreach(_.dispose())
      disposables.foreach(_.dispose())
      deferTx {
        sonMgr.dispose()
      }
    }

    private[this] var disposables = List.empty[Disposable[S#Tx]]

    // XXX TODO --- could observe obj for changes in name, fitness, def
    private[this] def mkRow(obj: Obj[S], fIdx: Int)(implicit tx: S#Tx): Option[Row] = obj match {
      case p: Proc[S] =>
        val attr = p.attr
        import proc.Implicits._
        val fitness = attr.$[DoubleObj](Negatum.attrFitness).map(_.value).getOrElse(Double.NaN)
        val r = new Row(name = p.name, graph = p.graph.value, fitness = fitness, folderIdx = fIdx)
        Some(r)
      case _ => None
    }

    /* Binary search on an indexed collection.
     *
     * @return  if positive: the position of the sought element in coll (i.e. the element is
     *          contained in coll). if negative: `(-ins -1)` where ins is the
     *          position at which the element should be inserted into the collection.
     */
    private[this] def binarySearch[A](coll: IndexedSeq[A])(compare: A => Int): Int = {
      var index = 0
      var low = 0
      var high = coll.size - 1
      while ({
        index  = (high + low) >> 1
        low   <= high
      }) {
        val cmp = compare(coll(index))
        if (cmp == 0) return index
        if (cmp < 0) {
          low = index + 1
        } else {
          high = index - 1
        }
      }
      -low - 1
    }

//    private[this] object auralClient extends AuralSystem.Client {
//      def auralStarted(s: Server)(implicit tx: Txn): Unit =   ...
//
//      def auralStopped()(implicit tx: Txn): Unit = ...
//    }

    def init(negatum: Negatum[S])(implicit tx: S#Tx): this.type = {
      val f = negatum.population
      disposables ::= f.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case expr.List.Added  (fIdx, elem) =>
            val rowOpt = mkRow(elem, fIdx)
            deferTx {
              val d     = mTable.data
              val mIdx  = binarySearch(d)(_.folderIdx.compareTo(fIdx))
              assert(mIdx < 0)
              val ins = -(mIdx + 1)
              d.drop(ins).foreach { succ =>
                succ.folderIdx += 1
              }
              rowOpt.foreach(mTable.insert(ins, _))
            }

          case expr.List.Removed(fIdx, elem) =>
            deferTx {
              val d     = mTable.data
              val mIdx  = binarySearch(d)(_.folderIdx.compareTo(fIdx))
              val ins   = if (mIdx >= 0) mIdx + 1 else -(mIdx + 1)
              d.drop(ins).foreach { succ =>
                succ.folderIdx -= 1
              }
              if (mIdx >= 0) mTable.remove(mIdx)
            }
        }
      }
      val data0 = f.iterator.zipWithIndex.flatMap {
        case (obj, fIdx) => mkRow(obj, fIdx)
      } .toVector

      deferTx(guiInit(data0))
      // Mellite.auralSystem.addClient()

      this
    }

    private class Row(var name: String, var graph: SynthGraph, var fitness: Double, var folderIdx: Int) {
      var rendered  = Option.empty[Future[AudioCue]]
      var son       = Option.empty[sonogram.Overview]
      var features  = Option.empty[SOMEval.Weight]
    }

    private[this] object mTable extends AbstractTableModel {
      var data = Vector.empty[Row]

      def insert(idx: Int, r: Row): Unit = {
        data = data.patch(idx, r :: Nil, 0)
        fireTableRowsInserted(idx, idx)
      }

      def remove(idx: Int): Unit = {
        data = data.patch(idx, Nil, 1)
        fireTableRowsDeleted(idx, idx)
      }

      def update(idx: Int): Unit = {
        fireTableRowsUpdated(idx, idx)
      }

      def getRowCount: Int = data.size

      override def getColumnClass(colIdx: Int): Class[_] =
        (colIdx: @switch) match {
          case 0 => classOf[java.lang.Integer]
          case 1 => classOf[String]
          case 2 => classOf[java.lang.Double]
          case 3 => classOf[sonogram.Overview]
          case 4 => classOf[SOMEval.Weight]
        }

      def getColumnCount: Int = 5

      override def getColumnName(colIdx: Int): String =
        (colIdx: @switch) match {
          case 0 => "Idx"
          case 1 => "SynthGraph"
          case 2 => "Fitness"
          case 3 => "Sonogram"
          case 4 => "Features"
        }

      def getValueAt(rowIdx: Int, colIdx: Int): AnyRef =
        (colIdx: @switch) match {
          case 0 => rowIdx.asInstanceOf[AnyRef]
          case 1 => data(rowIdx).name
          case 2 => data(rowIdx).fitness.asInstanceOf[AnyRef]
          case 3 => data(rowIdx).son      .orNull
          case 4 => data(rowIdx).features .orNull
        }
    }

    private[this] val synthRef = Ref(Option.empty[Synth])

    val sonConfig   = sonogram.OverviewManager.Config()
    private[this] var sonMgr: sonogram.OverviewManager = _

    private[this] object fitnessRenderer extends TableCellRenderer {
      private[this] val comp = new ProgressBar
      comp.max = 90
      comp.labelPainted = true

      def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean,
                                        row: Int, column: Int): java.awt.Component = {
        val fit0 = value match {
          case d: Double => d
          case _ => Double.NaN
        }
        import numbers.Implicits._
        val fit = if (fit0.isNaN) 0.0 else fit0
        comp.value  = (fit.clip(0, 1). linlin(0, 1, 0, 90) + 0.5).toInt
        comp.label  = f"$fit0%g"
        comp.peer
      }
    }

    private[this] object overviewRenderer extends TableCellRenderer {
      private[this] val comp = new SonogramComponent

      def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean,
                                        row: Int, column: Int): java.awt.Component = {
        val ovrOpt = value match {
          case ovr: sonogram.Overview => Some(ovr)
          case _ => None
        }
        comp.sono = ovrOpt
        comp
      }
    }

    private[this] object featuresRenderer extends TableCellRenderer {
      private[this] var features  = Option.empty[SOMEval.Weight]
      private[this] val minMaxS   = mutable.Map.empty[Int, (Double, Double)]
      private[this] val minMaxT   = mutable.Map.empty[Int, (Double, Double)]
      private[this] val comp      = new Component {
        private[this] val ln        = new Line2D.Double
        private[this] val strkFeat  = new BasicStroke(3f)
        private[this] val strkDiv   = new BasicStroke(1f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10.0f,
          Array[Float](2f, 2f), 0.0f)

        override protected def paintComponent(g: Graphics2D): Unit = {
          super.paintComponent(g)
          features.foreach { vec =>
            val num = vec.spectral.length + vec.temporal.length
            val w   = peer.getWidth
            val h   = peer.getHeight
            // val sy  = h.toDouble / (num + 1)
            val strkOrig = g.getStroke
            g.setColor(Color.lightGray)
            g.fillRect(0, 0, w, h)
            g.setColor(Color.black)

            import numbers.Implicits._

            def drawFeatures(d: Array[Double], j0: Int, m: mutable.Map[Int, (Double, Double)]): Int = {
              g.setStroke(strkFeat)
              var i = 0
              var j = j0
              while (i < d.length) {
                val x = d(i)
                var (min, max) = m.getOrElseUpdate(i, (x, x))
                if (x < min || x > max) {
                  min = math.min(x, min)
                  max = math.max(x, max)
                  m.update(i, (min, max))
                }
                if (max > min) {
                  val y   = j.linlin(0, num - 1, 4, w - 4)
                  val xw  = x.linlin(min, max, h - 4, 4)
//                  ln.setLine(4, y, xw, y)
                  ln.setLine(y, h - 4, y, xw)
                  g.draw(ln)
                }
                i += 1
                j += 1
              }
              j
            }

            val j1    = drawFeatures(vec.spectral, 0, minMaxS)
            val yDiv  = j1.linlin(0, num - 1, 4, w - 4)
//            ln.setLine(4, yDiv, w - 8, yDiv)
            ln.setLine(yDiv, h - 4, yDiv, 4)
            g.setStroke(strkDiv)
            g.draw(ln)
            drawFeatures(vec.temporal, j1 + 1, minMaxT)

            g.setStroke(strkOrig)
          }
        }
      }

      def printMinMax(): Unit = {
        println("---- Spectral ----")
        printMinMax(minMaxS)
        println("\n---- Temporal ----")
        printMinMax(minMaxT)
      }

      private def printMinMax(m: mutable.Map[Int, (Double, Double)]): Unit = if (m.nonEmpty) {
        val maxIdx = m.keysIterator.max
        for (i <- 0 to maxIdx) {
          val (min, max) = m.getOrElse(i, (Double.NaN, Double.NaN))
          println(f"$i%03d: $min%g to $max%g")
        }
      }

      def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean,
                                        row: Int, column: Int): java.awt.Component = {
        val featOpt = value match {
          case w: SOMEval.Weight => Some(w)
          case _ => None
        }
        features = featOpt
        comp.peer
      }
    }

    private def guiInit(data0: Vector[Row]): Unit = {
      mTable.data   = data0
      val ggTable   = new Table {
        override lazy val peer = new JTable // bloody scala.swing kills the cell renderer

        model = mTable
        selection.intervalMode = IntervalMode.SingleInterval
//        listenTo(selection)
//        reactions += {
//          case TableRowsSelected(_, range, false) =>
//        }
        rowHeight = 64
      }
//      ggTable.peer.setDefaultRenderer(classOf[sonogram.Overview], overviewRenderer)
      val cm = ggTable.peer.getColumnModel
      val col0 = cm.getColumn(0)  // idx
      col0.setPreferredWidth(40)
      col0.setMinWidth      (40)
      col0.setMaxWidth      (40)
      val col1 = cm.getColumn(1)  // name
      col1.setPreferredWidth(138)
      col1.setMinWidth      (138)
      col1.setMaxWidth      (132)
      val col2 = cm.getColumn(2)
      col2.setPreferredWidth(96)  // fitness
      col2.setMinWidth      (96)
      col2.setMaxWidth      (96)
      col2.setCellRenderer(fitnessRenderer)
      val col3 = cm.getColumn(3)  // overview
      col3.setPreferredWidth(256)
      col3.setCellRenderer(overviewRenderer)
      val col4 = cm.getColumn(4)  // features
      col4.setPreferredWidth(160)
      col4.setCellRenderer(featuresRenderer)
      val ggScroll  = new ScrollPane(ggTable)
      ggScroll.peer.putClientProperty("styleId", "undecorated")
      ggTable.peer.setAutoCreateRowSorter(true)

      def selectedRow: Option[Row] = ggTable.selection.rows.headOption
        .map { viewRowIdx =>
          val rowIdx = ggTable.peer.convertRowIndexToModel(viewRowIdx)
          mTable.data(rowIdx)
        }

      def selectedRows: Seq[Row] = ggTable.selection.rows
        .map { viewRowIdx =>
          val rowIdx = ggTable.peer.convertRowIndexToModel(viewRowIdx)
          mTable.data(rowIdx)
        } (breakOut)

      def actionStop(): Unit =
        atomic { implicit itx =>
          implicit val tx = Txn.wrap(itx)
          synthRef.swap(None).foreach(_.dispose())
        }

      def actionPlay(): Unit =
        selectedRow.foreach { r =>
          atomic { implicit itx =>
            implicit val tx = Txn.wrap(itx)
            Mellite.auralSystem.serverOption.foreach { s =>
              synthRef.swap(None).foreach(_.dispose())
              val syn = Synth.play(r.graph, nameHint = Some(r.name))(s.defaultGroup)
              synthRef() = Some(syn)
            }
          }
        }

      def actionView(): Unit =
        selectedRow.foreach { r =>
          cursor.step { implicit tx =>
            negatumH().population.apply(r.folderIdx) match {
              case p: Proc[S] =>
                import de.sciss.mellite.Mellite.compiler
                CodeFrame.proc(p)
              case _ =>
                println(s"Error. No Proc in folder at index ${r.folderIdx}")
            }
          }
        }


// THIS SHIT DOESN'T WORK -- JUST HANGS
//      implicit val exec: ExecutionContext =
//        ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
//
//      implicit val exec: ExecutionContext =
//        ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(4))

      implicit val exec: ExecutionContext = ExecutionContext.global

      def actionRender(): Unit = {
        def mkRowFut(r: Row): Future[Any] = {
          requireEDT()

          val audioF = File.createTemp(suffix = ".aif")
          val proc = Evaluation.bounce(graph = r.graph, audioF = audioF, inputSpec = template.spec)
          val fut = proc.map { _ =>
            // println(s"here we are [0]: $audioF - ${audioF.isFile}")
            val spec = AudioFile.readSpec(audioF)
            AudioCue(audioF, spec, 0L, 1.0)
          }
          val fut1: Future[AudioCue] = fut.flatMap { cue =>
            // println(s"here we are [1]: $audioF - ${audioF.isFile}")
            val sonJob    = sonogram.OverviewManager.Job(file = audioF)
            val overview  = sonMgr.acquire(sonJob)
            overview.onFailure {
              case ex => println("Sonogram failed:")
                ex.printStackTrace()
            }
            overview.map { _ =>
              defer {
                val mIdx = mTable.data.indexOf(r)
                if (mIdx >= 0) {
                  val d = mTable.data(mIdx)
                  d.son = Some(overview)
                  mTable.update(mIdx)
                } else {
                  println("Oops, lost row")
                }
              }
              cue
            }
          }

          val fut2: Future[AudioCue] = fut1.flatMap { cue =>
            val futFeat = Future(blocking(SOMEval(audioF)))
            futFeat.onFailure {
              case ex => println("Features failed:")
                ex.printStackTrace()
            }
            futFeat.map { vecOpt =>
              defer {
                if (vecOpt.isEmpty) {
                  println("Error: could not calculate features!")
                } else {
                  val mIdx = mTable.data.indexOf(r)
                  if (mIdx >= 0) {
                    val d = mTable.data(mIdx)
                    d.features = vecOpt
                    mTable.update(mIdx)
                  }
                }
              }
              cue
            }
          }

          r.rendered = Some(fut2)
          fut2
        }

        def loop(rem: List[Seq[Row]]): Unit = rem match {
          case head :: tail =>
            val clumpFut = head.map(mkRowFut)
            Future.sequence(clumpFut).onComplete {
              _ => defer(loop(tail))
            }
          case _ =>
        }

        val rows = selectedRows.filter(r => r.rendered.isEmpty || r.rendered.exists(_.value.exists(_.isFailure)))
        val clumps = rows.grouped(4).toList
        loop(clumps)
      }

      val ggTransport = Transport.makeButtonStrip(Seq(
        Transport.Stop(actionStop()), Transport.Play(actionPlay())
      ))

      val ggView        = GUI.toolButton(Action(null)(actionView()), raphael.Shapes.View)
      val ggRender      = GUI.toolButton(Action("Render")(actionRender()), raphael.Shapes.LineChart)
      val ggPrintMinMax = GUI.toolButton(Action("Min/Max")(featuresRenderer.printMinMax()), raphael.Shapes.Printer)

      val pBottom = new FlowPanel(ggTransport, ggView, ggRender, ggPrintMinMax)

      val pBorder = new BorderPanel {
        add(ggScroll, BorderPanel.Position.Center)
        add(pBottom , BorderPanel.Position.South )
      }

      sonMgr = sonogram.OverviewManager(sonConfig)

      component = pBorder
    }
  }
}