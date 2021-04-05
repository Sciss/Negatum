/*
 *  FeatureAnalysisViewImpl.scala
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

package de.sciss.negatum.gui.impl

import de.sciss.audiofile.AudioFile
import de.sciss.audiowidgets.{Transport => GUITransport}
import de.sciss.desktop.KeyStrokes
import de.sciss.file.File
import de.sciss.icons.raphael
import de.sciss.lucre.swing.LucreSwing.{defer, deferTx, requireEDT}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.synth.Txn
import de.sciss.lucre.{BooleanObj, Cursor, Disposable, DoubleObj, ListObj, Obj, Source}
import de.sciss.mellite.{Application, CodeFrame, DragAndDrop, GUI, ObjListView, ObjView, ViewState}
import de.sciss.negatum.Negatum
import de.sciss.negatum.gui.FeatureAnalysisView
import de.sciss.negatum.impl.{Evaluation, Weight}
import de.sciss.proc.{AudioCue, Proc, Transport, Universe}
import de.sciss.sonogram.SonogramComponent
import de.sciss.synth.SynthGraph
import de.sciss.{desktop, numbers, proc, sonogram}

import java.awt.datatransfer.Transferable
import java.awt.geom.Line2D
import java.awt.{BasicStroke, Color}
import javax.swing.table.{AbstractTableModel, TableCellRenderer}
import javax.swing.{JComponent, JTable, TransferHandler}
import scala.annotation.switch
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.swing.Table.IntervalMode
import scala.swing.{Action, BorderPanel, Component, FlowPanel, Graphics2D, ProgressBar, ScrollPane, Table}
import scala.util.Failure

object FeatureAnalysisViewImpl {
  def apply[T <: Txn[T]](negatum: Negatum[T])(implicit tx: T, universe: Universe[T]): FeatureAnalysisView[T] = {
    type I  = tx.I
    implicit val iCsr : Cursor[I] = tx.inMemoryCursor
    implicit val itx  : I         = tx.inMemory
    val ui  = Universe.dummy[I]
    val tr  = Transport[I](ui)
    val pi  = Proc[I]()
    tr.addObject(pi)
    implicit val bridge: T => I = tx.inMemoryBridge // sys.inMemoryTx
    new Impl[I, T](tx.newHandle(negatum), negatum.template.value, tr, itx.newHandle(pi)).init(negatum)
  }

  private final class Impl[I <: Txn[I], T <: Txn[T]](negatumH: Source[T, Negatum[T]], template: AudioCue,
                                                     playT: Transport[I], playH: Source[I, Proc[I]])
                                                    (implicit val universe: Universe[T], bridge: T => I)
    extends FeatureAnalysisView[T] with ComponentHolder[Component] {

    type C = Component

    override def viewState: Set[ViewState] = Set.empty  // XXX TODO

    @volatile
    private[this] var _disposedGUI  = false

    private[this] var disposables   = List.empty[Disposable[T]]

//    private[this] val synthRef      = Ref(Option.empty[Synth])
//    private[this] val synthRef      = Ref(Option.empty[Synth])

    private[this] var sonMgr: sonogram.OverviewManager = _

    private[this] var ggTable: Table = _

    def dispose()(implicit tx: T): Unit = {
//      import TxnLike.peer
      implicit val itx: I = bridge(tx)
      playT.dispose()
//      synthRef.swap(None).foreach(_.dispose())
      disposables.foreach(_.dispose())
      deferTx {
        _disposedGUI = true
        sonMgr.dispose()
      } (tx)
    }

    // XXX TODO --- could observe obj for changes in name, fitness, def
    private[this] def mkRow(obj: Obj[T], fIdx: Int)(implicit tx: T): Option[Row] = obj match {
      case p: Proc[T] =>
        val attr = p.attr
        import proc.Implicits._
        val fitness   = attr.$[DoubleObj ](Negatum.attrFitness ).map   (_.value).getOrElse(Double.NaN)
        val selected  = attr.$[BooleanObj](Negatum.attrSelected).exists(_.value)
//        attr.changed.react { implicit tx => upd =>
//          upd.changes.foreach {
//            case Obj.AttrAdded(Negatum.attrSelected)
//          }
//        }

        val r = new Row(name = p.name, graph = p.graph.value, fitness = fitness, selected = selected,
          folderIdx = fIdx)
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

    def init(negatum: Negatum[T])(implicit tx: T): this.type = {
      val f = negatum.population
      disposables ::= f.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case ListObj.Added  (fIdx, elem) =>
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

          case ListObj.Removed(fIdx, _) =>
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

    private class Row(var name: String, var graph: SynthGraph, var fitness: Double, var selected: Boolean,
                      var folderIdx: Int /* , observer: Disposable[S] */)
      /* extends Disposable[S] */ {

      var rendered  = Option.empty[Future[AudioCue]]
      var son       = Option.empty[sonogram.Overview]
      var features  = Option.empty[Weight]

      // def dispose()(implicit tx: S): Unit = observer.dispose()
    }

    private[this] object mTable extends AbstractTableModel {
      var data = Vector.empty[Row]

      def insert(idx: Int, r: Row): Unit = {
        data = data.patch(idx, r :: Nil, 0)
        fireTableRowsInserted(idx, idx)
      }

      def remove(idx: Int): Row = {
        val r = data(idx)
        data = data.patch(idx, Nil, 1)
        fireTableRowsDeleted(idx, idx)
        r
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
          case 4 => classOf[Weight]
          case 5 => classOf[java.lang.Boolean]
        }

      def getColumnCount: Int = 6

      override def getColumnName(colIdx: Int): String =
        (colIdx: @switch) match {
          case 0 => "Idx"
          case 1 => "SynthGraph"
          case 2 => "Fitness"
          case 3 => "Sonogram"
          case 4 => "Features"
          case 5 => "Sel"
        }

      def getValueAt(rowIdx: Int, colIdx: Int): AnyRef =
        (colIdx: @switch) match {
          case 0 => rowIdx                .asInstanceOf[AnyRef]
          case 1 => data(rowIdx).name
          case 2 => data(rowIdx).fitness  .asInstanceOf[AnyRef]
          case 3 => data(rowIdx).son      .orNull
          case 4 => data(rowIdx).features .orNull
          case 5 => data(rowIdx).selected .asInstanceOf[AnyRef]
        }
    }

    private object TH extends TransferHandler {
      // ---- export ----

      override def getSourceActions(c: JComponent): Int =
        TransferHandler.COPY | /* TransferHandler.MOVE | */ TransferHandler.LINK // dragging only works when MOVE is included. Why?

      override def createTransferable(c: JComponent): Transferable = {
        val sel = selectedRows
        if (sel.size != 1) return null
        cursor.step { implicit tx =>
          negatumH().population.get(sel.head.folderIdx).map { obj =>
            val view = ObjListView(obj)
            DragAndDrop.Transferable(ObjView.Flavor) {
              new ObjView.Drag[T](universe, view, Set.empty)
            }
          } .orNull
        }
      }
    }

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
        comp.value  = (fit.clip(0, 1). linLin(0, 1, 0, 90) + 0.5).toInt
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
        comp.sonogram = ovrOpt
        comp
      }
    }

    private[this] object featuresRenderer extends TableCellRenderer {
      private[this] var features  = Option.empty[Weight]
      private[this] val minMaxS   = mutable.Map.empty[Int, (Double, Double)]
      private[this] val minMaxT   = mutable.Map.empty[Int, (Double, Double)]
      private[this] val comp: Component = new Component {
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
                  val y   = j.linLin(0, num - 1, 4, w - 4)
                  val xw  = x.linLin(min, max, h - 4, 4)
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
            val yDiv  = j1.linLin(0, num - 1, 4, w - 4)
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
          case w: Weight => Some(w)
          case _ => None
        }
        features = featOpt
        comp.peer
      }
    }

//    private[this] object selectedRenderer extends TableCellRenderer {
//      private[this] val comp = new Label
//      private[this] val iconClear     = raphael.Icon(extent = 16)(raphael.Shapes.CheckboxUnselected)
//      private[this] val iconSelected  = raphael.Icon(extent = 16)(raphael.Shapes.CheckboxSelected)
//
//      def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean,
//                                        row: Int, column: Int): java.awt.Component = {
//        val selected = value == true
//        comp.icon = if (selected) iconSelected else iconClear
//        comp.peer
//      }
//    }

    private def selectedRow: Option[Row] = ggTable.selection.rows.headOption
      .map { viewRowIdx =>
        val rowIdx = ggTable.peer.convertRowIndexToModel(viewRowIdx)
        mTable.data(rowIdx)
      }

    private def selectedRows: Seq[Row] = ggTable.selection.rows
      .iterator.map { viewRowIdx =>
        val rowIdx = ggTable.peer.convertRowIndexToModel(viewRowIdx)
        mTable.data(rowIdx)
      } .toList // (breakOut)

    private def guiInit(data0: Vector[Row]): Unit = {
      mTable.data   = data0
      ggTable   = new Table {
        override lazy val peer = new JTable // bloody scala.swing kills the cell renderer

        model = mTable
        selection.intervalMode = IntervalMode.SingleInterval
//        listenTo(selection)
//        reactions += {
//          case TableRowsSelected(_, range, false) =>
//        }
        rowHeight = 64
        peer.setDragEnabled(true)
        peer.setTransferHandler(TH)
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
      val col5 = cm.getColumn(5)  // selected
      col5.setPreferredWidth(24)
      col5.setMinWidth      (24)
      col5.setMaxWidth      (24)
//      col5.setCellRenderer(selectedRenderer)
      val ggScroll = new ScrollPane(ggTable)
      ggScroll.peer.putClientProperty("styleId", "undecorated")
      ggTable.peer.setAutoCreateRowSorter(true)

      val actionToggle = Action(null) {
        val rows = selectedRows
        if (rows.nonEmpty) {
          val rowsFound = cursor.step { implicit tx =>
            val f = negatumH().population
            val tup = rows.flatMap(r => f.get(r.folderIdx).map(obj => (obj.attr, r)))
            tup.foreach { case (attr, r) =>
              val state = !r.selected
              attr.$[BooleanObj](Negatum.attrSelected) match {
                case Some(BooleanObj.Var(vr)) => vr() = state
                case _ => attr.put(Negatum.attrSelected, BooleanObj.newVar(state))
              }
            }
            tup.map(_._2)
          }
          val indices = rowsFound.map { r =>
            r.selected = !r.selected
            mTable.data.indexOf(r)
          }
          mTable.fireTableRowsUpdated(indices.min, indices.max)
        }
      }
      val ggToggleSel = GUI.toolButton(actionToggle, raphael.Shapes.CheckboxSelected, "Toggle Selection (T)")

      def actionStop(): Unit =
        cursor.step { implicit tx =>
          implicit val itx: I = bridge(tx)
          playT.stop()
        }

      def actionPlay(): Unit =
        selectedRow.foreach { r =>
          cursor.step { implicit tx =>
            implicit val itx: I = bridge(tx)
            val proc = playH()
            playT.stop()
            proc.graph() = r.graph
            playT.seek(0L)
            playT.play()
          }
        }

      def actionView(): Unit =
        selectedRow.foreach { r =>
          cursor.step { implicit tx =>
            negatumH().population.apply(r.folderIdx) match {
              case p: Proc[T] =>
                import Application.compiler
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
            AudioCue(audioF.toURI, spec, 0L, 1.0)
          }
          val fut1: Future[AudioCue] = fut.flatMap { cue =>
            // println(s"here we are [1]: $audioF - ${audioF.isFile}")
            if (_disposedGUI) Future.successful(cue) else {
              val sonJob    = sonogram.OverviewManager.Job(file = audioF)
              val overview  = sonMgr.acquire(sonJob)
              overview.onComplete {
                case Failure(ex) => println("Sonogram failed:")
                  ex.printStackTrace()
                case _ =>
              }
              overview.map { _ =>
                defer {
                  if (!_disposedGUI) {
                    val mIdx = mTable.data.indexOf(r)
                    if (mIdx >= 0) {
                      val d = mTable.data(mIdx)
                      d.son = Some(overview)
                      mTable.update(mIdx)
                    } else {
                      println("Oops, lost row")
                    }
                  }
                }
                cue
              }
            }
          }

          val fut2: Future[AudioCue] = fut1.flatMap { cue =>
            if (_disposedGUI) Future.successful(cue) else {
              val futFeat = Future(blocking(Weight(audioF)))
              futFeat.onComplete {
                case Failure(ex) => println("Features failed:")
                  ex.printStackTrace()
                case _ =>
              }
              futFeat.map { vecOpt =>
                defer {
                  if (!_disposedGUI) {
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
                }
                cue
              }
            }
          }

          r.rendered = Some(fut2)
          fut2
        }

        def loop(rem: List[Seq[Row]]): Unit = rem match {
          case head :: tail if !_disposedGUI =>
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

      val ggTransport = GUITransport.makeButtonStrip(Seq(
        GUITransport.Stop(actionStop()), GUITransport.Play(actionPlay())
      ))
      val ggStop = ggTransport.button(GUITransport.Stop).get
      val ggPlay = ggTransport.button(GUITransport.Play).get

      desktop.Util.addGlobalKey(ggToggleSel, KeyStrokes.plain + 't')
      ggStop.peer.getActionMap.put("click", Action(null) {
        val isPlaying = cursor.step { implicit tx =>
          implicit val itx: I = bridge(tx)
          playT.isPlaying
        }
        (if (isPlaying) ggStop else ggPlay).doClick()
      }.peer)
      ggStop.peer.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStrokes.plain + ' ', "click")

      val ggView        = GUI.toolButton(Action(null)(actionView()), raphael.Shapes.View)
      val ggRender      = GUI.toolButton(Action("Render")(actionRender()), raphael.Shapes.LineChart)
      val ggPrintMinMax = GUI.toolButton(Action("Min/Max")(featuresRenderer.printMinMax()), raphael.Shapes.Printer)

      val pBottom = new FlowPanel(ggTransport, ggView, ggRender, ggPrintMinMax, ggToggleSel)

      val pBorder = new BorderPanel {
        add(ggScroll, BorderPanel.Position.Center)
        add(pBottom , BorderPanel.Position.South )
      }

      val sonConfig = sonogram.OverviewManager.Config()
      sonMgr = sonogram.OverviewManager(sonConfig)

      component = pBorder
    }
  }
}
