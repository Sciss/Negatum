/*
 *  NegatumViewImpl.scala
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

import de.sciss.desktop.UndoManager
import de.sciss.icons.raphael
import de.sciss.lucre.Txn.{peer => txPeer}
import de.sciss.lucre.expr.CellView
import de.sciss.lucre.swing.LucreSwing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{BooleanCheckBoxView, DoubleSpinnerView, IntSpinnerView, TargetIcon, View}
import de.sciss.lucre.{BooleanObj, DoubleObj, IntObj, Source, synth}
import de.sciss.mellite.{DragAndDrop, GUI, ObjView, Prefs, ViewState}
import de.sciss.negatum.gui.{FeatureAnalysisFrame, NegatumView}
import de.sciss.negatum.{Negatum, Optimize, Rendering}
import de.sciss.proc.{Proc, Universe}
import de.sciss.processor.Processor
import de.sciss.swingplus.{GroupPanel, Spinner}

import javax.swing.TransferHandler.TransferSupport
import javax.swing.{JLabel, SpinnerNumberModel, TransferHandler}
import scala.concurrent.ExecutionContext
import scala.concurrent.stm.Ref
import scala.swing.{BorderPanel, BoxPanel, Button, Component, Dialog, Dimension, FlowPanel, Graphics2D, Label, Orientation, ProgressBar, Swing}
import scala.util.{Failure, Success}

object NegatumViewImpl {
  def apply[T <: synth.Txn[T]](n: Negatum[T])(implicit tx: T, universe: Universe[T]): NegatumView[T] = {
    implicit val undo: UndoManager = UndoManager()
    val res = new Impl[T](tx.newHandle(n))
    res.init(n)
  }

  private final class Impl[T <: synth.Txn[T]](negatumH: Source[T, Negatum[T]])
                                             (implicit val universe: Universe[T], val undoManager: UndoManager)
    extends NegatumView[T] with ComponentHolder[Component] {

    type C = Component

    override def viewState: Set[ViewState] = Set.empty  // XXX TODO

    private[this] val renderRef   = Ref(Option.empty[Rendering[T, Unit]])
    private[this] val optimizeRef = Ref(Option.empty[Processor[Any]])

    private def defaultOptAnaDur(n: Negatum[T])(implicit tx: T): Double = {
      val t     = n.template()
      val sr    = t.sampleRate
      math.max(2.0, t.numFrames / sr)
    }

    private final class DropProcLabel extends Label {
      override lazy val peer: JLabel = new JLabel(" ") with SuperMixin {
        // XXX TODO --- hack to avoid too narrow buttons under certain look-and-feel
        override def getPreferredSize: Dimension = {
          val d = super.getPreferredSize
          if (!isPreferredSizeSet) {
            val e     = math.max(24, math.max(d.width, d.height))
            d.width   = e
            d.height  = e
          }
          d
        }
      }

      override protected def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        val p       = peer
        val w       = p.getWidth
        val h       = p.getHeight
        val extent  = math.min(w, h)
        if (extent > 0) {
          TargetIcon.paint(g, x = (w - extent) >> 1, y = (h - extent) >> 1, extent = extent, enabled = enabled)
        }
      }

      private object TH extends TransferHandler {
        override def canImport (support: TransferSupport): Boolean = enabled && {
          val t = support.getTransferable
          t.isDataFlavorSupported(ObjView.Flavor)
        }

        override def importData(support: TransferSupport): Boolean = enabled && {
          val t = support.getTransferable
          val td = DragAndDrop.getTransferData(t, ObjView.Flavor)
          td.universe.workspace == universe.workspace && (td.view.factory.tpe == Proc && {
            universe.cursor.step { implicit tx =>
              optimizeRef().isEmpty && (td.view.asInstanceOf[ObjView[T]].obj match {
                case p: Proc[T] =>
                  import Negatum._
                  val obj   = negatumH()
                  val attr  = obj.attr
                  val gIn   = p.graph().value
                  val t     = obj.template()
                  val sr    = t.sampleRate
                  val bs    = Prefs.audioBlockSize.getOrElse(Prefs.defaultAudioBlockSize)
                  val dur   = attr.$[DoubleObj ](attrOptDuration      ).map(_.value).getOrElse(defaultOptAnaDur(obj))
                  val expPr = attr.$[BooleanObj](attrOptExpandProtect ).forall(_.value)
                  val expIO = attr.$[BooleanObj](attrOptExpandIO      ).forall(_.value)

                  val cfg = Optimize.Config(
                    graph         = gIn,
                    sampleRate    = sr,
                    blockSize     = bs,
                    analysisDur   = dur,
                    expandProtect = expPr,
                    expandIO      = expIO
                  )
                  val o = Optimize(cfg)
                  optimizeRef() = Some(o)
                  val procH = tx.newHandle(p: Proc[T])
                  import ExecutionContext.Implicits.global
                  o.onComplete {
                    case Success(res) =>
                      println(s"Optimization found ${res.numConst} constant replacement and ${res.numEqual} redundant elements.")
                      universe.cursor.step { implicit tx =>
                        optimizeRef() = None
                        val p1        = procH()
                        p1.graph()    = res.graph
                        p1.attr.remove(Proc.attrSource)  // make sure it's regenerated if the editor is opened
                      }

                    case Failure(ex) =>
                      println("Optimize failed")
                      ex.printStackTrace()
                      universe.cursor.step { implicit tx =>
                        optimizeRef() = None
                      }
                  }
                  tx.afterCommit(o.start())
                  true

                case _ =>
                  false
              })
            }
          })
        }
      }

      peer.setTransferHandler(TH)
      tooltip = "Drop Proc to optimize"
    }

    def init(n: Negatum[T])(implicit tx: T): this.type = {
      val attr  = n.attr
      implicit val intEx    : IntObj    .type = IntObj
      implicit val doubleEx : DoubleObj .type = DoubleObj
//      implicit val booleanEx: BooleanObj.type = BooleanObj

      class Field(name: String, view: View[T]) {
        lazy val label : Label      = new Label(s"$name:")
        def      editor: Component  = view.component
      }

      def mkIntField(name: String, key: String, default: Int): Field = {
        val view = IntSpinnerView.optional[T](CellView.attr[T, Int, IntObj](attr, key),
          name = name, default = Some(default))
        new Field(name, view)
      }

      def mkDoubleField(name: String, key: String, default: Double): Field = {
        val view = DoubleSpinnerView.optional[T](CellView.attr[T, Double, DoubleObj](attr, key),
          name = name, default = Some(default))
        new Field(name, view)
      }

      def mkBooleanField(name: String, key: String, default: Boolean): Field = {
        val view = BooleanCheckBoxView.optional[T](CellView.attr[T, Boolean, BooleanObj](attr, key),
          name = name, default = default)
        new Field(name, view) {
          override lazy val editor: Component = {
            val res = view.component
            res.text = null
            res
          }
        }
      }

      // def mkEmptyField(): Field = new Field("", View.wrap[S](Swing.HGlue))

      import Negatum.Config.default._
      import Negatum._

      val fSeed = {
        val name = "Seed"
        val view = IntSpinnerView.optional[T](CellView.attr[T, Int, IntObj](attr, attrSeed),
          name = name, default = None)
        new Field(name, view)
      }

      val fGenPopulation      = mkIntField    ("Population"             , attrGenPopulation , gen.population)
      val fGenConstProb       = mkDoubleField ("Prob. of Constants"     , attrGenProbConst  , gen.probConst)
      val fGenMinVertices     = mkIntField    ("Min. # of Vertices"     , attrGenMinVertices, gen.minVertices)
      val fGenMaxVertices     = mkIntField    ("Max. # of Vertices"     , attrGenMaxVertices, gen.maxVertices)
      val fGenDefaultProb     = mkDoubleField ("Prob. of Default Values", attrGenProbDefault, gen.probDefault)
      // attrGenAllowedUGens -- TODO
      val gridGen = Seq(fGenPopulation, fGenConstProb, fGenMinVertices, fGenDefaultProb, fGenMaxVertices, fSeed)

      val fEvalMinFreq        = mkIntField    ("Min Freq [Hz]"          , attrEvalMinFreq   , eval.minFreq)
      val fEvalMaxFreq        = mkIntField    ("Max Freq [Hz]"          , attrEvalMaxFreq   , eval.maxFreq)
      val fEvalNumMFCC        = mkIntField    ("# of MFCC"              , attrEvalNumMFCC   , eval.numMFCC)
      val fEvalNumMel         = mkIntField    ("# of Mel Bands"         , attrEvalNumMel    , eval.numMel )
      val fEvalMaxBoost       = mkDoubleField ("Max. Boost"             , attrEvalMaxBoost  , eval.maxBoost)
      val fEvalTempWeight     = mkDoubleField ("Temporal Weight"        , attrEvalTimeWeight, eval.timeWeight)
      val gridEval = Seq(fEvalMinFreq, fEvalNumMFCC, fEvalMaxFreq, fEvalNumMel, fEvalMaxBoost, fEvalTempWeight)

      val fBreedSelFraction   = mkDoubleField ("Selection Fraction"     , attrBreedSelectFraction, breed.selectFraction)
      val fBreedElitism       = mkIntField    ("Elitism"                , attrBreedElitism  , breed.elitism)
      val fBreedMinMut        = mkIntField    ("Min. # of Mutations"    , attrBreedMinMut   , breed.minMut)
      val fBreedMaxMut        = mkIntField    ("Max. # of Mutations"    , attrBreedMaxMut   , breed.maxMut)
      val fBreedProbMut       = mkDoubleField ("Prob. of Mutation"      , attrBreedProbMut  , breed.probMut)
      val fBreedGolem         = mkIntField    ("# of Golems"            , attrBreedGolem    , breed.golem)
      val gridBreed = Seq(fBreedSelFraction, fBreedProbMut, fBreedElitism, fBreedMinMut, fBreedGolem, fBreedMaxMut)

      val fOptDuration        = mkDoubleField ("Analysis Duration [s]"  , attrOptDuration     , defaultOptAnaDur(n))
      val fOptExpandProtect   = mkBooleanField("Expand Protect Elements", attrOptExpandProtect, true)
      val fOptExpandIO        = mkBooleanField("Expand I/O Elements"    , attrOptExpandIO     , true)
      val gridOpt = Seq(fOptDuration, fOptExpandProtect, fOptExpandIO)

      deferTx {
        def mkGrid(title: String, grid: Seq[Field], columns: Int = 2): Component = {
          val gridSz  = grid.size
          val gridC0  = grid.flatMap(v => v.label :: v.editor :: Nil)
          val gridC   = if (gridSz % columns == 0) gridC0 else gridC0 ++ (Swing.HGlue :: Swing.HGlue :: Nil)
          val gridV   = gridC.grouped(columns * 2).toSeq
          val gridH   = gridV.transpose
          new GroupPanel {
            horizontal = Seq(gridH.map(col => Par          (col.map(c => GroupPanel.Element(c)): _*)): _*)
            vertical   = Seq(gridV.map(row => Par(Baseline)(row.map(c => GroupPanel.Element(c)): _*)): _*)
            border     = {
              val empty = Swing.EmptyBorder(4)
              if (title.isEmpty) empty else Swing.TitledBorder(empty, s"<HTML><BODY><B>$title</B></BODY>")
            }
          }
        }
        val panelGen    = mkGrid("Generation", gridGen)
        val panelEval   = mkGrid("Evaluation", gridEval)
        val panelBreed  = mkGrid("Breeding"  , gridBreed)
        val panelParams = new BoxPanel(Orientation.Vertical) {
          // horizontalAlignment
          contents += panelGen
          contents += panelEval
          contents += panelBreed
          border = Swing.EmptyBorder(4, 0, 4, 0)
        }

        val panelOpt = mkGrid("", gridOpt, columns = 1)

        guiInit(panelParams, panelOpt)
      }
      this
    }

    def negatum  (implicit tx: T): Negatum[T]                  = negatumH()
    def rendering(implicit tx: T): Option[Rendering[T, Unit]]  = renderRef()

    private def guiInit(panelParams: Component, panelOpt: Component): Unit = {

      val ggProgress: ProgressBar = new ProgressBar
      ggProgress.max = 160

      val actionCancel: swing.Action = new swing.Action(null) {
        def apply(): Unit = cursor.step { implicit tx =>
          renderRef.swap(None).foreach(_.cancel())
        }
        enabled = false
      }

      val actionStop: swing.Action = new swing.Action(null) {
        def apply(): Unit = cursor.step { implicit tx =>
          renderRef.swap(None).foreach(_.stop())
        }
        enabled = false
      }

      val ggCancel  = GUI.toolButton(actionCancel, raphael.Shapes.Cross        , tooltip = "Abort Rendering")
      val ggStop    = GUI.toolButton(actionStop  , raphael.Shapes.TransportStop, tooltip = "Stop Rendering and Update Table")

      val mNumIterations  = new SpinnerNumberModel(1, 1, 65536, 1)
      val ggNumIterations = new Spinner(mNumIterations)

      val actionRender = new swing.Action("Evolve") { self =>
        def apply(): Unit = {
          val numIterations = mNumIterations.getNumber.intValue()
          val ok = cursor.step { implicit tx =>
            renderRef().isEmpty && {
              val obj   = negatumH()
              val attr  = obj.attr
              import Negatum.Config.default._
              import Negatum._
              val seed      = attr.$[IntObj](attrSeed).map(_.value.toLong).getOrElse(System.currentTimeMillis())

              val cGen      = Negatum.Generation(
                population  = attr.$[IntObj    ](attrGenPopulation  ).map(_.value).getOrElse(gen.population),
                probConst   = attr.$[DoubleObj ](attrGenProbConst   ).map(_.value).getOrElse(gen.probConst),
                minVertices = attr.$[IntObj    ](attrGenMinVertices ).map(_.value).getOrElse(gen.minVertices),
                maxVertices = attr.$[IntObj    ](attrGenMaxVertices ).map(_.value).getOrElse(gen.maxVertices),
                probDefault = attr.$[DoubleObj ](attrGenProbDefault ).map(_.value).getOrElse(gen.probDefault)
                // allowedUGens
              )
              val cBreed    = Negatum.Breeding(
                selectFraction  = attr.$[DoubleObj ](attrBreedSelectFraction).map(_.value).getOrElse(breed.selectFraction),
                elitism     = attr.$[IntObj    ](attrBreedElitism   ).map(_.value).getOrElse(breed.elitism),
                minMut      = attr.$[IntObj    ](attrBreedMinMut    ).map(_.value).getOrElse(breed.minMut),
                maxMut      = attr.$[IntObj    ](attrBreedMaxMut    ).map(_.value).getOrElse(breed.maxMut),
                probMut     = attr.$[DoubleObj ](attrBreedProbMut   ).map(_.value).getOrElse(breed.probMut),
                golem       = attr.$[IntObj    ](attrBreedGolem     ).map(_.value).getOrElse(breed.golem)
              )
              val cEval     = Negatum.Evaluation(
                minFreq     = attr.$[IntObj    ](attrEvalMinFreq    ).map(_.value).getOrElse(eval.minFreq ),
                maxFreq     = attr.$[IntObj    ](attrEvalMaxFreq    ).map(_.value).getOrElse(eval.maxFreq ),
                numMFCC     = attr.$[IntObj    ](attrEvalNumMFCC    ).map(_.value).getOrElse(eval.numMFCC ),
                numMel      = attr.$[IntObj    ](attrEvalNumMel     ).map(_.value).getOrElse(eval.numMel  ),
                maxBoost    = attr.$[DoubleObj ](attrEvalMaxBoost   ).map(_.value).getOrElse(eval.maxBoost),
                timeWeight  = attr.$[DoubleObj ](attrEvalTimeWeight ).map(_.value).getOrElse(eval.timeWeight)
              )
              val cPenalty  = Negatum.Penalty()
              val config    = Negatum.Config(seed = seed, generation = cGen, breeding = cBreed, evaluation = cEval,
                penalty = cPenalty)

              def finished()(implicit tx: T): Unit = {
                renderRef() = None
                deferTx {
                  actionCancel.enabled  = false
                  actionStop  .enabled  = false
                  self.enabled          = true
                }
              }

              val rendering = obj.run(config, iterations = numIterations)
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
              renderRef() = Some(rendering)
              true
            }
          }
          if (ok) {
            actionCancel.enabled = true
            actionStop  .enabled = true
            self        .enabled = false
          }
        }
      }
      val ggRender  = GUI.toolButton(actionRender, raphael.Shapes.Biohazard)

      val actionAnalyze = new swing.Action("Analyze...") {
        self =>
        def apply(): Unit = cursor.step { implicit tx =>
          FeatureAnalysisFrame(negatum)
        }
      }
      val ggAnalyze = GUI.toolButton(actionAnalyze, raphael.Shapes.View)

      //            val ggDebug = Button("Debug") {
      //              renderRef.single.get.foreach { r =>
      //                val ctrl = r.control
      //                println(ctrl.stats)
      //                ctrl.debugDotGraph()
      //              }
      //            }

      val ggDropProc = new DropProcLabel
      val ggOptimize = Button("Optimize…") {
        Dialog.showMessage(
          parent      = null,
          title       = "Optimize Settings",
          messageType = Dialog.Message.Plain,
          message     = panelOpt.peer
        )
      }
      ggOptimize.tooltip = "Click to configure"
      ggOptimize.peer.putClientProperty("styleId", "icon-hover")

      val panelControl = new FlowPanel(new Label("Iterations:"),
        ggNumIterations, ggProgress, ggCancel, ggStop, ggRender, ggAnalyze, ggOptimize, ggDropProc)
      component = new BorderPanel {
        add(panelParams , BorderPanel.Position.Center)
        add(panelControl, BorderPanel.Position.South )
      }
    }

    def dispose()(implicit tx: T): Unit = {
      renderRef   .swap(None).foreach(_.cancel ())
      optimizeRef .swap(None).foreach(_.abort  ())
    }
  }
}