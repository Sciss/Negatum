/*
 *  NegatumViewImpl.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum.gui.impl

import de.sciss.desktop.UndoManager
import de.sciss.icons.raphael
import de.sciss.lucre.expr.{CellView, DoubleObj, IntObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.{peer => txPeer}
import de.sciss.lucre.swing.LucreSwing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{DoubleSpinnerView, IntSpinnerView, TargetIcon, View}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.proc.ProcObjView
import de.sciss.mellite.gui.{DragAndDrop, GUI, ObjView}
import de.sciss.negatum.gui.{FeatureAnalysisFrame, NegatumView}
import de.sciss.negatum.{Negatum, Optimize, Rendering}
import de.sciss.processor.Processor
import de.sciss.swingplus.{GroupPanel, Spinner}
import de.sciss.synth.proc.{Proc, Universe}
import javax.swing.TransferHandler.TransferSupport
import javax.swing.{JLabel, SpinnerNumberModel, TransferHandler}

import scala.concurrent.ExecutionContext
import scala.concurrent.stm.Ref
import scala.swing.{BorderPanel, BoxPanel, Component, Dimension, FlowPanel, Graphics2D, Label, Orientation, ProgressBar, Swing}
import scala.util.{Failure, Success}

object NegatumViewImpl {
  def apply[S <: Sys[S]](n: Negatum[S])(implicit tx: S#Tx, universe: Universe[S]): NegatumView[S] = {
    implicit val undo: UndoManager = UndoManager()
    val res = new Impl[S](tx.newHandle(n))
    res.init(n)
  }

  private final class Impl[S <: Sys[S]](negatumH: stm.Source[S#Tx, Negatum[S]])
                                       (implicit val universe: Universe[S], val undoManager: UndoManager)
    extends NegatumView[S] with ComponentHolder[Component] {

    type C = Component

    private[this] val renderRef   = Ref(Option.empty[Rendering[S, Unit]])
    private[this] val optimizeRef = Ref(Option.empty[Processor[Any]])

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
          td.universe.workspace == universe.workspace && (td.view match {
            case p: ProcObjView[S] =>
              val procH = p.objH
              universe.cursor.step { implicit tx =>
                optimizeRef().isEmpty && {
                  val gIn = procH().graph().value
                  val t   = negatum.template()
                  val sr  = t.sampleRate
                  val dur = math.max(2.0, t.numFrames / sr)
                  val cfg = Optimize.Config(
                    graph = gIn, sampleRate = sr, analysisDur = dur, expandProtect = true
                  )
                  val o = Optimize(cfg)
                  optimizeRef() = Some(o)
                  import ExecutionContext.Implicits.global
                  o.onComplete {
                    case Success(res) =>
                      println(s"Optimization found ${res.numConst} constant replacement and ${res.numEqual} redundant elements.")
                      universe.cursor.step { implicit tx =>
                        optimizeRef() = None
                        val p         = procH()
                        p.graph()     = res.graph
                        p.attr.remove(Proc.attrSource)  // make sure it's regenerated if the editor is opened
                      }

                    case Failure(ex) =>
                      println("Optimize failed")
                      ex.printStackTrace()
                      universe.cursor.step { implicit tx =>
                        optimizeRef() = None
                      }
                  }
                  o.start()
                  true

                }
              }

            case _ => false
          })
        }
      }

      peer.setTransferHandler(TH)
      tooltip = "Drop Proc to optimize"
    }

    def init(n: Negatum[S])(implicit tx: S#Tx): this.type = {
      val attr  = n.attr
      implicit val intEx    : IntObj    .type = IntObj
      implicit val doubleEx : DoubleObj .type = DoubleObj
//      implicit val booleanEx: BooleanObj.type = BooleanObj

      class Field(name: String, view: View[S]) {
        lazy val label : Label      = new Label(s"$name:")
        def      editor: Component  = view.component
      }

      def mkIntField(name: String, key: String, default: Int): Field = {
        val view = IntSpinnerView.optional[S](CellView.attr[S, Int, IntObj](attr, key),
          name = name, default = Some(default))
        new Field(name, view)
      }

      def mkDoubleField(name: String, key: String, default: Double): Field = {
        val view = DoubleSpinnerView.optional[S](CellView.attr[S, Double, DoubleObj](attr, key),
          name = name, default = Some(default))
        new Field(name, view)
      }

//      def mkBooleanField(name: String, key: String, default: Boolean): Field = {
//        val view = BooleanCheckBoxView.optional[S](CellView.attr[S, Boolean, BooleanObj](attr, key),
//          name = name, default = default)
//        new Field(name, view) {
//          override lazy val editor: Component = {
//            val res = view.component
//            res.text = null
//            res
//          }
//        }
//      }

      // def mkEmptyField(): Field = new Field("", View.wrap[S](Swing.HGlue))

      import Negatum.Config.default._
      import Negatum._

      val fSeed = {
        val name = "Seed"
        val view = IntSpinnerView.optional[S](CellView.attr[S, Int, IntObj](attr, attrSeed),
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

      deferTx {
        def mkGrid(title: String, grid: Seq[Field]): Component = {
          val gridSz  = grid.size
          val gridC0  = grid.flatMap(v => v.label :: v.editor :: Nil)
          val gridC   = if (gridSz % 2 == 0) gridC0 else gridC0 ++ (Swing.HGlue :: Swing.HGlue :: Nil)
          val gridV   = gridC.grouped(4).toSeq
          val gridH   = gridV.transpose
          new GroupPanel {
            horizontal = Seq(gridH.map(col => Par          (col.map(c => GroupPanel.Element(c)): _*)): _*)
            vertical   = Seq(gridV.map(row => Par(Baseline)(row.map(c => GroupPanel.Element(c)): _*)): _*)
            border     = Swing.TitledBorder(Swing.EmptyBorder(4), s"<HTML><BODY><B>$title</B></BODY>")
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
        guiInit(panelParams)
      }
      this
    }

    def negatum  (implicit tx: S#Tx): Negatum[S]                  = negatumH()
    def rendering(implicit tx: S#Tx): Option[Rendering[S, Unit]]  = renderRef()

    private def guiInit(panelParams: Component): Unit = {

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

              def finished()(implicit tx: S#Tx): Unit = {
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

      val panelControl = new FlowPanel(new Label("Iterations:"),
        ggNumIterations, ggProgress, ggCancel, ggStop, ggRender, ggAnalyze, ggDropProc)
      component = new BorderPanel {
        add(panelParams , BorderPanel.Position.Center)
        add(panelControl, BorderPanel.Position.South )
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      renderRef   .swap(None).foreach(_.cancel ())
      optimizeRef .swap(None).foreach(_.abort  ())
    }
  }
}