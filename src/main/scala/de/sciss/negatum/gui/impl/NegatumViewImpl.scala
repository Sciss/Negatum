/*
 *  NegatumViewImpl.scala
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

import javax.swing.SpinnerNumberModel

import de.sciss.desktop.UndoManager
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.icons.raphael
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj}
import de.sciss.lucre.stm
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.{BooleanCheckBoxView, DoubleSpinnerView, IntSpinnerView, View}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.{AttrCellView, GUI}
import de.sciss.negatum.Negatum.Rendering
import de.sciss.swingplus.{GroupPanel, Spinner}
import de.sciss.synth.proc.Workspace

import scala.concurrent.stm.Ref
import scala.swing.{BorderPanel, BoxPanel, Component, FlowPanel, Label, Orientation, ProgressBar, Swing}

object NegatumViewImpl {
  def apply[S <: Sys[S]](n: Negatum[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                        workspace: Workspace[S]): NegatumView[S] = {
    implicit val undo = new UndoManagerImpl
    val res = new Impl[S](tx.newHandle(n))
    res.init(n)
  }

  private final class Impl[S <: Sys[S]](negatumH: stm.Source[S#Tx, Negatum[S]])
                                       (implicit val cursor: stm.Cursor[S],
                                        val workspace: Workspace[S], val undoManager: UndoManager)
    extends NegatumView[S] with ComponentHolder[Component] {

    def init(n: Negatum[S])(implicit tx: S#Tx): this.type = {
      val attr  = n.attr
      implicit val intEx      = IntObj
      implicit val doubleEx   = DoubleObj
      implicit val booleanEx  = BooleanObj

      class Field(name: String, view: View[S]) {
        lazy val label : Label      = new Label(s"$name:")
        def      editor: Component  = view.component
      }

      def mkIntField(name: String, key: String, default: Int): Field = {
        val view = IntSpinnerView.optional(AttrCellView[S, Int, IntObj](attr, key),
          name = name, default = Some(default))
        new Field(name, view)
      }

      def mkDoubleField(name: String, key: String, default: Double): Field = {
        val view = DoubleSpinnerView.optional(AttrCellView[S, Double, DoubleObj](attr, key),
          name = name, default = Some(default))
        new Field(name, view)
      }

      def mkBooleanField(name: String, key: String, default: Boolean): Field = {
        val view = BooleanCheckBoxView.optional(AttrCellView[S, Boolean, BooleanObj](attr, key),
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
        val view = IntSpinnerView.optional(AttrCellView[S, Int, IntObj](attr, attrSeed),
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

      val fEvalNumMFCC        = mkIntField    ("# of MFCC"              , attrEvalNumMFCC   , eval.numMFCC)
      val fEvalNormMFCC       = mkBooleanField("Normalize MFCC"         , attrEvalNormMFCC  , eval.normMFCC)
      val fEvalMaxBoost       = mkDoubleField ("Max. Boost"             , attrEvalMaxBoost  , eval.maxBoost)
      val fEvalTempWeight     = mkDoubleField ("Temporal Weight"        , attrEvalTimeWeight, eval.timeWeight)
      val gridEval = Seq(fEvalNumMFCC, fEvalMaxBoost, fEvalNormMFCC, fEvalTempWeight)

      val fBreedSelFrac       = mkDoubleField ("Selection Fraction"     , attrBreedSelectFrac  , breed.selectFrac)
      val fBreedElitism       = mkIntField    ("Elitism"                , attrBreedElitism  , breed.elitism)
      val fBreedMinMut        = mkIntField    ("Min. # of Mutations"    , attrBreedMinMut   , breed.minMut)
      val fBreedMaxMut        = mkIntField    ("Max. # of Mutations"    , attrBreedMaxMut   , breed.maxMut)
      val fBreedProbMut       = mkDoubleField ("Prob. of Mutation"      , attrBreedProbMut  , breed.probMut)
      val fBreedGolem         = mkIntField    ("# of Golems"            , attrBreedGolem    , breed.golem)
      val gridBreed = Seq(fBreedSelFrac, fBreedProbMut, fBreedElitism, fBreedMinMut, fBreedGolem, fBreedMaxMut)

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

    private[this] val renderRef = Ref(Option.empty[Negatum.Rendering[S]])

    def negatum  (implicit tx: S#Tx): Negatum[S]            = negatumH()
    def rendering(implicit tx: S#Tx): Option[Rendering[S]]  = renderRef.get(tx.peer)

    private def guiInit(panelParams: Component): Unit = {

      val ggProgress: ProgressBar = new ProgressBar
      ggProgress.max = 160

      val actionCancel: swing.Action = new swing.Action(null) {
        def apply(): Unit = cursor.step { implicit tx =>
          renderRef.swap(None)(tx.peer).foreach(_.cancel())
        }
        enabled = false
      }

      val actionStop: swing.Action = new swing.Action(null) {
        def apply(): Unit = cursor.step { implicit tx =>
          renderRef.swap(None)(tx.peer).foreach(_.stop())
        }
        enabled = false
      }

      val ggCancel  = GUI.toolButton(actionCancel, raphael.Shapes.Cross        , tooltip = "Abort Rendering")
      val ggStop    = GUI.toolButton(actionStop  , raphael.Shapes.TransportStop, tooltip = "Stop Rendering and Update Table")

      val mNumIter  = new SpinnerNumberModel(1, 1, 65536, 1)
      val ggNumIter = new Spinner(mNumIter)

      // XXX TODO --- should use custom view so we can cancel upon `dispose`
      val actionRender = new swing.Action("Evolve") { self =>
        def apply(): Unit = {
          val numIter = mNumIter.getNumber.intValue()
          val ok = cursor.step { implicit tx =>
            renderRef.get(tx.peer).isEmpty && {
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
                selectFrac  = attr.$[DoubleObj ](attrBreedSelectFrac).map(_.value).getOrElse(breed.selectFrac),
                elitism     = attr.$[IntObj    ](attrBreedElitism   ).map(_.value).getOrElse(breed.elitism),
                minMut      = attr.$[IntObj    ](attrBreedMinMut    ).map(_.value).getOrElse(breed.minMut),
                maxMut      = attr.$[IntObj    ](attrBreedMaxMut    ).map(_.value).getOrElse(breed.maxMut),
                probMut     = attr.$[DoubleObj ](attrBreedProbMut   ).map(_.value).getOrElse(breed.probMut),
                golem       = attr.$[IntObj    ](attrBreedGolem     ).map(_.value).getOrElse(breed.golem)
              )
              val cEval     = Negatum.Evaluation(
                numMFCC     = attr.$[IntObj    ](attrEvalNumMFCC    ).map(_.value).getOrElse(eval.numMFCC),
                normMFCC    = attr.$[BooleanObj](attrEvalNormMFCC   ).map(_.value).getOrElse(eval.normMFCC),
                maxBoost    = attr.$[DoubleObj ](attrEvalMaxBoost   ).map(_.value).getOrElse(eval.maxBoost),
                timeWeight  = attr.$[DoubleObj ](attrEvalTimeWeight ).map(_.value).getOrElse(eval.timeWeight)
              )
              val cPenalty  = Negatum.Penalty()
              val config    = Negatum.Config(seed = seed, generation = cGen, breeding = cBreed, evaluation = cEval,
                penalty = cPenalty)

              def finished()(implicit tx: S#Tx): Unit = {
                renderRef.set(None)(tx.peer)
                deferTx {
                  actionCancel.enabled  = false
                  actionStop  .enabled  = false
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

      val panelControl = new FlowPanel(new Label("Iterations:"),
        ggNumIter, ggProgress, ggCancel, ggStop, ggRender, ggAnalyze)
      component = new BorderPanel {
        add(panelParams , BorderPanel.Position.Center)
        add(panelControl, BorderPanel.Position.South )
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = rendering.foreach(_.cancel())
  }
}
