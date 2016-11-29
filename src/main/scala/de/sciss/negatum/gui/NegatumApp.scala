/*
 *  NegatumApp.scala
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

import java.awt.Color
import java.util.Locale
import javax.swing.UIManager
import javax.swing.plaf.ColorUIResource

import de.sciss.desktop.{Menu, WindowHandler}
import de.sciss.desktop.impl.{SwingApplicationImpl, WindowHandlerImpl}
import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.mellite
import de.sciss.mellite.gui._
import de.sciss.mellite.gui.impl.document.DocumentHandlerImpl
import de.sciss.mellite.{Application, Mellite, Prefs}
import de.sciss.synth.proc
import de.sciss.synth.proc.{Ensemble, SynthGraphObj, Workspace}

import scala.collection.immutable.{Seq => ISeq}
import scala.language.existentials
import scala.swing.Action
import scala.util.control.NonFatal
import de.sciss.numbers.Implicits._

/** The main entry point for the desktop Swing application.
  * Please note that this should _not_ be the main class of the project,
  * but you should always invoke `at.iem.sysson.Main`, because it first
  * initializes some type extensions that would be missing otherwise.
  */
object NegatumApp extends SwingApplicationImpl("Negatum") with mellite.Application {
  def LOG_FRAME: Boolean = false // true

  override lazy val windowHandler: WindowHandler = new WindowHandlerImpl(this, menuFactory) {
    override lazy val usesInternalFrames = {
      false // XXX TODO: eventually a preferences entry
    }

    override def usesNativeDecoration: Boolean = Prefs.nativeWindowDecoration.getOrElse(true)
  }

  private final case class Config(
                                 rattleVolume: Double = 1f, negatumVolume: Double = -9f.dbamp
                                 )

  override def init(): Unit = {
    val defaultConfig = Config()
    val p = new scopt.OptionParser[Config]("Negatum") {
      opt[Double]("rattle-volume")
        .text(s"Initial rattle volume (linear from zero to one; default: ${defaultConfig.rattleVolume})")
        .action { (v, c) => c.copy(rattleVolume = v) }

      opt[Double]("negatum-volume")
        .text(s"Initial Negatum volume (linear from zero to one; default: ${defaultConfig.negatumVolume})")
        .action { (v, c) => c.copy(rattleVolume = v) }
    }

    val config = p.parse(args, defaultConfig).getOrElse(defaultConfig)

    Locale.setDefault(Locale.US)    // (untested) make sure number formatting is consistent, while we do not have i18
    Application.init(this)

    // ---- look and feel ----

    try {
      Prefs.lookAndFeel.getOrElse(Prefs.LookAndFeel.default).install()
    } catch {
      case NonFatal(_) =>
    }

    // XXX TODO --- this should be fixed in Submin
    if (Mellite.isDarkSkin) {
      // for titled border
      UIManager.put("TitledBorder.titleColor" , new ColorUIResource(216, 220, 224))
      // for lucre matrix
      UIManager.put("Label.foreground"        , new ColorUIResource(216, 220, 224))
      UIManager.put("Label.disabledForeground", new ColorUIResource(new Color(216, 220, 224, 96)))
    }

    // ---- type extensions ----

    Mellite.initTypes()
    Negatum.init()
    NegatumObjView.init()
    Composition.registerActions()

    // XXX TODO --- bug in SoundProcesses; remove the following line when fixed (3.8.1)
    SynthGraphObj .init()

    print("Creating fresh workspace...")
    val ws = Composition.createFreshWorkspace()
    println(" ok.")
    ActionOpenWorkspace.openGUI(ws)

    if (LOG_FRAME) LogFrame.instance    // init

    val mf = new mellite.gui.MainFrame
    startEnsemble(ws)
    new ImperfectFrame(mf, defaultRattleVolume = config.rattleVolume, defaultNegatumVolume = config.negatumVolume)
  }

  def startEnsemble[S <: Sys[S]](ws: Workspace[S]): Unit = {
    import proc.Implicits._
    implicit val _ws    : Workspace [S] = ws
    implicit val cursor : stm.Cursor[S] = ws.cursor
    GUI.atomic[S, Unit]("Main Ensemble ", s"Opening main ensemble elements window for '${ws.name}'") {
      implicit tx =>
        val ensMap = ws.root.iterator.collect {
          case ens: Ensemble[S] => ens.name -> ens
        }.toMap

        ensMap.get("ens-negatum-listen").fold[Unit] {
          Console.err.println("WARNING: ens-negatum-listen not found!")
        } { ens =>
          ens.stop()
        }
        ensMap.get("main").fold[Unit] {
          Console.err.println("WARNING: main ensemble not found!")
        } { ens =>
          ens.play()
          val ensFrame = EnsembleFrame[S](ens)
          val ensView  = ensFrame.ensembleView
          val ensT     = ensView.transport
          ensT.stop()
          ensT.seek(0L)
          ensT.play()
        }
    }
  }

  lazy val menuFactory: Menu.Root = {
    val res = MenuBar.instance
    val gActions = res.get("actions").get.asInstanceOf[Menu.Group]
    gActions.addLine()
    gActions.add(Menu.Item("binaural", Action("Binaural GUI...")(openBinaural())))
    res
  }

  private[this] lazy val _binaural = DelaunaySpace.mkGUI(exitOnClose = false, videoOption = false)

  def openBinaural(): Unit = _binaural.open()

//  def analyzeFeatures(): Unit = {
//    def invoke[S <: Sys[S]](implicit workspace: Workspace[S]): Unit = {
//      implicit val cursor: stm.Cursor[S] = workspace.cursor
//      cursor.step { implicit tx =>
//        FeatureAnalysisFrame[S]
//      }
//    }
//
//    documentHandler.activeDocument.foreach { ws =>
//      val wst = ws.asInstanceOf[Workspace[~] forSome { type ~ <: Sys[~]}]
//      invoke(wst)
//    }
//  }

//  protected def menuFactory: Root = MenuBar.instance

  override lazy val documentHandler: DocumentHandler = new DocumentHandlerImpl

  // ---- Application trait ----

  def topLevelObjects: ISeq[String]      = Mellite.topLevelObjects
  def objectFilter   : String => Boolean = Mellite.objectFilter
}
