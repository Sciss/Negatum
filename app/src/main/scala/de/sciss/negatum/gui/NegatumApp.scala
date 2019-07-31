/*
 *  NegatumApp.scala
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

package de.sciss.negatum
package gui

import java.awt.Color
import java.util.Locale

import javax.swing.UIManager
import javax.swing.plaf.ColorUIResource
import de.sciss.desktop.Menu
import de.sciss.desktop.impl.SwingApplicationImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Cursor
import de.sciss.lucre.synth.Sys
import de.sciss.mellite
import de.sciss.mellite.gui._
import de.sciss.mellite.{Application, GUI, Mellite, Prefs}
import de.sciss.numbers.Implicits._
import de.sciss.synth.proc
import de.sciss.synth.proc.{AuralSystem, Code, Durable, Ensemble, SynthGraphObj, Universe, Workspace}

import scala.collection.immutable.{Seq => ISeq}
import scala.swing.Action
import scala.util.control.NonFatal

/** The main entry point for the desktop Swing application.
  * Please note that this should _not_ be the main class of the project,
  * but you should always invoke `at.iem.sysson.Main`, because it first
  * initializes some type extensions that would be missing otherwise.
  */
object NegatumApp extends SwingApplicationImpl[Application.Document]("Negatum") with mellite.Application {
  def LOG_FRAME: Boolean = false // true

//  override lazy val windowHandler: WindowHandler = new WindowHandlerImpl(this, menuFactory) {
//    override lazy val usesInternalFrames: Boolean = {
//      false // XXX TODO: eventually a preferences entry
//    }
//
//    override def usesNativeDecoration: Boolean = Prefs.nativeWindowDecoration.getOrElse(true)
//  }

  private final case class Config(
    rattleVolume    : Double  =  0.0.dbAmp,
    negatumVolume   : Double  = -2.5.dbAmp,
    openWorkspace   : Boolean = false,
    playEnsemble    : Boolean = false,
    imperfectFrame  : Boolean = false,
    hibernation     : Boolean = false,
    silentStartHour : Int     = 22,
    silentStopHour  : Int     =  8,
    rebootMinutes   : Int     =  0
  )

  override def init(): Unit = {
    val defaultConfig = Config()
    val p = new scopt.OptionParser[Config]("Negatum") {
      opt[Double]("rattle-volume")
        .text(s"Initial rattle volume (linear from zero to one; default: ${defaultConfig.rattleVolume})")
        .action { (v, c) => c.copy(rattleVolume = v) }

      opt[Double]("negatum-volume")
        .text(s"Initial Negatum volume (linear from zero to one; default: ${defaultConfig.negatumVolume})")
        .action { (v, c) => c.copy(negatumVolume = v) }

      opt[Unit]("workspace")
        .text("Create Negatum workspace upon start")
        .action { (_, c) => c.copy(openWorkspace = true) }

      opt[Unit]("autoplay")
        .text("Play Negatum ensemble upon start")
        .action { (_, c) => c.copy(playEnsemble = true) }

      opt[Unit]("imperfect")
        .text("Create Imperfect Reconstruction control frame")
        .action { (_, c) => c.copy(imperfectFrame = true) }

      opt[Unit]("hibernation")
        .text("Enable hibernation mode")
        .action { (_, c) => c.copy(hibernation = true) }

      opt[Int]("silent-start")
        .text(s"Silent start hour during hibernation (0 to 24; default: ${defaultConfig.silentStartHour})")
        .action { (v, c) => c.copy(silentStartHour = v) }

      opt[Int]("silent-stop")
        .text(s"Silent stop hour during hibernation (0 to 24; default: ${defaultConfig.silentStopHour})")
        .action { (v, c) => c.copy(silentStopHour = v) }

      opt[Int]("reboot-minutes")
        .text(s"Reboot raspi, hough, negatum after X minutes (0 for no reboot; default: ${defaultConfig.rebootMinutes})")
        .action { (v, c) => c.copy(rebootMinutes = v) }
    }

    val config = p.parse(args, defaultConfig).getOrElse(sys.exit(1))

    Locale.setDefault(Locale.US)    // (untested) make sure number formatting is consistent, while we do not have i18
    Application.init(this)

    // ---- look and feel ----

    try {
      Prefs.lookAndFeel.getOrElse(Prefs.LookAndFeel.default).install()
    } catch {
      case NonFatal(_) =>
    }

    // XXX TODO --- this should be fixed in Submin
    if (GUI.isDarkSkin) {
      // for titled border
      UIManager.put("TitledBorder.titleColor" , new ColorUIResource(216, 220, 224))
      // for lucre matrix
      UIManager.put("Label.foreground"        , new ColorUIResource(216, 220, 224))
      UIManager.put("Label.disabledForeground", new ColorUIResource(new Color(216, 220, 224, 96)))
    }

    // ---- type extensions ----

    Mellite.initTypes()
    Negatum         .init()
    SVMModel        .init()
    SOM             .init()

    NegatumObjView  .init()
    SVMModelObjView .init()
    SOMObjView      .init()

    if (config.hibernation) {
      Hibernation.registerActions()
    } else {
      Composition.registerActions()
    }

    // XXX TODO --- bug in SoundProcesses; remove the following line when fixed (3.8.1)
    SynthGraphObj .init()

    type S = Durable

    val uOpt = if (!config.openWorkspace) None else {
      print("Creating workspace...")
      implicit val ws: Workspace.Durable = if (config.hibernation) {
        Hibernation.createWorkspace()
      } else {
        Composition.createFreshWorkspace()
      }
      println(" ok.")
      implicit val cursor: Cursor[S] = ws.cursor
      val u: Universe[S] = cursor.step { implicit tx => Universe() }
      ActionOpenWorkspace.openGUI(u)
      Some(u)
    }

    if (LOG_FRAME) LogFrame.instance    // init

    val mf = new mellite.gui.MainFrame

    if (config.playEnsemble) uOpt.foreach { implicit u =>
      startEnsemble[S](hibernation = config.hibernation)
    }

    if (config.imperfectFrame) {
      new ImperfectFrame(mf, defaultRattleVolume = config.rattleVolume,
        defaultNegatumVolume = config.negatumVolume, hibernation = config.hibernation,
        silentStartHour = config.silentStartHour, silentStopHour = config.silentStopHour,
        rebootMinutes = config.rebootMinutes)
    }
  }

  def startEnsemble[S <: Sys[S]](hibernation: Boolean)(implicit u: Universe[S]): Unit = {
    import proc.Implicits._
    implicit val _ws    : Workspace [S] = u.workspace
    implicit val cursor : stm.Cursor[S] = u.cursor
    GUI.atomic[S, Unit]("Main Ensemble ", s"Opening main ensemble elements window for '${_ws.name}'") {
      implicit tx =>
        val ensMap = _ws.root.iterator.collect {
          case ens: Ensemble[S] => ens.name -> ens
        }.toMap

        val ensName = if (hibernation) "ens-hibernate-listen" else "ens-negatum-listen"

        ensMap.get(ensName).fold[Unit] {
          Console.err.println(s"WARNING: $ensName not found!")
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

//  override lazy val documentHandler: DocumentHandler = new DocumentHandlerImpl

  // ---- Application trait ----

  def topLevelObjects : ISeq[String]      = Mellite.topLevelObjects
  def objectFilter    : String => Boolean = Mellite.objectFilter
  def auralSystem     : AuralSystem       = Mellite.auralSystem
  def compiler        : Code.Compiler     = Mellite.compiler
}
