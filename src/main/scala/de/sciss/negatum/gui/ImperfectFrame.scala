/*
 *  ImperfectFrame.scala
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

import java.awt.Font
import java.net.InetSocketAddress
import java.util.{Calendar, TimerTask}
import javax.swing.Timer

import de.sciss.desktop.{DialogSource, Window, WindowHandler}
import de.sciss.lucre.synth.Txn
import de.sciss.mellite.gui.MainFrame
import de.sciss.osc.UDP
import de.sciss.synth.proc.SoundProcesses
import de.sciss.{desktop, numbers, osc}
import numbers.Implicits._

import scala.concurrent.Future
import scala.concurrent.stm.TxnExecutor
import scala.swing.event.ValueChanged
import scala.swing.{BorderPanel, Button, Component, FlowPanel, GridPanel, Label, Point, Slider, Swing}
import scala.util.control.NonFatal

final class ImperfectFrame(mainFrame: MainFrame, defaultRattleVolume: Double, defaultNegatumVolume: Double,
                           hibernation: Boolean, silentStartHour: Int, silentStopHour: Int,
                           rebootMinutes: Int)
  extends desktop.impl.WindowImpl { me =>

//  def this(mainFrame: MainFrame) = this(mainFrame, 1.0, -9.0.dbamp)

  private[this] val rattleSocket  = new InetSocketAddress("192.168.0.21", 7771)
  private[this] val raspiSocket   = new InetSocketAddress("192.168.0.11", 57110)
  private[this] val houghSocket   = new InetSocketAddress("192.168.0.20", 57110)
  private[this] val negatumSocket = new InetSocketAddress("192.168.0.66", 57120)

  private[this] final val DEFAULT_RATTLE  = (defaultRattleVolume  * 200).toInt
  private[this] final val DEFAULT_NEGATUM = (defaultNegatumVolume * 200).toInt

  private[this] var _t: UDP.Transmitter.Undirected = _

  private def  t = {
    if (_t == null || !_t.isConnected) {
      val config = UDP.Config()
      //    config.localAddress = InetAddress.getLocalHost
      config.localSocketAddress = negatumSocket
      println(s"Local OSC address is ${config.localAddress}")
      _t = UDP.Transmitter(config)
    }
    _t
  }

  override def handler: WindowHandler = NegatumApp.windowHandler

  override protected def style: Window.Style = Window.Auxiliary

  def rebootRaspi(): Unit =
    trySend(raspiSocket, osc.Message("/forward", "/reboot"))

  def rebootHough(): Unit =
    trySend(houghSocket, osc.Message("/reboot"))

  def rebootNegatum(): Unit =
    Future {
      import sys.process._
      Seq("sudo", "reboot", "now").!
    } (SoundProcesses.executionContext)

  private def trySend(socket: InetSocketAddress, message: osc.Message): Unit = try {
    t.send(message, socket)
  } catch {
    case NonFatal(ex) =>
      val dlg = DialogSource.Exception(ex -> message.args.mkString(s"[${message.name}, ", ", ", "]"))
      dlg.show(Some(this))
  }

  contents = {
    // audio HH: shutdown, reboot, volume
    // audio DP: volume
    // video HH: shutdown, reboot
    // raspis  : shutdown, reboot
    // master volume
    // shutdown all

    def shutdownRaspi(): Unit =
      trySend(raspiSocket, osc.Message("/forward", "/shutdown"))

    def shutdownHough(): Unit =
      trySend(houghSocket, osc.Message("/shutdown"))

    def shutdownNegatum(): Unit =
      Future {  // we use a future because with the executionContext it will be scheduled where transactions are executed
        import sys.process._
        Seq("sudo", "shutdown", "now").!
      } (SoundProcesses.executionContext)

    val fontMachine = new Font(Font.SANS_SERIF, Font.BOLD , 36)
//    val fontButton  = new Font(Font.SANS_SERIF, Font.PLAIN, 24)

    def lbMachine(name: String): Label = {
      val res = new Label(name)
      res.font = fontMachine
      res
    }

    def button(name: String)(fun: => Unit): Button = {
      val res = Button(name)(fun)
      res.font = fontMachine
      res
    }

    val ggRaspiReboot = button("Reboot")(rebootRaspi())

    val ggRaspiShutdown = button("Shutdown") {
      shutdownRaspi()
    }
    val ggHoughReboot = button("Reboot")(rebootHough())
    val ggHoughShutdown = button("Shutdown") {
      shutdownHough()
    }

    val ggNegatumReboot = button("Reboot")(rebootNegatum())
    val ggNegatumShutdown = button("Shutdown") {
      shutdownNegatum()
    }

    def sendRattleVolume(mul: Float): Unit = {
      import ggRattleVolume.{value, min, max}
      val v = value.linLin(min, max, 0f, mul)
      trySend(rattleSocket, osc.Message("/ampImp", v))
    }

    def sendNegatumVolume(mul: Float): Unit = {
      import ggNegatumVolume.{value, min, max}
      val v = value.linLin(min, max, 0f, mul)
      TxnExecutor.defaultAtomic { itx =>
        implicit val tx: Txn = Txn.wrap(itx)
        mainFrame.setMainVolume(v)
      }
    }

    def updateVolumes(): Unit = {
      val mainVolume0 = {
        import ggMainVolume.{min, max, value}
        val v = value.linLin(min, max, 0f, 1f)
        v
      }

      val mainVolume: Float = if (!hibernation) mainVolume0 else {
        val c     = Calendar.getInstance()
        val hour  = c.get(Calendar.HOUR_OF_DAY)
        val range: Seq[Int] = if (silentStartHour < silentStopHour)
          silentStartHour until silentStopHour
        else
          (silentStartHour to 24) ++ (0 until silentStopHour)

        val silent = range.contains(hour)
        if (silent) println("(silent)")
        if (silent) 0f else mainVolume0
      }

      sendRattleVolume (mainVolume)
      sendNegatumVolume(mainVolume)
    }

    lazy val ggMainVolume = new Slider {
      min   = 0
      max   = 200
      value = max
      // font  = fontButton
      listenTo(this)
      reactions += {
        case ValueChanged(_) => updateVolumes()
      }
    }

    lazy val ggNegatumVolume = new Slider {
      min   = 0
      max   = 200
      value = DEFAULT_NEGATUM.clip(min, max)
      // font  = fontButton
      listenTo(this)
      reactions += {
        case ValueChanged(_) => updateVolumes()
      }
    }

    lazy val ggRattleVolume = new Slider {
      min   = 0
      max   = 200
      value = DEFAULT_RATTLE.clip(min, max)
      // font  = fontButton
      listenTo(this)
      reactions += {
        case ValueChanged(_) => updateVolumes()
      }
    }

    val ggShutdownAll = Button("Shutdown All") {
      shutdownRaspi()
      shutdownHough()
      shutdownNegatum()
    }
    ggShutdownAll.font = new Font(Font.SANS_SERIF, Font.BOLD, 48)

    def panel(content: Component*): GridPanel = new GridPanel(4, 1) {
      contents ++= content
      border = Swing.EmptyBorder(48)
    }

    val pHough   = panel(lbMachine("Hough (Projektion)"), ggHoughShutdown  , ggHoughReboot)
    val pNegatum = panel(lbMachine("Negatum (Sound)"   ),
      new FlowPanel(new Label("Volume:"), ggNegatumVolume), ggNegatumShutdown, ggNegatumReboot)
    val pRaspi   = panel(lbMachine("Raspi (Monitore)"  ), ggRaspiShutdown  , ggRaspiReboot)

    val pRattle = panel(lbMachine("Rattle (Sound)"), new FlowPanel(new Label("Volume:"), ggRattleVolume))

    val pGrid = new GridPanel(2, 2) {
      contents ++= Seq(pRattle, pNegatum, pHough, pRaspi)
    }

    updateVolumes()
    // david's computer occasionally crashes and then comes back with full
    // volume; so if stuff had master turned down, this results in people
    // believing the sound is on, when only half of it is audible. We simply
    // resend the fader positions once every minute.
    val timer = new Timer(60 * 1000, Swing.ActionListener(_ => updateVolumes()))
    timer.setRepeats(true)
    timer.restart()

    new BorderPanel {
      add(new FlowPanel(new Label("Main Volume:"), ggMainVolume), BorderPanel.Position.North)
      add(ggShutdownAll, BorderPanel.Position.Center)
      add(pGrid, BorderPanel.Position.South)
    }
  }

  closeOperation = Window.CloseIgnore
  location = new Point(400, 150)
  title    = "Imperfect Reconstruction"

  pack()
  front()
  alwaysOnTop = true

  if (rebootMinutes > 0) {
    val rebootTimer = new java.util.Timer
    val rebootDelay: Long = rebootMinutes.toLong * 60 * 1000
    println(s"Reboot after $rebootDelay ms.")
    rebootTimer.schedule(new TimerTask {
      def run(): Unit = Swing.onEDT {
        println("Rebooting...")
        rebootHough()
        rebootRaspi()
        Thread.sleep(2000)
        rebootNegatum()
      }
    }, rebootDelay)
  }

  //  override protected def checkClose(): Boolean = false
}
