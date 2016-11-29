/*
 *  ImperfectFrame.scala
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

import java.awt.Font
import java.net.InetSocketAddress

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

final class ImperfectFrame(mainFrame: MainFrame) extends desktop.impl.WindowImpl { me =>

  private[this] val rattleSocket  = new InetSocketAddress("192.168.0.21", 7771)
  private[this] val raspiSocket   = new InetSocketAddress("192.168.0.11", 57110)
  private[this] val houghSocket   = new InetSocketAddress("192.168.0.20", 57110)
  private[this] val negatumSocket = new InetSocketAddress("192.168.0.66", 57120)

  final val DEFAULT_RATTLE  = (0.8 * 200).toInt
  final val DEFAULT_NEGATUM = (0.2 * 200).toInt

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
    val fontButton  = new Font(Font.SANS_SERIF, Font.PLAIN, 24)

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

    val ggRaspiReboot = button("Reboot") {
      trySend(raspiSocket, osc.Message("/forward", "/reboot"))
    }
    val ggRaspiShutdown = button("Shutdown") {
      shutdownRaspi()
    }
    val ggHoughReboot = button("Reboot") {
      trySend(houghSocket, osc.Message("/reboot"))
    }
    val ggHoughShutdown = button("Shutdown") {
      shutdownHough()
    }

    val ggNegatumReboot = button("Reboot") {
      Future {
        import sys.process._
        Seq("sudo", "reboot", "now").!
      } (SoundProcesses.executionContext)
    }
    val ggNegatumShutdown = button("Shutdown") {
      shutdownNegatum()
    }

    def sendRattleVolume(mul: Float): Unit = {
      import ggRattleVolume.{value, min, max}
      val v = value.linlin(min, max, 0f, mul)
      trySend(rattleSocket, osc.Message("/ampImp", v))
    }

    def sendNegatumVolume(mul: Float): Unit = {
      import ggNegatumVolume.{value, min, max}
      val v = value.linlin(min, max, 0f, mul)
      TxnExecutor.defaultAtomic { itx =>
        implicit val tx = Txn.wrap(itx)
        mainFrame.setMainVolume(v)
      }
    }

    def updateVolumes(): Unit = {
      val mainVolume = {
        import ggMainVolume.{min, max, value}
        val v = value.linlin(min, max, 0f, 1f)
        v
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

  //  override protected def checkClose(): Boolean = false
}
