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
import java.net.{InetAddress, InetSocketAddress}

import de.sciss.desktop.Window.{CloseIgnore, CloseOperation, Style}
import de.sciss.desktop.{Desktop, DialogSource, Window, WindowHandler}
import de.sciss.lucre.swing.{CellView, View, Window}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.{desktop, numbers, osc}
import de.sciss.osc.UDP
import de.sciss.synth.proc.SoundProcesses

import scala.concurrent.Future
import scala.swing.event.ValueChanged
import scala.swing.{Button, Component, FlowPanel, GridPanel, Label, Point, Slider, Swing}
import scala.util.control.NonFatal

final class ImperfectFrame extends desktop.impl.WindowImpl { me =>

  private[this] val rattleSocket  = new InetSocketAddress("192.168.0.21", 7771)
  private[this] val raspiSocket   = new InetSocketAddress("192.168.0.11", 57110)
  private[this] val houghSocket   = new InetSocketAddress("192.168.0.20", 57110)
  private[this] val negatumSocket = new InetSocketAddress("192.168.0.66", 57120)

  private[this] val t = {
    val config = UDP.Config()
//    config.localAddress = InetAddress.getLocalHost
    config.localSocketAddress = negatumSocket
    println(s"Local OSC address is ${config.localAddress}")
    UDP.Transmitter(config)
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
    val ggRattleVolume = new Slider {
      min   = 0
      max   = 200
      value = max
      // font  = fontButton
      listenTo(this)
      reactions += {
        case ValueChanged(_) =>
          import numbers.Implicits._
          val v = value.linlin(min, max, 0f, 1f)
          trySend(rattleSocket, osc.Message("/ampImp", v))
      }
    }

    val ggShutdownAll = Button("Shutdown All") {
      shutdownRaspi()
      shutdownHough()
      shutdownNegatum()
    }
    ggShutdownAll.font = new Font(Font.SANS_SERIF, Font.BOLD, 48)

    def panel(content: Component*): GridPanel = new GridPanel(3, 1) {
      contents ++= content
      border = Swing.EmptyBorder(48)
    }

    val pHough   = panel(lbMachine("Hough (Projektion)"), ggHoughShutdown  , ggHoughReboot)
    val pNegatum = panel(lbMachine("Negatum (Sound)"   ), ggNegatumShutdown, ggNegatumReboot)
    val pRaspi   = panel(lbMachine("Raspi (Monitore)"  ), ggRaspiShutdown  , ggRaspiReboot)

    val pRattle = panel(lbMachine("Rattle (Sound)"), new FlowPanel(new Label("Volume:"), ggRattleVolume))

    new GridPanel(2, 2) {
      contents ++= Seq(pRattle, pNegatum, pHough, pRaspi)
    }
  }

  closeOperation = Window.CloseIgnore
  location = new Point(420, 200)
  title    = "Imperfect Reconstruction"

  pack()
  front()

  //  override protected def checkClose(): Boolean = false
}
