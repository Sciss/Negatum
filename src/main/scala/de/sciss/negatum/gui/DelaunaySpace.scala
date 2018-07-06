/*
 *  DelaunaySpace.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum.gui

import java.awt.image.BufferedImage
import java.awt.{Color, RenderingHints}
import javax.imageio.ImageIO

import de.sciss.file._
import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth.{Group, Server, Synth, Txn}
import de.sciss.mellite.Mellite
import de.sciss.negatum.Binaural
import de.sciss.negatum.Binaural.{Person, Radians}
import de.sciss.negatum.Delaunay.{TriangleIndex, Vector2}
import de.sciss.negatum.Speakers._
import de.sciss.numbers.Implicits._
import de.sciss.synth
import de.sciss.synth.proc.AuralSystem
import de.sciss.synth.proc.AuralSystem.Client
import de.sciss.synth.swing.ServerStatusPanel
import de.sciss.synth.{SynthGraph, addToHead, addToTail}

import scala.Predef.{any2stringadd => _, _}
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm
import scala.concurrent.stm.{Ref, atomic}
import scala.swing.event.{MouseDragged, MouseMoved, MousePressed, MouseReleased}
import scala.swing.{BorderPanel, Button, Component, Dimension, Frame, Graphics2D, Point, Swing}

object DelaunaySpace {
  private val synthOpt = Ref(Option.empty[Synth])
  private val binOpt   = Ref(Option.empty[Group])

  def as: AuralSystem = Mellite.auralSystem

  def main(args: Array[String]): Unit = {
    Swing.onEDT(mkGUI(exitOnClose = true, videoOption = false))
    val cfg = Server.Config()
    cfg.outputBusChannels = select.size
    cfg.audioBusChannels  = 512
    cfg.deviceName        = Some("Negatum")
    atomic { itx =>
      implicit val tx: Txn = Txn.wrap(itx)
      as.addClient(new Client {
        def auralStarted(s: Server)(implicit tx: Txn): Unit = {
          import TxnLike.peer
          s.nextNodeId()  // XXX TODO -- ugly
          s.nextNodeId()
          synthOpt() = Some(mkSynth(s))
          import synth.swing.Implicits._
          Swing.onEDT {
            val f = s.peer.gui.meter()
            f.location = new Point(0, 50)
            val ssp = new ServerStatusPanel
            ssp.server = Some(s.peer)
            new Frame {
              contents = ssp
              pack().open()
            }
          }
        }

        def auralStopped()(implicit tx: Txn): Unit = ()
      })
      as.start(cfg)
    }
  }

  def mkSynth(s: Server)(implicit tx: Txn): Synth = {
    import synth.Ops._
    import synth.ugen
    import ugen._
    val g = SynthGraph {
      val sig = WhiteNoise.ar(0.08)
      val px  = "x".kr(0f)
      val py  = "y".kr(0f)
      val amp = NegatumDelaunay(px, py)
      Out.ar(0, sig * amp)
    }
    val syn = Synth(s, g)
    syn.play(target = s.defaultGroup, args = Nil, addAction = addToHead, dependencies = Nil)
    syn
  }

  def intersectLineLineF(a1x: Float, a1y: Float, a2x: Float, a2y: Float,
                         b1x: Float, b1y: Float, b2x: Float, b2y: Float): (Float, Float) =  {
    val dax   = a2x - a1x
    val day   = a2y - a1y
    val dbx   = b2x - b1x
    val dby   = b2y - b1y
    val dx1   = a1x - b1x
    val dy1   = a1y - b1y
    val ua_t  = dbx*dy1 - dby*dx1
    // val ub_t  = dax*dy1 - day*dx1
    val u_b   = dby*dax - dbx*day

    require (u_b != 0)

    val ua = ua_t / u_b
    // val ub = ub_t / u_b

    val ix = a1x + ua * dax
    val iy = a1y + ua * day
    (ix, iy)
  }

  def mkGUI(exitOnClose: Boolean, videoOption: Boolean): Frame = {
    val selectW = maxX - minX
    val selectH = maxY - minY
    val pad     = 16
    val padT    = pad << 1
    val prefW   = 800
    val prefH   = (prefW * selectH / selectW + 0.5).toInt
    val triLn0: Vec[List[(Int, Int)]] = tri.map { case TriangleIndex(a, b, c) =>
      List(
        (math.min(a, b), math.max(a, b)),
        (math.min(a, c), math.max(a, c)),
        (math.min(b, c), math.max(b, c))
      )
    }
    val triLn: Vec[(Int, Int)] = triLn0.flatten.distinct
//    val lineIndices: Vec[(Int, Int, Int)] = triLn0.map { case key1 :: key2 :: key3 :: Nil =>
//      (triLn.indexOf(key1), triLn.indexOf(key2), triLn.indexOf(key3))
//    }
    
    object view extends Component {
      preferredSize = new Dimension(prefW + padT, prefH + padT)
      opaque        = true

      var ptMouse = new Point(-1, -1)
      var ptBin1  = new Point(-1, -1)
      var ptBin   = Person(Vector2(-1, -1), Radians(0))

      listenTo(mouse.clicks)
      listenTo(mouse.moves)
      reactions += {
        case e: MouseMoved =>
          ptMouse = e.point
          repaint()
        case e: MousePressed =>
          ptBin1 = e.point
        case _: MouseDragged =>
        case e: MouseReleased =>
//          synthOpt.single().foreach(x => println(x.peer.server.counts))
          atomic { itx =>
            implicit val tx: Txn = Txn.wrap(itx)
            as.serverOption.foreach { s =>
              val w     = peer.getWidth
              val h     = peer.getHeight
              val wi    = w - padT
              val hi    = h - padT
              val scale = math.min(wi / selectW, hi / selectH)
              val px    = (ptMouse.x - pad) / scale + minX
              val py    = (ptMouse.y - pad) / scale + minY
//              println(f"listener pos $sx%g, $sx%g")
              val angle = math.atan2(-(e.point.y - ptBin1.y), e.point.x - ptBin1.x)
              ptBin     = Person(pos = Vector2(px, py), azi = Radians(angle))
              binOpt.get(itx).foreach(_.free())
              val bin = Binaural.build(target = s.defaultGroup, addAction = addToTail, listener = ptBin)
              binOpt.set(Some(bin))(itx)
              //          synthOpt.foreach(x => x.server.dumpOSC(osc.Dump.Text))
            }
          }
          repaint()
//          synthOpt.foreach(x => x.server.dumpOSC(osc.Dump.Off))
      }

      private[this] val colrGreen = new Color(0, 0xA0, 0)
      private[this] val colrRed   = Color.red
      private[this] val colrBlue  = Color.blue

      override def paintComponent(g: Graphics2D): Unit = {
        g.setColor(Color.lightGray)
        val w     = peer.getWidth
        val h     = peer.getHeight
        val wi    = w - padT
        val hi    = h - padT
        val scale = math.min(wi / selectW, hi / selectH)
        g.fillRect(0, 0, w, h)
        g.setColor(Color.black)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

        def drawPoint(x: Float, y: Float): Unit = {
          val sx = ((x - minX) * scale + pad + 0.5).toInt
          val sy = ((y - minY) * scale + pad + 0.5).toInt
          g.drawRect(sx - 2, sy - 2, 5, 5)
        }

        def drawCircle(x: Float, y: Float, radius: Float): Unit = {
          val sx = ((x - minX) * scale + pad + 0.5).toInt
          val sy = ((y - minY) * scale + pad + 0.5).toInt
//          g.drawRect(sx - 2, sy - 2, 5, 5)
          val ri = (radius + 0.5).toInt
          g.drawOval(sx - ri, sy - ri, ri * 2, ri * 2)
        }

        select.zipWithIndex.foreach { case (v, vi) =>
          drawPoint(v.x, v.y)
          val sx = ((v.x - minX) * scale + pad + 0.5).toInt
          val sy = ((v.y - minY) * scale + pad + 0.5).toInt
          g.drawString(vi.toString, sx, sy)
        }

        def drawLine(x1: Float, y1: Float, x2: Float, y2: Float): Unit = {
          val sx1 = ((x1 - minX) * scale + pad + 0.5).toInt
          val sy1 = ((y1 - minY) * scale + pad + 0.5).toInt
          val sx2 = ((x2 - minX) * scale + pad + 0.5).toInt
          val sy2 = ((y2 - minY) * scale + pad + 0.5).toInt
          g.drawLine(sx1, sy1, sx2, sy2)
        }

        triLn.foreach { case (i1, i2) =>
          val v1 = select(i1)
          val v2 = select(i2)
          drawLine(v1.x, v1.y, v2.x, v2.y)
        }

        if (ptBin.pos.x >= 0) {
          val px = ptBin.pos.x  // ((ptBin.pos.x - minX) * scale + pad + 0.5).toInt
          val py = ptBin.pos.y  // ((ptBin.pos.y - minY) * scale + pad + 0.5).toInt
//          val px = (ptBin.pos.x - pad) / scale + minX
//          val py = (ptBin.pos.y - pad) / scale + minY
          g.setColor(Color.blue)
          drawCircle(px, py, 4)
          val angle = ptBin.azi.value
          drawLine(px, py, px + math.cos(angle).toFloat * 12, py - math.sin(angle).toFloat * 12)
        }

        if (ptMouse.x >= 0) {
          val px = (ptMouse.x - pad) / scale + minX
          val py = (ptMouse.y - pad) / scale + minY
          val nx = px.linLin(minX, maxX, 0, 1)
          val ny = py.linLin(minY, maxY, 0, 1)

          stm.atomic { itx =>
            implicit val tx: Txn = Txn.wrap(itx)
            synthOpt.get(itx).foreach { n =>
              n.set("x" -> nx, "y" -> ny)
            }
          }

          g.setColor(Color.magenta)
          drawCircle(px, py, 4)

          g.setColor(colrRed)
//          val prjSides: Vec[Proj] = triLn.map { case (i1, i2) =>
//            val v1    = select(i1)
//            val v2    = select(i2)
//            val res   = projectPointOntoLineSegment(v1.x, v1.y, v2.x, v2.y, px, py)
//            if (res.inside) drawPoint(res.x, res.y)
//            res
//          }

          val inside = tri.indexWhere { case TriangleIndex(i1, i2, i3) =>
            val v1    = select(i1)
            val v2    = select(i2)
            val v3    = select(i3)
            // cf. https://en.wikipedia.org/wiki/Barycentric_coordinate_system
            val dx3   = px   - v3.x
            val dy3   = py   - v3.y
            // det of 2x2 matrix: r1c1 * r2c2 - r1c2 * r2c1
            // where r1c1 = x1 - x3, r2c2 = y2 - y3,
            //       r1c2 = x2 - x3, r2c1 = y1 - y3
            val detT  = (v2.y - v3.y) * (v1.x - v3.x) + (v3.x - v2.x) * (v1.y - v3.y)
            val alpha = ((v2.y - v3.y) * dx3 + (v3.x - v2.x) * dy3) / detT
            val beta  = ((v3.y - v1.y) * dx3 + (v1.x - v3.x) * dy3) / detT
            val gamma = 1.0f - alpha - beta
            alpha >= 0 && beta >= 0 && gamma >= 0
          }

          if (inside >= 0) {
//            val (i1, i2, i3) = lineIndices(inside)
//            val prj1 = prjSides(i1)
//            val prj2 = prjSides(i2)
//            val prj3 = prjSides(i3)
            val TriangleIndex(i1, i2, i3) = tri(inside)
            val v1    = select(i1)
            val v2    = select(i2)
            val v3    = select(i3)
            val (alt1, alt2, alt3) = altitudeProjections(inside)
            val a1x   = alt1.x
            val a1y   = alt1.y
            val a2x   = alt2.x
            val a2y   = alt2.y
            val a3x   = alt3.x
            val a3y   = alt3.y
//            val a1x   = (v2.x + v3.x)/2
//            val a1y   = (v2.y + v3.y)/2
//            val a2x   = (v3.x + v1.x)/2
//            val a2y   = (v3.y + v1.y)/2
//            val a3x   = (v1.x + v2.x)/2
//            val a3y   = (v1.y + v2.y)/2
            
//            val (a1x, a1y) = intersectLineLineF(v2.x, v2.y, v3.x, v3.y, v1.x, v1.y, px, py)
//            val (a2x, a2y) = intersectLineLineF(v3.x, v3.y, v1.x, v1.y, v2.x, v2.y, px, py)
//            val (a3x, a3y) = intersectLineLineF(v1.x, v1.y, v2.x, v2.y, v3.x, v3.y, px, py)

            g.setColor(colrRed)
            drawLine(v1.x, v1.y, a1x, a1y)
            drawLine(v2.x, v2.y, a2x, a2y)
            drawLine(v3.x, v3.y, a3x, a3y)
            g.setColor(colrGreen)
            val prj1 = projectPointOntoLineSegment(v1.x, v1.y, a1x, a1y, px, py)
            val prj2 = projectPointOntoLineSegment(v2.x, v2.y, a2x, a2y, px, py)
            val prj3 = projectPointOntoLineSegment(v3.x, v3.y, a3x, a3y, px, py)
            drawPoint(prj1.x, prj1.y)
            drawPoint(prj2.x, prj2.y)
            drawPoint(prj3.x, prj3.y)
//            val amp1 = math.sqrt(1 - prj1.loc).toFloat
//            val amp2 = math.sqrt(1 - prj2.loc).toFloat
//            val amp3 = math.sqrt(1 - prj3.loc).toFloat
//            println(f"$i1 - ${prj1.loc}%g, $i2 - ${prj2.loc}%g, $i3 - ${prj3.loc}%g")
            val amp1 = math.sqrt(1 - prj1.loc).toFloat
            val amp2 = math.sqrt(1 - prj2.loc).toFloat
            val amp3 = math.sqrt(1 - prj3.loc).toFloat
            g.setColor(colrBlue)
            drawCircle(v1.x, v1.y, amp1 * 24)
            drawCircle(v2.x, v2.y, amp2 * 24)
            drawCircle(v3.x, v3.y, amp3 * 24)
          }
        }
      }
    }

    val ggMakeVideo = Button("Export Video") {
      val dir = userHome / "Documents" / "temp" / "delaunay"
      if (!dir.exists()) dir.mkdir()
      val w   = view.peer.getWidth
      val wi  = w - padT
      val h   = view.peer.getHeight
      val hi  = h - padT
      val b = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
      val g = b.createGraphics()
      for (i <- 1 to 100) {
        val px = (i.linLin(1, 100, pad, pad + wi) + 0.5).toInt
        val py = (i.linLin(100, 1, pad, pad + hi) + 0.5).toInt
        view.ptMouse = new Point(px, py)
        view.paintComponent(g)
        ImageIO.write(b, "png", dir / s"frame-$i.png")
      }
    }

    new Frame {
      title     = "Delaunay Space"
      contents  = new BorderPanel {
        add(view, BorderPanel.Position.Center)
        if (videoOption) add(ggMakeVideo, BorderPanel.Position.South )
      }
      pack().centerOnScreen()
      open()

      override def closeOperation(): Unit = {
        if (exitOnClose) sys.exit()
        else {
          atomic { itx =>
            implicit val tx: Txn = Txn.wrap(itx)
            binOpt.get(itx).foreach(_.free())
          }
        }
      }
    }
  }
}
