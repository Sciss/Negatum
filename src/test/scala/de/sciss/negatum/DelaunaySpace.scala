package de.sciss.negatum

import java.awt.{Color, RenderingHints}

import de.sciss.negatum.Delaunay.{TriangleIndex, Vector2}

import scala.swing.event.MouseMoved
import scala.swing.{Component, Dimension, Frame, Graphics2D, Point, Swing}

object DelaunaySpace {
  val even = Vector(
    Vector2(238.396f, 521.4281f),
    Vector2(161.6244f, 299.19452f),
    Vector2(224.25385f, 167.87468f),
    Vector2(832.36578f, 22.412703f),
    Vector2(1297.0359f, 10.290872f),
    Vector2(1299.0562f, 256.7681f),
    Vector2(1313.1982f, 717.39764f),
    Vector2(1317.2389f, 818.4129f),
    Vector2(458.60925f, 834.57538f),
    Vector2(684.88336f, 826.49414f),
    Vector2(905.09668f, 826.49414f),
    Vector2(1111.1677f, 820.43323f),
    Vector2(711.1474f, 145.65134f),
    Vector2(1119.249f, 472.94077f),
    Vector2(296.98483f, 733.56018f),
    Vector2(1305.1169f, 483.04227f),
    Vector2(1080.8633f, 10.290871f),
    Vector2(628.31488f, 52.717278f),
    Vector2(739.43152f, 737.60071f),
    Vector2(929.34027f, 479.00168f),
    Vector2(428.30466f, 119.38736f),
    Vector2(1485.2186f, 493.1438f),
    Vector2(1483.1171f, 817.96606f),
    Vector2(1559.1577f, 487.08289f)
  )

  val odd = Vector(
    Vector2(123.23861f, 190.09804f),
    Vector2(327.2894f, 147.67163f),
    Vector2(535.38086f, 91.103073f),
    Vector2(731.35046f, 28.473616f),
    Vector2(953.58405f, 22.412703f),
    Vector2(1183.8988f, 16.351786f),
    Vector2(1307.1375f, 121.40765f),
    Vector2(1305.1171f, 375.96609f),
    Vector2(272.74118f, 630.5246f),
    Vector2(339.41125f, 840.63635f),
    Vector2(565.68542f, 830.53473f),
    Vector2(791.95953f, 826.49414f),
    Vector2(327.2894f, 521.4281f),
    Vector2(735.39099f, 644.66675f),
    Vector2(1036.4165f, 476.98138f),
    Vector2(195.96959f, 406.27069f),
    Vector2(1010.1525f, 826.49414f),
    Vector2(1220.2642f, 822.45355f),
    Vector2(1309.1576f, 606.28088f),
    Vector2(1214.2034f, 464.85956f),
    Vector2(711.1474f, 250.7072f),
    Vector2(1405.2389f, 818.4129f),
    Vector2(1403.2186f, 493.1438f),
    Vector2(1559.1171f, 817.96606f)
  )

  def main(args: Array[String]): Unit = {
    Swing.onEDT(run())
  }

  def run(): Unit = {
    val select  = even
    val minX    = select.minBy(_.x).x
    val minY    = select.minBy(_.y).y
    val maxX    = select.maxBy(_.x).x
    val maxY    = select.maxBy(_.y).y
    val selectW = maxX - minX
    val selectH = maxY - minY
    val pad     = 16
    val padT    = pad << 1
    val prefW   = 800
    val prefH   = (prefW * selectH / selectW + 0.5).toInt
    val tri     = Delaunay(select)
    val triLn   = tri.flatMap { case TriangleIndex(a, b, c) =>
      List(
        (math.min(a, b), math.max(a, b)),
        (math.min(a, c), math.max(a, c)),
        (math.min(b, c), math.max(b, c))
      )
    } .distinct

    object view extends Component {
      preferredSize = new Dimension(prefW + padT, prefH + padT)
      opaque        = true

      private[this] var ptMouse = new Point(-1, -1)

      listenTo(mouse.moves)
      reactions += {
        case e: MouseMoved =>
          ptMouse = e.point
          repaint()
      }

      private[this] val colrGreen = new Color(0, 0xA0, 0)
      private[this] val colrRed   = Color.red

      override protected def paintComponent(g: Graphics2D): Unit = {
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

        select.foreach { v =>
          drawPoint(v.x, v.y)
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

        if (ptMouse.x >= 0) {
          val px = (ptMouse.x - pad) / scale + minX
          val py = (ptMouse.y - pad) / scale + minY
          // g.setColor(Color.red)
          // drawPoint(px, py)

          triLn.foreach { case (i1, i2) =>
            val v1    = select(i1)
            val v2    = select(i2)
            val dvx   = v2.x - v1.x
            val dvy   = v2.y - v1.y
            val dpx   = px - v1.x                   // 1 ADD
            val dpy   = py - v1.y                   // 1 ADD
            val dot   = dvx * dpx + dvy * dpy       // 1 ADD, 2 MUL
            val len   = dvx * dvx + dvy * dvy       // 1 ADD, 2 MUL
            val f     = dot / len                   // 1 MUL
            val prjX  = v1.x + dvx * f
            val prjY  = v1.y + dvy * f
            val inside = 0 <= f && f <= 1
            g.setColor(if (inside) colrGreen else colrRed)
            drawPoint(prjX, prjY)
          }
        }
      }
    }

    new Frame {
      title = "Delaunay Space"
      contents = view
      pack().centerOnScreen()
      open()

      override def closeOperation(): Unit = sys.exit()
    }
  }
}
