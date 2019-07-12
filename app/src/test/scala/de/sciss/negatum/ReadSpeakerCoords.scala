package de.sciss.negatum

import de.sciss.file._

import scala.xml.XML

object ReadSpeakerCoords extends App {
  val baseDir = userHome / "Documents" / "projects" / "Imperfect" / "modell_photos"
  val path    = baseDir / "model_with_speakers_negatum.svg"
  val root    = XML.load(path.toURI.toURL)
  val nText   = root \\ "text"
  val nRect   = root \\ "rect"
  require(nText.size == 48, s"Expecting 48 text nodes, but found ${nText.size}")
  require(nRect.size == 48, s"Expecting 48 rect nodes, but found ${nRect.size}")

  case class Text(x: Float, y: Float, ch: Int)
  case class Rect(x: Float, y: Float, ch: Int, even: Boolean)

  val RegCh = """\d+, (\d+)""".r

  val texts = nText.map { n =>
    val x  = (n \ "@x").text.toFloat
    val y  = (n \ "@y").text.toFloat
    val s  = (n \ "tspan").text
    val RegCh(chS) = s
    val ch = chS.toInt - 1
    Text(x = x, y = y, ch = ch)
  }
  val chansFound = texts.map(_.ch).sorted
  require(chansFound == (0 until 48), s"Expected 0 to 47 chan indices. Found $chansFound")

  val rects = nRect.map { n =>
    val x = (n \ "@x").text.toFloat
    val y = (n \ "@y").text.toFloat
    val style = (n \ "@style").text
    val isEven = style.contains("fill:#00ab60")
    if (!isEven) require(style.contains("fill:#0084ab"))

    val text = texts.minBy { case Text(xt, yt, _) =>
      val dx = xt - x
      val dy = yt - y
      dx * dx + dy * dy
    }
    Rect(x = x, y = y, ch = text.ch, even = isEven)
  }
  val chansFound2 = rects.map(_.ch).sorted
  require(chansFound2 == (0 until 48), s"Expected 0 to 47 chan indices. Found $chansFound2")

  val numEven = rects.count(_.even)
  require(numEven == 24, s"Expected same number of green and blue channels (but $numEven are green)")

  val minX    = rects.minBy(_.x).x
  val minY    = rects.minBy(_.y).y
  val rectsN  = rects.map(r => r.copy(x = r.x - minX, y = r.y - minY))
  val rectsS  = rectsN.sortBy(_.ch)

  def mkVector(even: Boolean): String = {
    val select = rectsS.filter(_.even == even)
    val mapped = select.map { case Rect(x, y, _ /*ch*/, _) =>
      f"Vector2($x%gf, $y%gf)"
    }
    val pre = if (even) "even" else "odd"
    val head = s"  val $pre: Vec[Vector2] = Vector(\n    "
    mapped.mkString(head, ",\n    ", "\n  )")
  }

  println(mkVector(even = true))
  println()
  println(mkVector(even = false))
}
