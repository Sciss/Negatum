/*
 *  Speakers.scala
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

import de.sciss.negatum.Delaunay.{TriangleIndex, Vector2}
import de.sciss.numbers
import de.sciss.synth.GE

import scala.Predef.{any2stringadd => _}
import scala.collection.immutable.{IndexedSeq => Vec}

object Speakers {
  val even: Vec[Vector2] = Vector(
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

  val odd: Vec[Vector2] = Vector(
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

  /** Speakers selected for the projection. */
  val select : Vec[Vector2]        = even
  /** Delaunay triangulation indices. */
  val tri    : Vec[TriangleIndex]  = Delaunay(select)

  /** Minimum horizontal coordinate in `select`. */
  val minX: Float = select.minBy(_.x).x
  /** Minimum vertical coordinate in `select`. */
  val minY: Float = select.minBy(_.y).y
  /** Maximum horizontal coordinate in `select`. */
  val maxX: Float = select.maxBy(_.x).x
  /** Maximum vertical coordinate in `select`. */
  val maxY: Float = select.maxBy(_.y).y

  /** Selected speakers with coordinates normalized to (0, 1) */
  val selectN: Vec[Vector2] = select.map { case Vector2(x, y) =>
    import numbers.Implicits._
    Vector2(x.linlin(minX, maxX, 0, 1), y.linlin(minY, maxY, 0, 1))
  }

  final case class Proj(x: Float, y: Float, loc: Float) {
    def inside: Boolean = 0 <= loc && loc <= 1
  }

  final case class ProjGE(x: GE, y: GE, loc: GE) {
    def inside: GE = 0 <= loc & loc <= 1
  }

  /** Altitude projections for each triangle. */
  val altitudeProjections: Vec[(Proj, Proj, Proj)] = tri.map { case TriangleIndex(i1, i2, i3) =>
    val v1    = select(i1)
    val v2    = select(i2)
    val v3    = select(i3)
    val alt1  = projectPointOntoLineSegment(v2.x, v2.y, v3.x, v3.y, v1.x, v1.y)
    val alt2  = projectPointOntoLineSegment(v3.x, v3.y, v1.x, v1.y, v2.x, v2.y)
    val alt3  = projectPointOntoLineSegment(v1.x, v1.y, v2.x, v2.y, v3.x, v3.y)
    (alt1, alt2, alt3)
  }

  /** Altitude projections for each triangle. */
  val altitudeProjectionsN: Vec[(Proj, Proj, Proj)] = tri.map { case TriangleIndex(i1, i2, i3) =>
    val v1    = selectN(i1)
    val v2    = selectN(i2)
    val v3    = selectN(i3)
    val alt1  = projectPointOntoLineSegment(v2.x, v2.y, v3.x, v3.y, v1.x, v1.y)
    val alt2  = projectPointOntoLineSegment(v3.x, v3.y, v1.x, v1.y, v2.x, v2.y)
    val alt3  = projectPointOntoLineSegment(v1.x, v1.y, v2.x, v2.y, v3.x, v3.y)
    (alt1, alt2, alt3)
  }

  def projectPointOntoLineSegment(v1x: Float, v1y: Float, v2x: Float, v2y: Float, px: Float, py: Float): Proj = {
    val dvx   = v2x - v1x
    val dvy   = v2y - v1y
    val dpx   = px - v1x
    val dpy   = py - v1y
    val dot   = dvx * dpx + dvy * dpy
    val len   = dvx * dvx + dvy * dvy
    val f     = dot / len
    val prjX  = v1x + dvx * f
    val prjY  = v1y + dvy * f
    Proj(x = prjX, y = prjY, loc = f)
  }

  def projectPointOntoLineSegmentGE(v1x: Float, v1y: Float, v2x: Float, v2y: Float, px: GE, py: GE): ProjGE = {
    val dvx   = v2x - v1x
    val dvy   = v2y - v1y
    val dpx   = px - v1x
    val dpy   = py - v1y
    val dot   = dvx * dpx + dvy * dpy
    val len   = dvx * dvx + dvy * dvy
    val f     = dot / len
    val prjX  = v1x + dvx * f
    val prjY  = v1y + dvy * f
    ProjGE(x = prjX, y = prjY, loc = f)
  }

  def projectPointLineLoc(v1x: Float, v1y: Float, v2x: Float, v2y: Float, px: GE, py: GE): GE = {
    val dvx   = v2x - v1x
    val dvy   = v2y - v1y
    val dpx   = px - v1x
    val dpy   = py - v1y
    val dot   = dvx * dpx + dvy * dpy
    val len   = dvx * dvx + dvy * dvy
    val f     = dot / len
//    val prjX  = v1x + dvx * f
//    val prjY  = v1y + dvy * f
    f
  }
}