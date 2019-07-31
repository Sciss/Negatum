/*
 *  Shapes.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum
package gui

import java.awt.geom.Path2D

object Shapes {
  def Category(p: Path2D): Unit = {
    p.moveTo(8.32f, 0.64f)
    p.lineTo(23.68f, 0.64f)
    p.lineTo(23.68f, 3.1999998f)
    p.lineTo(8.320001f, 3.1999998f)
    p.lineTo(8.32f, 0.64f)
    p.moveTo(5.7599998f, 5.7599998f)
    p.lineTo(26.24f, 5.7599998f)
    p.lineTo(26.24f, 8.32f)
    p.lineTo(5.76f, 8.32f)
    p.lineTo(5.7599998f, 5.7599998f)
    p.moveTo(3.1999998f, 10.879999f)
    p.lineTo(28.8f, 10.879999f)
    p.lineTo(28.8f, 31.359999f)
    p.lineTo(3.1999989f, 31.359999f)
    p.lineTo(3.1999998f, 10.879999f)
  }
}
