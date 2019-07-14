/*
 *  Edge.scala
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

import de.sciss.topology

/** The edge points from '''consuming''' (source) element to '''input''' element (target).
  * Therefore, the `sourceVertex`'s `inlet` will be occupied by `targetVertex`
  */
case class Edge(sourceVertex: Vertex, targetVertex: Vertex, inlet: String) extends topology.Edge[Vertex]
