/*
 *  Edge.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum
package impl

import de.sciss.topology.Topology

/** The edge points from '''consuming''' (source) element to '''input''' element (target).
  * Therefore, the `sourceVertex`'s `inlet` will be occupied by `targetVertex`
  */
case class Edge(sourceVertex: Vertex, targetVertex: Vertex, inlet: String) extends Topology.Edge[Vertex]
