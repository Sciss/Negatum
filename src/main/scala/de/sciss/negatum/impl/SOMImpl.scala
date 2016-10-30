/*
 *  SOMImpl.scala
 *  (SVMModel)
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
package impl

import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.event.impl.ConstObjImpl
import de.sciss.lucre.geom.{IntHyperCubeN, IntPoint2D, IntPoint3D, IntPointN, IntSpace, Space}
import de.sciss.lucre.stm.{Copy, Elem, Obj, Sys}
import de.sciss.negatum.SOM.Config
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

import scala.language.existentials

object SOMImpl {
  private sealed trait PlacedWeightLike {
    def weight: IntPointN
  }
  private object PlacedWeight2D {
    implicit object serializer extends ImmutableSerializer[PlacedWeight2D] {
      def write(p: PlacedWeight2D, out: DataOutput): Unit = {
        IntSpace.NDim  .pointSerializer.write(p.weight, out)
        IntSpace.TwoDim.pointSerializer.write(p.coord , out)
      }

      def read(in: DataInput): PlacedWeight2D = {
        val weight = IntSpace.NDim  .pointSerializer.read(in)
        val coord  = IntSpace.TwoDim.pointSerializer.read(in)
        PlacedWeight2D(weight = weight, coord = coord)
      }
    }
  }
  private final case class PlacedWeight2D(weight: IntPointN, coord: IntPoint2D) extends PlacedWeightLike

  private object PlacedWeight3D {
    implicit object serializer extends ImmutableSerializer[PlacedWeight3D] {
      def write(p: PlacedWeight3D, out: DataOutput): Unit = {
        IntSpace.NDim    .pointSerializer.write(p.weight, out)
        IntSpace.ThreeDim.pointSerializer.write(p.coord , out)
      }

      def read(in: DataInput): PlacedWeight3D = {
        val weight = IntSpace.NDim    .pointSerializer.read(in)
        val coord  = IntSpace.ThreeDim.pointSerializer.read(in)
        PlacedWeight3D(weight = weight, coord = coord)
      }
    }
  }
  private final case class PlacedWeight3D(weight: IntPointN, coord: IntPoint3D) extends PlacedWeightLike

  private object PlacedWeightN {
    implicit object serializer extends ImmutableSerializer[PlacedWeightN] {
      def write(p: PlacedWeightN, out: DataOutput): Unit = {
        val ser = IntSpace.NDim.pointSerializer
        ser.write(p.weight, out)
        ser.write(p.coord , out)
      }

      def read(in: DataInput): PlacedWeightN = {
        val ser = IntSpace.NDim.pointSerializer
        val weight = ser.read(in)
        val coord  = ser.read(in)
        PlacedWeightN(weight = weight, coord = coord)
      }
    }
  }
  private final case class PlacedWeightN (weight: IntPointN, coord: IntPointN ) extends PlacedWeightLike

  def apply[S <: Sys[S]](config: Config)(implicit tx: S#Tx): SOM[S] = {
    val id = tx.newID()

    import config.{features, dimensions => dim}
    implicit val space      = IntSpace.NDim(features)
    val quad                = IntHyperCubeN.apply(Vector.fill(features)(0x40000000), extent = 0x40000000)
    implicit val pointView  = (p: PlacedWeightLike, tx: S#Tx) => p.weight
    val lattice             = if (dim == 2)
      SkipOctree.empty[S, IntSpace.NDim, PlacedWeight2D](quad)
    else if (dim == 3)
      SkipOctree.empty[S, IntSpace.NDim, PlacedWeight3D](quad)
    else
      SkipOctree.empty[S, IntSpace.NDim, PlacedWeightN ](quad)

    new Impl(id, config, lattice)
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): SOM[S] = ???

  private final class Impl[S <: Sys[S], W](val id: S#ID, val config: Config, som: SkipOctree[S, IntSpace.NDim, W])
    extends SOM[S] with ConstObjImpl[S, Any] {

    def tpe: Obj.Type = SOM

    def add(key: Vec[Double], value: Obj[S])(implicit tx: S#Tx): Unit = ???

    protected def writeData(out: DataOutput): Unit = ???

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl(txOut.newID(), config = config, som = ???)
  }
}
