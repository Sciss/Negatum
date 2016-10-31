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
import de.sciss.lucre.event.{Dummy, Event, EventLike}
import de.sciss.lucre.event.impl.ConstObjImpl
import de.sciss.lucre.geom.{IntHyperCubeN, IntPoint2D, IntPoint3D, IntPointN, IntSpace, Space}
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.negatum.SOM.Config
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer}

import scala.language.existentials

object SOMImpl {
  //  private object PlacedWeightLike {
//    implicit def view[S <: Sys[S]]: (PlacedWeightLike, S#Tx) => IntPointN = (p, tx) => p.weight
//  }
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
  private final case class PlacedWeightN (weight: IntPointN, coord: IntPointN) extends PlacedWeightLike

  private object PlacedNode2D {
    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, PlacedNode2D[S]] = anySer.asInstanceOf[Ser[S]]

    implicit def view[S <: Sys[S]]: (PlacedNode2D[S], S#Tx) => IntPoint2D = (p, tx) => p.coord

    private[this] val anySer = new Ser[NoSys]

    private final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, PlacedNode2D[S]] {
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): PlacedNode2D[S] = {
        val coord = IntSpace.TwoDim.pointSerializer.read(in)
        val obj   = Obj.read(in, access)
        PlacedNode2D(coord = coord, obj = obj)
      }

      def write(p: PlacedNode2D[S], out: DataOutput): Unit = {
        IntSpace.TwoDim.pointSerializer.write(p.coord, out)
        p.obj.write(out)
      }
    }
  }
  private final case class PlacedNode2D[S <: Sys[S]](coord: IntPoint2D, obj: Obj[S])

  private object PlacedNode3D {
    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, PlacedNode3D[S]] = anySer.asInstanceOf[Ser[S]]

    implicit def view[S <: Sys[S]]: (PlacedNode3D[S], S#Tx) => IntPoint3D = (p, tx) => p.coord

    private[this] val anySer = new Ser[NoSys]

    private final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, PlacedNode3D[S]] {
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): PlacedNode3D[S] = {
        val coord = IntSpace.ThreeDim.pointSerializer.read(in)
        val obj   = Obj.read(in, access)
        PlacedNode3D(coord = coord, obj = obj)
      }

      def write(p: PlacedNode3D[S], out: DataOutput): Unit = {
        IntSpace.ThreeDim.pointSerializer.write(p.coord, out)
        p.obj.write(out)
      }
    }
  }
  private final case class PlacedNode3D[S <: Sys[S]](coord: IntPoint3D, obj: Obj[S])

  def apply[S <: Sys[S]](config: Config)(implicit tx: S#Tx): SOM[S] = {
    import config.{dimensions => dim, _}

    val random = new util.Random(seed)

    def rndWeight(): IntPointN = {
      val components = Vector.fill(features)(random.nextInt() & 0x7FFFFFFF)
      IntPointN(components)
    }

    implicit val space      = IntSpace.NDim(features)
    val quad                = IntHyperCubeN.apply(Vector.fill(features)(0x40000000), extent = 0x40000000)
    implicit val pointView  = (p: PlacedWeightLike, tx: S#Tx) => p.weight
    val lattice             = if (dim == 2) {
      val res = SkipOctree.empty[S, IntSpace.NDim, PlacedWeight2D](quad)
      for {
        x <- 0 until (extent << 1) by gridStep
        y <- 0 until (extent << 1) by gridStep
      } {
        val c = IntPoint2D(x, y)
        val w = rndWeight()
        val n = PlacedWeight2D(weight = w, coord = c)
        res.add(n)
      }
      res
    } else if (dim == 3) {
      val res = SkipOctree.empty[S, IntSpace.NDim, PlacedWeight3D](quad)
      for {
        x <- 0 until (extent << 1) by gridStep
        y <- 0 until (extent << 1) by gridStep
        z <- 0 until (extent << 1) by gridStep
      } {
        val c = IntPoint3D(x, y, z)
        val w = rndWeight()
        val n = PlacedWeight3D(weight = w, coord = c)
        res.add(n)
      }
      res
    } else {
      val res = SkipOctree.empty[S, IntSpace.NDim, PlacedWeightN](quad)
      ???
      res
    }
    new Impl(config, lattice = lattice, map = ???)
  }

  private final val COOKIE = 0x736f6d00

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, SOM[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, SOM[S]] {
    def tpe: Obj.Type = SOM
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): SOM[S] = {
    val cookie = in.readInt()
    if (cookie != COOKIE) sys.error(s"Unexpected cookie ${cookie.toHexString} -- expected ${COOKIE.toHexString}")
    val config  = Config.serializer.read(in)
    import config.{features, dimensions => dim}
    implicit val space      = IntSpace.NDim(features)
    implicit val pointView  = (p: PlacedWeightLike, tx: S#Tx) => p.weight
    if (dim == 2) {
      val lattice = SkipOctree.read[S, IntSpace.NDim  , PlacedWeight2D] (in, access)
      val map     = SkipOctree.read[S, IntSpace.TwoDim, PlacedNode2D[S]](in, access)
      new Impl(config, lattice = lattice, map = map)
    } else if (dim == 3) {
      val lattice = SkipOctree.read[S, IntSpace.NDim    , PlacedWeight3D](in, access)
      val map     = SkipOctree.read[S, IntSpace.ThreeDim, PlacedNode3D[S]](in, access)
      new Impl(config, lattice = lattice, map = map)
    } else {
      val lattice = SkipOctree.read[S, IntSpace.NDim, PlacedWeightN](in, access)
      new Impl(config, lattice = lattice, map = ???)
    }
  }

  private final class Impl[S <: Sys[S], D <: Space[D], W, Z](val config: Config,
                                           lattice: SkipOctree[S, IntSpace.NDim, W],
                                           map: SkipOctree[S, D, Z])
    extends SOM[S] {

    def id: S#ID = lattice.id

    def tpe: Obj.Type = SOM

    def add(key: Vec[Double], value: Obj[S])(implicit tx: S#Tx): Unit = ???

    def event(slot: Int): Event[S, Any] = throw new UnsupportedOperationException

    final def changed: EventLike[S, Any] = Dummy[S, Any]

    def write(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      Config.serializer.write(config, out)
      lattice.write(out)
      map    .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      lattice.dispose()
      map    .dispose()
    }

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl(config = config, lattice = ???, map = ???)
  }
}
