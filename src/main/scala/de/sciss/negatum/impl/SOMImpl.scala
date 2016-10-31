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
import de.sciss.lucre.geom.{IntCube, IntHyperCubeN, IntPoint2D, IntPoint3D, IntPointN, IntSpace, IntSquare, Space}
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.negatum.SOM.Config
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer}
import IntSpace.{TwoDim, ThreeDim}

import scala.language.existentials

object SOMImpl {
  private object Node {
    implicit def serializer[S <: Sys[S], D <: Space[D]](implicit space: D): Serializer[S#Tx, S#Acc, Node[S, D]] =
      new Ser[S, D]

    private final class Ser[S <: Sys[S], D <: Space[D]](implicit space: D)
      extends Serializer[S#Tx, S#Acc, Node[S, D]] {

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Node[S, D] = {
        val key   = space.pointSerializer.read(in)
        val value = Obj.read(in, access)
        Node(key = key, value = value)
      }

      def write(p: Node[S, D], out: DataOutput): Unit = {
        space.pointSerializer.write(p.key, out)
        p.value.write(out)
      }
    }

    implicit def view[S <: Sys[S], D <: Space[D]]: (Node[S, D], S#Tx) => D#PointLike = (p, tx) => p.key
  }
  private final case class Node[S <: Sys[S], D <: Space[D]](key: D#Point, value: Obj[S])

  private implicit object LatticeSer extends ImmutableSerializer[Array[Float]] {
    def read(in: DataInput): Array[Float] = {
      val sz  = in.readInt()
      val arr = new Array[Float](sz)
      var i   = 0
      while (i < sz) {
        arr(i) = in.readFloat()
        i += 1
      }
      arr
    }

    def write(arr: Array[Float], out: DataOutput): Unit = {
      val sz = arr.length
      out.writeInt(sz)
      var i = 0
      while (i < sz) {
        out.writeFloat(arr(i))
        i += 1
      }
    }
  }

  def apply[S <: Sys[S]](config: Config)(implicit tx: S#Tx): SOM[S] = {
    import config.{dimensions => dim, _}

    val random = new util.Random(seed)

    require (dim > 0 && extent > 1 && gridStep > 0 && gridStep <= extent)
    require (extent < (0x40000000 >> (dim - 1)), "Integer overflow")
    val numLatSide  = (extent << 1) / gridStep
    val numLattice  = numLatSide << (dim - 1)
    val lattice0    = new Array[Float](numLattice)
    var i = 0
    while (i < numLattice) {
      lattice0(i) = random.nextFloat()
      i += 1
    }
    val id          = tx.newID()
    val lattice     = tx.newVar[Array[Float]](id, lattice0)

    if (dim == 2) {
      val quad = IntSquare(extent, extent, extent)
      val map = SkipOctree.empty[S, TwoDim, Node[S, TwoDim]](quad)
      new Impl(id, config, lattice = lattice, map = map)
    } else if (dim == 3) {
      val cube = IntCube(extent, extent, extent, extent)
      val map  = SkipOctree.empty[S, ThreeDim, Node[S, ThreeDim]](cube)
      new Impl(id, config, lattice = lattice, map = map)
    } else {
      ???
    }
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
    val id      = tx.readID(in, access)
    val config  = Config.serializer.read(in)
    val lattice = tx.readVar[Array[Float]](id, in)

    import config.{features, dimensions => dim}
    if (dim == 2) {
      val map     = SkipOctree.read[S, TwoDim, Node[S, TwoDim]](in, access)
      new Impl(id, config, lattice = lattice, map = map)
    } else if (dim == 3) {
      val map     = SkipOctree.read[S, ThreeDim, Node[S, ThreeDim]](in, access)
      new Impl(id, config, lattice = lattice, map = map)
    } else {
      ???
    }
  }

  private type TreeImpl[S <: Sys[S], D <: Space[D]] = SkipOctree[S, D, Node[S, D]]

  private def copyTree[In <: Sys[In], Out <: Sys[Out], D <: Space[D]](
       in: TreeImpl[In, D], out: TreeImpl[Out, D], outImpl: Impl[Out, D])
      (implicit txIn: In#Tx, txOut: Out#Tx, context: Copy[In, Out]): Unit = {

    in.iterator.foreach { nodeIn =>
      val valueOut = context(nodeIn.value)
      val nodeOut  = Node(nodeIn.key, valueOut)
      out.add(nodeOut)
//      xsOut.foreach { entry =>
//        outImpl.changed += entry
//      }
    }
  }

  private final class Impl[S <: Sys[S], D <: Space[D]](val id: S#ID, val config: Config,
                                                       lattice: S#Var[Array[Float]], map: TreeImpl[S, D])
                                                      (implicit space: D)
    extends SOM[S] {

    def tpe: Obj.Type = SOM

    def add(key: Vec[Double], value: Obj[S])(implicit tx: S#Tx): Unit = ???

    def event(slot: Int): Event[S, Any] = throw new UnsupportedOperationException

    final def changed: EventLike[S, Any] = Dummy[S, Any]

    def write(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      id     .write(out)
      Config.serializer.write(config, out)
      lattice.write(out)
      map    .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id     .dispose()
      lattice.dispose()
      map    .dispose()
    }

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val idOut       = txOut.newID()
      val latticeOut  = txOut.newVar(idOut, lattice())
      val mapOut      = SkipOctree.empty[Out, D, Node[Out, D]](map.hyperCube)
      val out         = new Impl(id = idOut, config = config, lattice = latticeOut, map = mapOut)
      context.defer(this, out)(copyTree(map, mapOut, out))
      out
    }
  }
}