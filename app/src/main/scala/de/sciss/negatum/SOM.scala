/*
 *  SOM.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum

import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.geom.Space
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Folder, Obj, Sys}
import de.sciss.negatum.SOM.Node
import de.sciss.negatum.impl.{SOMImpl => Impl}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec, Seq => ISeq}
import scala.language.implicitConversions

object SOM extends Obj.Type {
  final val typeId = 0x40002

  def apply[S <: Sys[S]](config: Config)(implicit tx: S#Tx): SOM[S] = Impl(config)

  trait ConfigLike {
    def features      : Int
    def dimensions    : Int
    def extent        : Int
    def gridStep      : Int
//    def maxNodes      : Int
    def numIterations : Int
    def learningCoef  : Double
    def seed          : Long
  }

  object Config {
    def apply(): ConfigBuilder = new ConfigBuilder

    implicit def build(b: ConfigBuilder): Config = b.build

    implicit object serializer extends ImmutableSerializer[Config] {
      private[this] final val SER_VERSION = 1

      def write(c: Config, out: DataOutput): Unit = {
        import c._
        out.writeByte(SER_VERSION)
        out.writeInt(features     )
        out.writeInt(dimensions   )
        out.writeInt(extent       )
        out.writeInt(gridStep     )
//        out.writeInt(maxNodes     )
        out.writeInt(numIterations)
        out.writeDouble(learningCoef)
        out.writeLong(seed)
      }

      def read(in: DataInput): Config = {
        val ver           = in.readByte()
        if (ver != SER_VERSION) sys.error(s"Unexpected serialization version ($ver) - expected $SER_VERSION")
        val features      = in.readInt()
        val dimensions    = in.readInt()
        val extent        = in.readInt()
        val gridStep      = in.readInt()
//        val maxNodes      = in.readInt()
        val numIterations = in.readInt()
        val learningCoef  = in.readDouble()
        val seed          = in.readLong()
        ConfigImpl(features = features, dimensions = dimensions, extent = extent, gridStep = gridStep,
//          maxNodes = maxNodes,
          numIterations = numIterations, learningCoef = learningCoef, seed = seed)
      }
    }
  }
  sealed trait Config extends ConfigLike

  private final case class ConfigImpl(
                          features      : Int,
                          dimensions    : Int,
                          extent        : Int,
                          gridStep      : Int,
//                          maxNodes      : Int     = 16384,
                          numIterations : Int,
                          learningCoef  : Double,
                          seed          : Long
                         )
    extends Config

  final class ConfigBuilder extends ConfigLike {
    var features      : Int     = 48
    var dimensions    : Int     = 2
    var extent        : Int     = 256
    var gridStep      : Int     = 1
//    var maxNodes      : Int     = 16384
    var numIterations : Int     = 32768
    var learningCoef  : Double  = 0.072
    var seed          : Long    = System.currentTimeMillis()

    def build: Config = ConfigImpl(
      features = features, dimensions = dimensions, extent = extent, gridStep = gridStep,
//      maxNodes = maxNodes,
      numIterations = numIterations, learningCoef = learningCoef,
      seed = seed
    )
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, SOM[S]] = Impl.serializer[S]

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  // --- Node ---

  object Node {
    implicit def serializer[S <: Sys[S], D <: Space[D]](implicit space: D): Serializer[S#Tx, S#Acc, Node[S, D]] =
      new Ser[S, D]

    private final class Ser[S <: Sys[S], D <: Space[D]](implicit space: D)
      extends Serializer[S#Tx, S#Acc, Node[S, D]] {

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Node[S, D] = {
        // val index = in.readInt()
        val key   = space.pointSerializer.read(in)
        val value = Obj.read(in, access)
        Node(/* index = index, */ key = key, value = value)
      }

      def write(p: Node[S, D], out: DataOutput): Unit = {
        // out.writeInt(p.index)
        space.pointSerializer.write(p.key, out)
        p.value.write(out)
      }
    }

    implicit def view[S <: Sys[S], D <: Space[D]]: (Node[S, D], S#Tx) => D#PointLike = (p, _ /* tx */) => p.key
  }
  final case class Node[S <: Sys[S], D <: Space[D]](/* index: Int, */ key: D#Point, value: Obj[S])
}
trait SOM[S <: Sys[S]] extends Obj[S] {
  def config: SOM.Config

  def add(key: Vec[Double], value: Obj[S])(implicit tx: S#Tx): Unit

  /** Searches in the map for the object closest to a given point.
    *
    * @param point    the point in the map (i.e. with each axis
    *                 between zero and `config.extent * 2`.
    * @return if the map is not empty, the closest element together with
    *         its actual map location.
    */
  def query(point: Seq[Int])(implicit tx: S#Tx): Option[(ISeq[Int], Obj[S])]

  def iterator(implicit tx: S#Tx): Iterator[(ISeq[Int], Obj[S])]

  def debugStats()(implicit tx: S#Tx): String

  /** Asynchronously adds all elements in the folder for which the `attrFeatures` attribute is set.
    *
    * @param selected   if `true`, only adds elements of the folder, for which the `attrSelected`
    *                   attribute is set to `true`.
    */
  def addAll(f: Folder[S], selected: Boolean)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Rendering[S, Int]

  /** Current iteration */
  def iteration(implicit tx: S#Tx): Int

  /** Number of objects in the map */
  def size(implicit tx: S#Tx): Int

  type D <: Space[D]

  def space: D

  def tree: SkipOctree[S, D, Node[S, D]]
}