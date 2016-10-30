/*
 *  SOM.scala
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

import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.negatum.impl.{SOMImpl => Impl}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

import scala.collection.immutable.{IndexedSeq => Vec}

object SOM extends Obj.Type {
  final val typeID = 0x40002

  object Config {
    implicit object serializer extends ImmutableSerializer[Config] {
      private[this] final val SER_VERSION = 1

      def write(c: Config, out: DataOutput): Unit = {
        import c._
        out.writeByte(SER_VERSION)
        out.writeInt(extent)
        out.writeInt(gridStep)
        out.writeInt(maxNodes)
      }

      def read(in: DataInput): Config = {
        val ver = in.readByte()
        if (ver != SER_VERSION) sys.error(s"Unexpected serialization version ($ver) - expected ${SER_VERSION}")
        val extent    = in.readInt()
        val gridStep  = in.readInt()
        val maxNodes  = in.readInt()
        Config(extent = extent, gridStep = gridStep, maxNodes = maxNodes)
      }
    }
  }
  final case class Config(extent: Int = 256, gridStep: Int = 1, maxNodes: Int = 16384)

  def apply[S <: Sys[S]](config: Config)(implicit tx: S#Tx): SOM[S] = Impl(config)

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}
trait SOM[S <: Sys[S]] extends Obj[S] {
  // def config: SVMConfig

  def add(key: Vec[Double], value: Obj[S])(implicit tx: S#Tx): Unit
}