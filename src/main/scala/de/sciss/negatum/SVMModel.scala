/*
 *  SVMModel.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.negatum.impl.{SVMModelImpl => Impl}
import de.sciss.processor.Processor
import de.sciss.serial.DataInput

import scala.collection.immutable.{Seq => ISeq}

object SVMModel extends Obj.Type {
  final val typeID = 0x40001

  def train[S <: Sys[S]](n: ISeq[Negatum[S]], config: SVMConfig, numCoeff: Int = 24)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): Processor[SVMModel[S]] =
    Impl.train(n, config, numCoeff = numCoeff)

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}
trait SVMModel[S <: Sys[S]] extends Obj[S] {
  def config: SVMConfig
}