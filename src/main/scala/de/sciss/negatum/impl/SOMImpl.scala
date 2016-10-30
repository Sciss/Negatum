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

import de.sciss.lucre.event.impl.ConstObjImpl
import de.sciss.lucre.stm.{Copy, Elem, Obj, Sys}
import de.sciss.negatum.SOM.Config
import de.sciss.serial.{DataInput, DataOutput}

object SOMImpl {
  def apply[S <: Sys[S]](config: Config)(implicit tx: S#Tx): SOM[S] = {
    val id = tx.newID()
    new Impl(id)
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): SOM[S] = ???

  private final class Impl[S <: Sys[S]](val id: S#ID) extends SOM[S] with ConstObjImpl[S, Any] {
    def tpe: Obj.Type = SOM

    def add(key: Vec[Double], value: Obj[S])(implicit tx: S#Tx): Unit = ???

    protected def writeData(out: DataOutput): Unit = ???

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl(txOut.newID())
  }
}
