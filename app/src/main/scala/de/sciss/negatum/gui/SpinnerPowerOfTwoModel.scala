/*
 *  SpinnerPowerOfTwoModel.scala
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
package gui

import javax.swing.SpinnerNumberModel

/** Overrides step behaviour */
class SpinnerPowerOfTwoModel(value: Int, minimum: Int, maximum: Int)
  extends SpinnerNumberModel(value, minimum, maximum, 1) {

  private def update(incr: Boolean): Number = {
    val value = getNumber
    val v0    = value.longValue
    val v     = if (incr) v0 << 1 else v0 >> 1
    val newValue: Number =
      if      (value.isInstanceOf[java.lang.Long   ]) java.lang.Long   .valueOf(v)
      else if (value.isInstanceOf[java.lang.Integer]) java.lang.Integer.valueOf(v.toInt)
      else if (value.isInstanceOf[java.lang.Short  ]) java.lang.Short  .valueOf(v.toShort)
      else                                            java.lang.Byte   .valueOf(v.toByte)

    // XXX TODO --- don't know how to do this better.
    // `Comparable` was retroactively generified in Java,
    // now we have `Comparable[_]` here, so in order to
    // run `compareTo` we need to cast them to something arbitrary?
    val maximum = getMaximum.asInstanceOf[Comparable[AnyRef]]
    val minimum = getMinimum.asInstanceOf[Comparable[AnyRef]]
    if (((maximum != null) && (maximum.compareTo(newValue) < 0)) ||
        ((minimum != null) && (minimum.compareTo(newValue) > 0))) null
    else newValue
  }

  override def getNextValue     : AnyRef = update(incr = true )
  override def getPreviousValue : AnyRef = update(incr = false)
}
