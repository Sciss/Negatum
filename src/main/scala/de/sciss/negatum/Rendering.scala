/*
 *  Rendering.scala
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

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.processor.Processor

import scala.util.Try

object Rendering {
  sealed trait State[+A] {
    def isComplete: Boolean
  }
  case class Completed[A](value: Try[A]) extends State[A] {
    def isComplete = true
  }
  final case class Progress(amount: Double) extends State[Nothing] {
    def isComplete = false
  }

  val  Cancelled = Processor.Aborted
  type Cancelled = Processor.Aborted
}
trait Rendering[S <: Sys[S], A] extends Observable[S#Tx, Rendering.State[A]] with Disposable[S#Tx] {
  def state(implicit tx: S#Tx): Rendering.State[A]

  /** Like `react` but invokes the function immediately with the current state. */
  def reactNow(fun: S#Tx => Rendering.State[A] => Unit)(implicit tx: S#Tx): Disposable[S#Tx]

  /** Cancels the process and does not keep results. */
  def cancel()(implicit tx: S#Tx): Unit

  /** Stops process at the next possible moment, and return current results. */
  def stop  ()(implicit tx: S#Tx): Unit
}