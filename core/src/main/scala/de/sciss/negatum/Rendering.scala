/*
 *  Rendering.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum

import de.sciss.lucre.Observable
import de.sciss.lucre.{Disposable, Txn}
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

  val  Cancelled: Processor.Aborted.type  = Processor.Aborted
  type Cancelled                          = Processor.Aborted
}
trait Rendering[T <: Txn[T], A] extends Observable[T, Rendering.State[A]] with Disposable[T] {
  def state(implicit tx: T): Rendering.State[A]

  /** Like `react` but invokes the function immediately with the current state. */
  def reactNow(fun: T => Rendering.State[A] => Unit)(implicit tx: T): Disposable[T]

  /** Cancels the process and does not keep results. */
  def cancel()(implicit tx: T): Unit

  /** Stops process at the next possible moment, and return current results. */
  def stop  ()(implicit tx: T): Unit
}