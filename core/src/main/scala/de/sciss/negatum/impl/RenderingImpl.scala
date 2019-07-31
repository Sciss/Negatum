/*
 *  RenderingImpl.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum
package impl

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.negatum.Rendering.State
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl

import scala.concurrent.ExecutionContext
import scala.concurrent.stm.Ref
import scala.util.{Failure, Success, Try}

trait RenderingImpl[S <: Sys[S], A, B]
  extends ProcessorImpl[B, Processor[B]]
  with Processor[B]
  with Rendering[S, A]
  with ObservableImpl[S, State[A]] {

  // ---- abstract ----

  protected def cursor: stm.Cursor[S]

  protected def fillResult(out: B)(implicit tx: S#Tx): A

  // ---- impl ----

  private[this] val _state        = Ref[State[A]](Rendering.Progress(0.0))
  private[this] val _disposed     = Ref(false)

  final def reactNow(fun: S#Tx => State[A] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
    val res = react(fun)
    fun(tx)(state)
    res
  }

  protected def completeWith(t: Try[B]): Unit = {
    if (!_disposed.single.get) {
      cursor.step { implicit tx =>
        if (!_disposed.get(tx.peer)) t match {
          case Success(out) =>
            val res = fillResult(out)
            state = Rendering.Completed(Success(res))
          case Failure(ex) =>
            state = Rendering.Completed(Failure(ex))  // bad design: Failure is not Try[Nothing]
        }
      }
    }
  }

  private def progressTx(amt: Double): Unit = if (!_disposed.single.get)
    cursor.step { implicit tx =>
      state = Rendering.Progress(amt)
    }

  def startTx()(implicit tx: S#Tx): Unit = {
    tx.afterCommit {
      addListener {
        case Processor.Progress(_, d)   => progressTx(d)
        case Processor.Result(_, value) => completeWith(value)
      }
      // NB: bad design in `ProcessorImpl`; because we're in the sub-class,
      // we have implicit execution context in scope, but that's the one
      // we want to _set_ here.
      start()(ExecutionContext.Implicits.global)
    }
  }

  final def state(implicit tx: S#Tx): State[A] =
    _state.get(tx.peer)

  protected final def state_=(value: State[A])(implicit tx: S#Tx): Unit = {
    val old = _state.swap(value)(tx.peer)
    if (old != value) fire(value)
  }

  def cancel ()(implicit tx: S#Tx): Unit = tx.afterCommit(abort())
  def stop   ()(implicit tx: S#Tx): Unit = cancel()
  def dispose()(implicit tx: S#Tx): Unit = cancel()
}