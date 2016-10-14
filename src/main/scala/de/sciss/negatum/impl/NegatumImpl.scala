/*
 *  NegatumImpl.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum
package impl

import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.DoubleObj
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.lucre.{stm, event => evt}
import de.sciss.negatum.Negatum.{Config, Rendering}
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.proc.{AudioCue, Folder, SynthGraphObj, WorkspaceHandle}

object NegatumImpl {
  private final val SER_VERSION = 0x4e56  // "Ne"

  def apply[S <: Sys[S]](template: AudioCue.Obj[S])(implicit tx: S#Tx): Negatum[S] = new New[S](template)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Negatum[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Negatum[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, Negatum[S]] {
    def tpe: Obj.Type = Negatum
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Negatum[S] = {
    val targets = Targets.read(in, access)
    new Read(in, access, targets)
  }

  // ----

  private sealed trait Impl[S <: Sys[S]]
    extends Negatum[S] with evt.impl.SingleNode[S, Negatum.Update[S]] {
    proc =>

    final def tpe: Obj.Type = Negatum

    // --- rendering ---

    final def run(config: Config, iter: Int)(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                             workspace: WorkspaceHandle[S]): Rendering[S] = {
      val popIn = population.iterator.collect {
        case gObj: SynthGraphObj[S] =>
          val g         = gObj.value
          val attr      = gObj.attr
          val fitness   = attr.$[DoubleObj ](Negatum.attrFitness ).map(_.value).getOrElse(Double.NaN)
//          val selected  = attr.$[BooleanObj](Negatum.attrSelected).exists(_.value)
          new Individual(g, fitness = fitness)
      } .toIndexedSeq
      val templateV   = template.value
      val populationH = tx.newHandle(population)
      val r = new RenderingImpl[S](config = config, template = templateV, popIn = popIn, numIter = iter,
        populationH = populationH)
      r.startTx()
      r
    }

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl[Out] { out =>
        protected val targets   = Targets[Out]
        val template            = context(proc.template  )
        val population          = context(proc.population)
        connect()
      }

    import Negatum._

    // ---- key maps ----

    def isConnected(implicit tx: S#Tx): Boolean = targets.nonEmpty

    final def connect()(implicit tx: S#Tx): this.type = {
      template  .changed ---> changed
      population.changed ---> changed
      this
    }

    private def disconnect()(implicit tx: S#Tx): Unit = {
      template  .changed -/-> changed
      population.changed -/-> changed
    }

    object changed extends Changed
      with evt.impl.Generator[S, Negatum.Update[S]] {
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Negatum.Update[S]] = {
        val templateCh    = template.changed
        val templateOpt   = if (pull.contains(templateCh  )) pull(templateCh  ) else None
        val populationCh  = population.changed
        val populationOpt = if (pull.contains(populationCh)) pull(populationCh) else None

        val seq0 = templateOpt.fold(Vector.empty[Change[S]]) { u =>
          Vector(TemplateChange(u))
        }

        val seq3 = populationOpt.fold(seq0) { u =>
          if (seq0.isEmpty) Vector(PopulationChange(u)) else seq0 :+ PopulationChange(u)
        }
        if (seq3.isEmpty) None else Some(Negatum.Update(proc, seq3))
      }
    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      template  .write(out)
      population.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      disconnect()
      template  .dispose()
      population.dispose()
    }

    override def toString: String = s"Negatum$id"
  }

  private final class New[S <: Sys[S]](temp0: AudioCue.Obj[S])(implicit tx0: S#Tx) extends Impl[S] {
    protected val targets   = evt.Targets[S](tx0)

    val template    = AudioCue.Obj.newVar[S](temp0)
    val population  = Folder[S]
    connect()(tx0)
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: evt.Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    {
      val serVer = in.readShort()
      if (serVer != SER_VERSION) sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    val template    = AudioCue.Obj.readVar[S](in, access)
    val population  = Folder      .read   [S](in, access)
  }
}