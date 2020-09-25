/*
 *  NegatumImpl.scala
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
package impl

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.{GeneratorEvent, ObjFormat, SingleEventNode}
import de.sciss.lucre.synth.AnyTxn
import de.sciss.lucre.{Copy, DoubleObj, Elem, Folder, IntObj, Obj, Pull, Txn}
import de.sciss.negatum.Negatum.Config
import de.sciss.serial.{DataInput, DataOutput, TFormat}
import de.sciss.synth.proc.{AudioCue, Proc, Universe}

object NegatumImpl {
  private final val SER_VERSION = 0x4e56  // "Ne"

  def apply[T <: Txn[T]](template: AudioCue.Obj[T])(implicit tx: T): Negatum[T] = new New[T](template)

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Negatum[T] =
    format[T].readT(in)

  def format[T <: Txn[T]]: TFormat[T, Negatum[T]] = anyFmt.asInstanceOf[Fmt[T]]

  def attrToConfig[T <: Txn[T]](obj: Obj[T])(implicit tx: T): Config = {
    val attr  = obj.attr
    import Negatum.Config.default._
    import Negatum._
    val seed      = attr.$[IntObj](attrSeed).map(_.value.toLong).getOrElse(System.currentTimeMillis())

    val cGen      = Negatum.Generation(
      population  = attr.$[IntObj    ](attrGenPopulation  ).map(_.value).getOrElse(gen.population),
      probConst   = attr.$[DoubleObj ](attrGenProbConst   ).map(_.value).getOrElse(gen.probConst),
      minVertices = attr.$[IntObj    ](attrGenMinVertices ).map(_.value).getOrElse(gen.minVertices),
      maxVertices = attr.$[IntObj    ](attrGenMaxVertices ).map(_.value).getOrElse(gen.maxVertices),
      probDefault = attr.$[DoubleObj ](attrGenProbDefault ).map(_.value).getOrElse(gen.probDefault)
      // allowedUGens
    )
    val cBreed    = Negatum.Breeding(
      selectFraction  = attr.$[DoubleObj ](attrBreedSelectFraction).map(_.value).getOrElse(breed.selectFraction),
      elitism     = attr.$[IntObj    ](attrBreedElitism   ).map(_.value).getOrElse(breed.elitism),
      minMut      = attr.$[IntObj    ](attrBreedMinMut    ).map(_.value).getOrElse(breed.minMut),
      maxMut      = attr.$[IntObj    ](attrBreedMaxMut    ).map(_.value).getOrElse(breed.maxMut),
      probMut     = attr.$[DoubleObj ](attrBreedProbMut   ).map(_.value).getOrElse(breed.probMut),
      golem       = attr.$[IntObj    ](attrBreedGolem     ).map(_.value).getOrElse(breed.golem)
    )
    val cEval     = Negatum.Evaluation(
      minFreq     = attr.$[IntObj    ](attrEvalMinFreq    ).map(_.value).getOrElse(eval.minFreq ),
      maxFreq     = attr.$[IntObj    ](attrEvalMaxFreq    ).map(_.value).getOrElse(eval.maxFreq ),
      numMFCC     = attr.$[IntObj    ](attrEvalNumMFCC    ).map(_.value).getOrElse(eval.numMFCC ),
      numMel      = attr.$[IntObj    ](attrEvalNumMel     ).map(_.value).getOrElse(eval.numMel  ),
      maxBoost    = attr.$[DoubleObj ](attrEvalMaxBoost   ).map(_.value).getOrElse(eval.maxBoost),
      timeWeight  = attr.$[DoubleObj ](attrEvalTimeWeight ).map(_.value).getOrElse(eval.timeWeight)
    )
    val cPenalty  = Negatum.Penalty()
    val config    = Negatum.Config(seed = seed, generation = cGen, breeding = cBreed, evaluation = cEval,
      penalty = cPenalty)
    config
  }

  private val anyFmt = new Fmt[AnyTxn]

  private class Fmt[T <: Txn[T]] extends ObjFormat[T, Negatum[T]] {
    def tpe: Obj.Type = Negatum
  }

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Negatum[T] = {
    val targets = Targets.read(in)
    new Read(in, targets)
  }

  // ----

  private sealed trait Impl[T <: Txn[T]]
    extends Negatum[T] with SingleEventNode[T, Negatum.Update[T]] {
    proc =>

    final def tpe: Obj.Type = Negatum

    // --- rendering ---

    final def run(config: Config, iterations: Int)
                 (implicit tx: T, universe: Universe[T]): Rendering[T, Unit] = {
      val popIn = population.iterator.collect {
        case p: Proc[T] =>
          val gObj      = p.graph()
          val g         = gObj.value
          val attr      = p.attr
          val fitness   = attr.$[DoubleObj ](Negatum.attrFitness ).map(_.value).getOrElse(Double.NaN)
//          val selected  = attr.$[BooleanObj](Negatum.attrSelected).exists(_.value)
          new Individual(g, fitness = fitness)
      } .toIndexedSeq
      val templateV   = template.value
      val populationH = tx.newHandle(population)
      import universe.cursor
      val r = new NegatumRenderingImpl[T](config = config, template = templateV, popIn = popIn, numIterations = iterations,
        populationH = populationH)
      r.startTx()
      r
    }

    def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Impl[Out] { out =>
        protected val targets : Targets         [Out] = Targets[Out]()
        val template          : AudioCue.Obj.Var[Out] = context(proc.template  )
        val population        : Folder          [Out] = context(proc.population)
        connect()
      }

    import Negatum._

    // ---- key maps ----

    def isConnected(implicit tx: T): Boolean = targets.nonEmpty

    final def connect()(implicit tx: T): this.type = {
      template  .changed ---> changed
      population.changed ---> changed
      this
    }

    private def disconnect()(implicit tx: T): Unit = {
      template  .changed -/-> changed
      population.changed -/-> changed
    }

    object changed extends Changed
      with GeneratorEvent[T, Negatum.Update[T]] {
      def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Negatum.Update[T]] = {
        val templateCh    = template.changed
        val templateOpt   = if (pull.contains(templateCh  )) pull(templateCh  ) else None
        val populationCh  = population.changed
        val populationOpt = if (pull.contains(populationCh)) pull(populationCh) else None

        val seq0 = templateOpt.fold(Vector.empty[Change[T]]) { u =>
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

    final protected def disposeData()(implicit tx: T): Unit = {
      disconnect()
      template  .dispose()
      population.dispose()
    }

    override def toString: String = s"Negatum$id"
  }

  private final class New[T <: Txn[T]](temp0: AudioCue.Obj[T])(implicit tx0: T) extends Impl[T] {
    protected val targets: Targets[T] = Targets[T]()(tx0)

    val template  : AudioCue.Obj.Var[T] = AudioCue.Obj.newVar[T](temp0)
    val population: Folder[T]           = Folder[T]()
    connect()(tx0)
  }

  private final class Read[T <: Txn[T]](in: DataInput, protected val targets: Targets[T])
                                       (implicit tx0: T)
    extends Impl[T] {

    {
      val serVer = in.readShort()
      if (serVer != SER_VERSION) sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    val template  : AudioCue.Obj.Var[T] = AudioCue.Obj.readVar[T](in)
    val population: Folder[T]           = Folder      .read   [T](in)
  }
}