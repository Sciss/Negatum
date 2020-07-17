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

import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.{DoubleObj, IntObj}
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, Folder, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.negatum.Negatum.Config
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.proc.{AudioCue, Proc, Universe}

object NegatumImpl {
  private final val SER_VERSION = 0x4e56  // "Ne"

  def apply[S <: Sys[S]](template: AudioCue.Obj[S])(implicit tx: S#Tx): Negatum[S] = new New[S](template)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Negatum[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Negatum[S]] = anySer.asInstanceOf[Ser[S]]

  def attrToConfig[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): Config = {
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

    final def run(config: Config, iterations: Int)
                 (implicit tx: S#Tx, universe: Universe[S]): Rendering[S, Unit] = {
      val popIn = population.iterator.collect {
        case p: Proc[S] =>
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
      val r = new NegatumRenderingImpl[S](config = config, template = templateV, popIn = popIn, numIterations = iterations,
        populationH = populationH)
      r.startTx()
      r
    }

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl[Out] { out =>
        protected val targets : Targets         [Out] = Targets[Out]
        val template          : AudioCue.Obj.Var[Out] = context(proc.template  )
        val population        : Folder          [Out] = context(proc.population)
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
    protected val targets: Targets[S] = Targets[S](tx0)

    val template  : AudioCue.Obj.Var[S] = AudioCue.Obj.newVar[S](temp0)
    val population: Folder[S]           = Folder[S]()
    connect()(tx0)
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: evt.Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    {
      val serVer = in.readShort()
      if (serVer != SER_VERSION) sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    val template  : AudioCue.Obj.Var[S] = AudioCue.Obj.readVar[S](in, access)
    val population: Folder[S]           = Folder      .read   [S](in, access)
  }
}