/*
 *  Negatum.scala
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

import de.sciss.lucre.event.{Observable, Publisher}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj, Sys}
import de.sciss.model
import de.sciss.negatum.impl.{NegatumImpl => Impl}
import de.sciss.processor.Processor
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.{AudioCue, Folder, WorkspaceHandle}

import scala.collection.immutable.{IndexedSeq => Vec}

object Negatum extends Obj.Type {
  final val typeID = 0x40000

  def apply[S <: Sys[S]](template: AudioCue.Obj[S])(implicit tx: S#Tx): Negatum[S] = Impl[S](template)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Negatum[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Negatum[S]] = Impl.serializer[S]

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  // ---- events ----

  /** An update is a sequence of changes */
  final case class Update[S <: Sys[S]](n: Negatum[S], changes: Vec[Change[S]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[S <: Sys[S]]

  final case class TemplateChange  [S <: Sys[S]](peer: model.Change[AudioCue]) extends Change[S]
  final case class PopulationChange[S <: Sys[S]](peer: Folder.Update[S])       extends Change[S]

  object Rendering {
    sealed trait State {
      def isComplete: Boolean
    }
    case object Success extends State {
      def isComplete = true
    }
    /** Rendering either failed or was aborted.
      * In the case of abortion, the throwable is
      * of type `Cancelled`.
      */
    final case class Failure(ex: Throwable) extends State {
      def isComplete = true
    }
    final case class Progress(amount: Double) extends State {
      def isComplete = false
    }

    val  Cancelled = Processor.Aborted
    type Cancelled = Processor.Aborted
  }
  trait Rendering[S <: Sys[S]] extends Observable[S#Tx, Rendering.State] with Disposable[S#Tx] {
    def state(implicit tx: S#Tx): Rendering.State

    /** Like `react` but invokes the function immediately with the current state. */
    def reactNow(fun: S#Tx => Rendering.State => Unit)(implicit tx: S#Tx): Disposable[S#Tx]

    def cancel()(implicit tx: S#Tx): Unit
  }

  object Generation {
    def apply(
      population      : Int     = 500,
      constProb       : Double  = 0.5,
      minNumVertices  : Int     = 64,
      maxNumVertices  : Int     = 256,
      nonDefaultProb  : Double  = 0.95,
      allowedUGens    : Set[String] = Set.empty
    ): Generation = new Impl(population = population, constProb = constProb,
      minNumVertices = minNumVertices, maxNumVertices = maxNumVertices, nonDefaultProb = nonDefaultProb,
      allowedUGens = allowedUGens)

    private class Impl(val population: Int, val constProb: Double,  val minNumVertices: Int, val maxNumVertices: Int,
                       val nonDefaultProb: Double, val allowedUGens: Set[String]) extends Generation
  }
  trait Generation {
    def population      : Int
    def constProb       : Double
    def minNumVertices  : Int
    def maxNumVertices  : Int
    def nonDefaultProb  : Double
    def allowedUGens    : Set[String]
  }

  object Evaluation {
    def apply(
      numMFCC         : Int     = 42,
      normalizeMFCC   : Boolean = false,
      maxBoost        : Double  = 10.0,
      temporalWeight  : Double  = 0.3
    ): Evaluation = new Impl(numMFCC = numMFCC, normalizeMFCC = normalizeMFCC, maxBoost = maxBoost,
      temporalWeight = temporalWeight)

    private class Impl(val numMFCC: Int, val normalizeMFCC: Boolean, val maxBoost: Double,
                       val temporalWeight: Double) extends Evaluation
  }
  trait Evaluation {
    def numMFCC         : Int
    def normalizeMFCC   : Boolean
    def maxBoost        : Double
    def temporalWeight  : Double
  }

  object Penalty {
    def apply(
      vertexPenalty   : Double  = 0.02,
      graphPenaltyIter: Int     = 10,
      graphPenaltyCeil: Double  = 0.275,
      graphPenaltyAmt : Double  = 0.2,
      graphPenaltyCoin: Double  = 0.25
    ): Penalty = new Impl(vertexPenalty = vertexPenalty, graphPenaltyIter = graphPenaltyIter,
      graphPenaltyCeil = graphPenaltyCeil, graphPenaltyAmt = graphPenaltyAmt, graphPenaltyCoin = graphPenaltyCoin)

    private class Impl(val vertexPenalty   : Double, val graphPenaltyIter: Int, val graphPenaltyCeil: Double,
                       val graphPenaltyAmt : Double, val graphPenaltyCoin: Double) extends Penalty
  }
  trait Penalty {
    def vertexPenalty   : Double
    def graphPenaltyIter: Int
    def graphPenaltyCeil: Double
    def graphPenaltyAmt : Double
    def graphPenaltyCoin: Double
  }

  object Breeding {
    def apply(
      selectionFrac   : Double  = 0.33,
      numElitism      : Int     = 3,
      mutMin          : Int     = 2,
      mutMax          : Int     = 4,
      mutProb         : Double  = 0.75,
      numGolem        : Int     = 15
    ): Breeding = new Impl(selectionFrac = selectionFrac, numElitism = numElitism,
      mutMin = mutMin, mutMax = mutMax, mutProb = mutProb, numGolem = numGolem)

    private class Impl(val selectionFrac: Double, val numElitism: Int, val mutMin: Int, val mutMax: Int,
                       val mutProb: Double, val numGolem: Int) extends Breeding
  }
  trait Breeding {
    def selectionFrac   : Double
    def numElitism      : Int
    def mutMin          : Int
    def mutMax          : Int
    def mutProb         : Double
    def numGolem        : Int
  }

  object Config {
    def apply(
      generation      : Generation  = Generation(),
      evaluation      : Evaluation  = Evaluation(),
      penalty         : Penalty     = Penalty(),
      breeding        : Breeding    = Breeding()
    ): Config = new Impl(generation = generation, evaluation = evaluation, penalty = penalty, breeding = breeding)

    private class Impl(val generation: Generation, val evaluation: Evaluation,
                       val penalty: Penalty, val breeding: Breeding) extends Config
  }
  trait Config {
    val generation      : Generation
    val evaluation      : Evaluation
    val penalty         : Penalty
    val breeding        : Breeding
  }

  /** Attribute for evaluated fitness (children of `population`). Type `Double` */
  final val attrFitness = "fitness"

//  /** Attribute for individual being selected (children of `population`). Type `Boolean` */
//  final val attrSelected = "selected"
}
trait Negatum[S <: Sys[S]] extends Obj[S] with Publisher[S, Negatum.Update[S]] {
  def run(config: Negatum.Config, iter: Int = 1)(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                  workspace: WorkspaceHandle[S]): Negatum.Rendering[S]

  def template: AudioCue.Obj.Var[S]

  /** The folder's children are of type `SynthGraphObj`. Each child's attribute
    * map contains (after evaluation) the key `attrFitness`, and (after selection)
    * the attribute `attrSelected`.
    */
  def population: Folder[S]
}