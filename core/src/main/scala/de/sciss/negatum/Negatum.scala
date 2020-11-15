/*
 *  Negatum.scala
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

import de.sciss.lucre.Publisher
import de.sciss.lucre.{Folder, Obj, Txn}
import de.sciss.model
import de.sciss.negatum.impl.{NegatumImpl => Impl}
import de.sciss.serial.{DataInput, TFormat}
import de.sciss.proc.{AudioCue, Universe}
import de.sciss.topology.Topology

import scala.collection.immutable.{IndexedSeq => Vec}

object Negatum extends Obj.Type {
  final val typeId = 0x40000

  type SynthGraphT = Topology[Vertex, Edge]

//  /** Initializes this type and other related type such as `SVMModel`. */
//  override def init(): Unit = {
//    super   .init()
//    SVMModel.init()
//    SOM     .init()
//  }

  // ---- creation ----

  def apply[T <: Txn[T]](template: AudioCue.Obj[T]   )(implicit tx: T): Negatum[T] = Impl[T](template)
  def read [T <: Txn[T]](in: DataInput)(implicit tx: T): Negatum[T] = Impl.read(in)

  // ----

  def attrToConfig[T <: Txn[T]](obj: Obj[T])(implicit tx: T): Config = Impl.attrToConfig(obj)

  implicit def format[T <: Txn[T]]: TFormat[T, Negatum[T]] = Impl.format[T]

  override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
    Impl.readIdentifiedObj(in)

  // ---- events ----

  /** An update is a sequence of changes */
  final case class Update[T <: Txn[T]](n: Negatum[T], changes: Vec[Change[T]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[T <: Txn[T]]

  final case class TemplateChange  [T <: Txn[T]](peer: model.Change[AudioCue]) extends Change[T]
  final case class PopulationChange[T <: Txn[T]](peer: Folder.Update[T])       extends Change[T]

  object Generation {
    def apply(
               population      : Int     = 500,
               probConst       : Double  = 0.5,
               minVertices     : Int     = 64,
               maxVertices     : Int     = 256,
               probDefault     : Double  = 0.05,
               allowedUGens    : Set[String] = Set.empty
    ): Generation = new Impl(population = population, probConst = probConst,
      minVertices = minVertices, maxVertices = maxVertices, probDefault = probDefault,
      allowedUGens = allowedUGens)

    private class Impl(val population: Int, val probConst: Double, val minVertices: Int, val maxVertices: Int,
                       val probDefault: Double, val allowedUGens: Set[String]) extends Generation
  }
  trait Generation {
    def population      : Int
    def probConst       : Double
    def minVertices     : Int
    def maxVertices     : Int
    def probDefault     : Double
    def allowedUGens    : Set[String]
  }

  object Evaluation {
    def apply(
               minFreq    : Int     = 32,
               maxFreq    : Int     = 16000,
               numMel     : Int     = 42,
               numMFCC    : Int     = 42,
               maxBoost   : Double  = 10.0,
               timeWeight : Double  = 0.3
             ): Evaluation =
      apply2(minFreq = minFreq, maxFreq = maxFreq, numMel = numMel, maxBoost = maxBoost,
        timeWeight = timeWeight)

    //
    def apply2(
               minFreq    : Int     = 32,
               maxFreq    : Int     = 16000,
               numMel     : Int     = 42,
               numMFCC    : Int     = 42,
               maxBoost   : Double  = 10.0,
               timeWeight : Double  = 0.3,
               timeOut    : Double  = 6.0,
    ): Evaluation =
      new Impl(minFreq = minFreq, maxFreq = maxFreq, numMFCC = numMFCC, numMel = numMel,
        maxBoost = maxBoost, timeWeight = timeWeight, timeOut = timeOut)

    private class Impl(val minFreq: Int, val maxFreq: Int, val numMFCC: Int, val numMel: Int, val maxBoost: Double,
                       val timeWeight: Double, override val timeOut: Double)
      extends Evaluation
  }
  trait Evaluation {
    def minFreq     : Int
    def maxFreq     : Int
    def numMFCC     : Int
    def numMel      : Int
    def maxBoost    : Double
    def timeWeight  : Double

    def timeOut     : Double = 6.0
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
               selectFraction : Double  = 0.33,
               elitism        : Int     = 3,
               minMut         : Int     = 2,
               maxMut         : Int     = 4,
               probMut        : Double  = 0.75,
               golem          : Int     = 15
    ): Breeding = new Impl(selectFraction = selectFraction, elitism = elitism,
      minMut = minMut, maxMut = maxMut, probMut = probMut, golem = golem)

    private class Impl(val selectFraction: Double, val elitism: Int, val minMut: Int, val maxMut: Int,
                       val probMut: Double, val golem: Int) extends Breeding
  }
  trait Breeding {
    def selectFraction: Double
    def elitism       : Int
    def minMut        : Int
    def maxMut        : Int
    def probMut       : Double
    def golem         : Int
  }

  object Config {
    def apply(
      seed            : Long        = System.currentTimeMillis(),
      generation      : Generation  = Generation(),
      evaluation      : Evaluation  = Evaluation(),
      penalty         : Penalty     = Penalty(),
      breeding        : Breeding    = Breeding()
    ): Config = new Impl(seed = seed, gen = generation, eval = evaluation, penalty = penalty, breed = breeding)

    final val default: Config = apply()

    private class Impl(val seed: Long, val gen: Generation, val eval: Evaluation,
                       val penalty: Penalty, val breed: Breeding) extends Config
  }
  trait Config {
    val seed    : Long
    val gen     : Generation
    val eval    : Evaluation
    val penalty : Penalty
    val breed   : Breeding
  }

  /** Attribute for evaluated fitness (children of `population`). Type `Double` */
  final val attrFitness   = "fitness"

  /** Attribute for inclusion in SOM. Type `Boolean` */
  final val attrSelected  = "selected"

  /** Attribute for SOM evaluation. Type `Vec[Double]` (`DoubleVector`) */
  final val attrFeatures  = "features"

  /** Attribute for config defaults. Type `Long` */
  final val attrSeed              = "seed"

  /** Attribute for generation config defaults. Type `Int` */
  final val attrGenPopulation     = "gen-population"

  /** Attribute for generation config defaults. Type `Double` */
  final val attrGenProbConst      = "gen-prob-const"

  /** Attribute for generation config defaults. Type `Int` */
  final val attrGenMinVertices    = "gen-min-vertices"

  /** Attribute for generation config defaults. Type `Int` */
  final val attrGenMaxVertices    = "gen-max-vertices"

  /** Attribute for generation config defaults. Type `Double` */
  final val attrGenProbDefault    = "gen-prob-default"

  //  final val attrGenAllowedUGens  = "gen-allowed-ugens"

  /** Attribute for evaluation config defaults. Type `Int` */
  final val attrEvalMinFreq       = "eval-min-freq"

  /** Attribute for evaluation config defaults. Type `Int` */
  final val attrEvalMaxFreq       = "eval-max-freq"

  /** Attribute for evaluation config defaults. Type `Int` */
  final val attrEvalNumMFCC       = "eval-num-mfcc"

  /** Attribute for evaluation config defaults. Type `Int` */
  final val attrEvalNumMel        = "eval-num-mel"

//  /** Attribute for evaluation config defaults. Type `Boolean` */
//  final val attrEvalNormMFCC      = "eval-norm-mfcc"

  /** Attribute for evaluation config defaults. Type `Double` */
  final val attrEvalMaxBoost      = "eval-max-boost"

  /** Attribute for evaluation config defaults. Type `Double` */
  final val attrEvalTimeWeight    = "eval-time-weight"

  /** Attribute for breeding config defaults. Type `Double` */
  final val attrBreedSelectFraction = "breed-select-frac"

  /** Attribute for breeding config defaults. Type `Int` */
  final val attrBreedElitism      = "breed-elitism"

  /** Attribute for breeding config defaults. Type `Int` */
  final val attrBreedMinMut       = "breed-min-mut"

  /** Attribute for breeding config defaults. Type `Int` */
  final val attrBreedMaxMut       = "breed-max-mut"

  /** Attribute for breeding config defaults. Type `Double` */
  final val attrBreedProbMut      = "breed-prob-mut"

  /** Attribute for breeding config defaults. Type `Int` */
  final val attrBreedGolem        = "breed-golem"

  /** Attribute for optimize settings: analysis duration in seconds. Type `Double` */
  final val attrOptDuration       = "optimize-duration"

  /** Attribute for optimize settings: whether to expand `Protect` elements. Type `Boolean` */
  final val attrOptExpandProtect  = "optimize-expand-protect"

  /** Attribute for optimize settings: whether to expand IO elements. Type `Boolean` */
  final val attrOptExpandIO       = "optimize-expand-io"
}
trait Negatum[T <: Txn[T]] extends Obj[T] with Publisher[T, Negatum.Update[T]] {

  def run(config: Negatum.Config, iterations: Int = 1)
         (implicit tx: T, universe: Universe[T]): Rendering[T, Unit]

  def template: AudioCue.Obj.Var[T]

  /** The folder's children are of type `Proc`.
    *
    * ''NOT:'' Each child's attribute
    * map contains (after evaluation) the key `attrFitness`, and (after selection)
    * the attribute `attrSelected`.
    */
  def population: Folder[T]
}