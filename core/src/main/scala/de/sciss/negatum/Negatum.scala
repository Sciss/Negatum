/*
 *  Negatum.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.stm.{Folder, Obj, Sys}
import de.sciss.model
import de.sciss.negatum.impl.{NegatumImpl => Impl}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.{AudioCue, Universe}

import scala.collection.immutable.{IndexedSeq => Vec}

object Negatum extends Obj.Type {
  final val typeId = 0x40000

//  /** Initializes this type and other related type such as `SVMModel`. */
//  override def init(): Unit = {
//    super   .init()
//    SVMModel.init()
//    SOM     .init()
//  }

  def apply[S <: Sys[S]](template: AudioCue.Obj[S]   )(implicit tx: S#Tx): Negatum[S] = Impl[S](template)
  def read [S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Negatum[S] = Impl.read(in, access)

  def attrToConfig[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): Config = Impl.attrToConfig(obj)

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
               numMFCC    : Int     = 42,
               normMFCC   : Boolean = false,
               maxBoost   : Double  = 10.0,
               timeWeight : Double  = 0.3
    ): Evaluation = new Impl(numMFCC = numMFCC, normMFCC = normMFCC, maxBoost = maxBoost,
      timeWeight = timeWeight)

    private class Impl(val numMFCC: Int, val normMFCC: Boolean, val maxBoost: Double,
                       val timeWeight: Double) extends Evaluation
  }
  trait Evaluation {
    def numMFCC     : Int
    def normMFCC    : Boolean
    def maxBoost    : Double
    def timeWeight  : Double
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
               selectFrac  : Double  = 0.33,
               elitism     : Int     = 3,
               minMut      : Int     = 2,
               maxMut      : Int     = 4,
               probMut     : Double  = 0.75,
               golem       : Int     = 15
    ): Breeding = new Impl(selectFrac = selectFrac, elitism = elitism,
      minMut = minMut, maxMut = maxMut, probMut = probMut, golem = golem)

    private class Impl(val selectFrac: Double, val elitism: Int, val minMut: Int, val maxMut: Int,
                       val probMut: Double, val golem: Int) extends Breeding
  }
  trait Breeding {
    def selectFrac   : Double
    def elitism      : Int
    def minMut       : Int
    def maxMut       : Int
    def probMut      : Double
    def golem        : Int
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
  final val attrEvalNumMFCC       = "eval-num-mfcc"

  /** Attribute for evaluation config defaults. Type `Boolean` */
  final val attrEvalNormMFCC      = "eval-norm-mfcc"

  /** Attribute for evaluation config defaults. Type `Double` */
  final val attrEvalMaxBoost      = "eval-max-boost"

  /** Attribute for evaluation config defaults. Type `Double` */
  final val attrEvalTimeWeight    = "eval-time-weight"

  /** Attribute for breeding config defaults. Type `Double` */
  final val attrBreedSelectFrac   = "breed-select-frac"

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
}
trait Negatum[S <: Sys[S]] extends Obj[S] with Publisher[S, Negatum.Update[S]] {

  def run(config: Negatum.Config, iter: Int = 1)
         (implicit tx: S#Tx, universe: Universe[S]): Rendering[S, Unit]

  def template: AudioCue.Obj.Var[S]

  /** The folder's children are of type `Proc`.
    *
    * ''NOT:'' Each child's attribute
    * map contains (after evaluation) the key `attrFitness`, and (after selection)
    * the attribute `attrSelected`.
    */
  def population: Folder[S]
}