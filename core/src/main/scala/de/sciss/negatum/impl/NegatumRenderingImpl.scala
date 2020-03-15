/*
 *  NegatumRenderingImpl.scala
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

import java.io.FileOutputStream
import java.util.concurrent.{TimeUnit, TimeoutException}

import de.sciss.file._
import de.sciss.lucre.expr.DoubleObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Folder, Sys}
import de.sciss.negatum.Negatum.Config
import de.sciss.negatum.impl.Util.scramble
import de.sciss.synth.proc.impl.MkSynthGraphSource
import de.sciss.synth.proc.{AudioCue, Proc, SynthGraphObj}
import de.sciss.synth.{SynthGraph, proc}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, blocking}
import scala.util.Random
import scala.util.control.NonFatal

object NegatumRenderingImpl {
  /** DEBUGGING*/
  var STORE_BAD_DEFINITIONS = false // if `true`, writes to `~Documents/temp/negatum_broken`
  var REPORT_TIME_OUT       = false
}
final class NegatumRenderingImpl[S <: Sys[S]](config: Config, template: AudioCue,
                                              popIn: Vec[Individual], populationH: stm.Source[S#Tx, Folder[S]],
                                              numIterations: Int)
                                             (implicit protected val cursor: stm.Cursor[S])
  extends RenderingImpl[S, Unit, Vec[Individual]] {

//  NegatumRenderingImpl.instance = this

  import NegatumRenderingImpl._

  @volatile
  private[this] var _shouldStop   = false

  private[this] implicit val random: Random = new Random(config.seed)

  protected def body(): Vec[Individual] = blocking {
    import config._
    import gen._
    val pop = new Array[Individual](population)
//    DEBUG_ARRAY = pop

    var i = 0
    val sz0 = math.min(popIn.size, population)
    while (i < sz0) {
      pop(i) = popIn(i)
      i += 1
    }
    while (i < population) {
      val individual = mkIndividual()
      pop(i) = individual
      i += 1
    }

    val inputFeatureF: File = {
      val e = config.eval
      val featCfg = Features.Config(
        minFreq = e.minFreq,
        maxFreq = e.maxFreq,
        numMel  = e.numMel,
        numMFCC = e.numMFCC
      )
      val fut = Features.extract(template.artifact, featCfg)
      Await.result(fut, Duration(6 /* 30 */, TimeUnit.SECONDS))._1
    }

    val INIT_COUNT      = pop.count(_.fitness.isNaN)
    val PROGRESS_WEIGHT = 1.0 / (numIterations.toLong * pop.length + INIT_COUNT)
    var iteration       = 0
    var PROGRESS_COUNT  = 0L

    def evalPop(): Unit = {
      var ii = 0
      while (ii < pop.length) {
        val individual = pop(ii)
        if (individual.fitness.isNaN) {
          val graph       = individual.graph
          val numVertices = graph.sources.size  // XXX TODO -- ok?
          val fut = Evaluation(config, graph = graph, inputSpec = template.spec,
            inputExtr = inputFeatureF, numVertices = numVertices)
          // XXX TODO --- Mutagen used four parallel processes; should we do the same?
          val sim = try {
            Await.result(fut, Duration(6 /* 30 */, TimeUnit.SECONDS))
          } catch {
            case NonFatal(ex) =>
              val message = if (ex.isInstanceOf[TimeoutException]) {
                if (REPORT_TIME_OUT) "timeout" else ""
              } else {
                s"failed - ${ex.getClass.getSimpleName}${if (ex.getMessage == null) "" else " - " + ex.getMessage}"
              }
              if (!message.isEmpty) Console.err.println(s"Negatum: evaluation $message")
              if (STORE_BAD_DEFINITIONS) {
                val dir = userHome / "Documents" / "temp" / "negatum_broken"
                dir.mkdirs()
                val source = MkSynthGraphSource(graph)
                val name   = mkGraphName(graph)
                try {
                  val fos  = new FileOutputStream(dir / s"$name.scala")
                  try {
                    fos.write(source.getBytes("UTF-8"))
                  } finally {
                    fos.close()
                  }
                } catch {
                  case NonFatal(ex2) =>
                    ex2.printStackTrace()
                }
              }
              0.0f
          }
          // negative correlations are not nice because they
          // mess up the roulette selection; so clip them to zero
          individual.fitness = math.max(0.0, sim)
          checkAborted()
          PROGRESS_COUNT += 1
          progress = PROGRESS_COUNT * PROGRESS_WEIGHT
        }
        ii += 1
      }
    }

    // evaluate those that haven't been
    evalPop()

    while (iteration < numIterations && !_shouldStop) {
      val el    = Selection.elitism(config, pop)
      val _sel0 = Selection(config, pop)
      val sel   = scramble(_sel0)

      import config.breed._

      val numGolem1 = math.min(golem, pop.length - el.size)
      val nGen      = pop.length - el.size - numGolem1
      val nMut      = (probMut * nGen + 0.5).toInt
      val nCross    = nGen - nMut

      val mut       = Mutation (config, sel, nMut)
      val cross     = Crossover(config, sel, nCross)

      val golems    = Vector.fill(numGolem1)(mkIndividual())

      i = 0
      el.foreach { individual =>
        pop(i) = individual
        i += 1
      }
      mut.foreach { individual =>
        pop(i) = individual
        i += 1
      }
      cross.foreach { individual =>
        pop(i) = individual
        i += 1
      }
      golems.foreach { individual =>
        pop(i) = individual
        i += 1
      }

      evalPop()

      iteration      += 1
      PROGRESS_COUNT  = iteration.toLong * pop.length + INIT_COUNT
      progress        = PROGRESS_COUNT * PROGRESS_WEIGHT
      checkAborted()
    }

    pop.toIndexedSeq
  }

  // ---- utility ----

  /* Creates an individual chromosome. */
  @inline
  private def mkIndividual(): Individual = {
    val g = Chromosome.mkGraph(config)
    new Individual(g)
  }

  private def mkGraphName(graph: SynthGraph): String = s"negatum-${graph.hashCode().toHexString}"

//  def DEBUG_FILL(): Unit = cursor.step { implicit tx =>
//    fillResult(DEBUG_ARRAY.toVector)
//  }

  protected def fillResult(popOut: Vec[Individual])(implicit tx: S#Tx): Unit = {
    val folder = populationH()
    folder.clear()  // XXX TODO --- re-use existing processes?
    popOut.foreach { individual =>
      val gObj  = SynthGraphObj.newConst[S](individual.graph)
      val p     = Proc[S]
      import proc.Implicits._
      val attr  = p.attr
      p.name    = mkGraphName(individual.graph)
      p.graph() = gObj
      if (!individual.fitness.isNaN) {
        attr.put(Negatum.attrFitness, DoubleObj.newConst[S](individual.fitness))
      }
      // XXX TODO --- should we store fitness or not?
      folder.addLast(p)
    }
  }

  override def stop()(implicit tx: S#Tx): Unit = tx.afterCommit { _shouldStop = true }
}