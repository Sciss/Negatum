/*
 *  NegatumRenderingImpl.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
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
import de.sciss.negatum.impl.Util._
import de.sciss.synth.proc.impl.MkSynthGraphSource
import de.sciss.synth.proc.{AudioCue, Proc, SynthGraphObj}
import de.sciss.synth.{SynthGraph, proc}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, blocking}
import scala.util.Random
import scala.util.control.NonFatal

//object NegatumRenderingImpl {
//  /** DEBUGGING*/
//  var instance: NegatumRenderingImpl[_] = null
//}
final class NegatumRenderingImpl[S <: Sys[S]](config: Config, template: AudioCue,
                                              popIn: Vec[Individual], populationH: stm.Source[S#Tx, Folder[S]], numIter: Int)
                                             (implicit protected val cursor: stm.Cursor[S])
  extends RenderingImpl[S, Unit, Vec[Individual]] {

//  NegatumRenderingImpl.instance = this

  private[this] val STORE_BAD_DEFS = false

  @volatile
  private[this] var _shouldStop   = false

  private[this] implicit val random: Random = new Random(config.seed)

  protected def body(): Vec[Individual] = blocking {
    import config._
    import gen._
    val pop = new Array[Individual](population)
//    DEBUG_ARRAY = pop

    var i = 0
    while (i < popIn.size) {
      pop(i) = popIn(i)
      i += 1
    }
    while (i < population) {
      val indiv = mkIndividual()
      pop(i) = indiv
      i += 1
    }

    val inputExtr: File = {
      val fut = Features.extract(template.artifact, config.eval.numMFCC)
      Await.result(fut, Duration(30, TimeUnit.SECONDS))._1
    }

    val INIT_COUNT  = pop.count(_.fitness.isNaN)
    val PROG_WEIGHT = 1.0 / (numIter.toLong * pop.length + INIT_COUNT)

    var iter = 0
    var PROG_COUNT = 0L

    def evalPop(): Unit = {
      var ii = 0
      while (ii < pop.length) {
        val indiv = pop(ii)
        if (indiv.fitness.isNaN) {
          val graph       = indiv.graph
          val numVertices = graph.sources.size  // XXX TODO -- ok?
          val fut = Evaluation(config, graph = graph, inputSpec = template.spec,
            inputExtr = inputExtr, numVertices = numVertices)
          // XXX TODO --- Mutagen used four parallel processes; should we do the same?
          val sim = try {
            Await.result(fut, Duration(30, TimeUnit.SECONDS))
          } catch {
            case NonFatal(ex) =>
              val message = if (ex.isInstanceOf[TimeoutException]) {
                "timeout"
              } else {
                s"failed - ${ex.getClass.getSimpleName}${if (ex.getMessage == null) "" else " - " + ex.getMessage}"
              }
              Console.err.println(s"Negatum: evaluation $message")
              if (STORE_BAD_DEFS) {
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
          indiv.fitness = sim
          checkAborted()
          PROG_COUNT += 1
          progress = PROG_COUNT * PROG_WEIGHT
        }
        ii += 1
      }
    }

    // evaluate those that haven't been
    evalPop()

    while (iter < numIter && !_shouldStop) {
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
      el.foreach { indiv =>
        pop(i) = indiv
        i += 1
      }
      mut.foreach { indiv =>
        pop(i) = indiv
        i += 1
      }
      cross.foreach { indiv =>
        pop(i) = indiv
        i += 1
      }
      golems.foreach { indiv =>
        pop(i) = indiv
        i += 1
      }

      evalPop()

      iter += 1
      PROG_COUNT  = iter.toLong * pop.length + INIT_COUNT
      progress    = PROG_COUNT * PROG_WEIGHT
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
    folder.clear()  // XXX TODO --- re-use existing procs?
    popOut.foreach { indiv =>
      val gObj  = SynthGraphObj.newConst[S](indiv.graph)
      val p     = Proc[S]
      import proc.Implicits._
      val attr  = p.attr
      p.name    = mkGraphName(indiv.graph)
      p.graph() = gObj
      if (!indiv.fitness.isNaN) {
        attr.put(Negatum.attrFitness, DoubleObj.newConst[S](indiv.fitness))
      }
      // XXX TODO --- should we store fitness or not?
      folder.addLast(p)
    }
  }

  override def stop()(implicit tx: S#Tx): Unit = tx.afterCommit { _shouldStop = true }
}