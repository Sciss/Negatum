package de.sciss.negatum

import de.sciss.file._
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.mellite.Mellite
import de.sciss.negatum.SVMConfig.{Kernel, Type}
import de.sciss.synth.proc
import de.sciss.synth.proc.{Folder, Workspace}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object MakeModel extends App {
  // val sessionF = userHome / "mellite" / "sessions" / "Negatum-forModelBuilding.mllt"
  val sessionF = userHome / "Documents" / "projects" / "Imperfect" / "anemone" / "Negatum-forModelBuilding2.mllt"

  Mellite.initTypes()
  Negatum.init()

  Workspace.read(sessionF, BerkeleyDB.factory(sessionF, createIfNecessary = false)) match {
    case workspace: Workspace.Durable => run(workspace)
    case other => println(s"Not a durable workspace: $other")
  }

  def run[S <: Sys[S]](workspace: Workspace[S]): Unit = {
    implicit val cursor: stm.Cursor[S] = workspace.cursor

    val training = cursor.step { implicit tx =>
      val neg: Vec[Negatum[S]] = workspace.collectObjects {
        case f: Folder[S] => f.iterator.collectFirst {
          case n: Negatum[S] => n
        }
      } .flatten

      println(s"Analyzing ${neg.size} results...")

      /*
        Remember: weighting -- C(+) / C(-) = n(-) / n(+)

        So with n(-) = 5000 - 718 and n(+) = 718,

        C(+) / C(-) = (5000 - 718) / 718 = 5000/718 - 1
        C(+) = C(-) * (5000/718 - 1)

        say

        w(+) = 5.9637, w(-) = 1.0

        e.g. make w(+) + w(-) == 1.0

        w(+) = (5000/718 - 1) / (5000/718) -> w(+) = 0.8564, w(-) = 0.1436


       */
      val conf = SVMConfig()
      conf.tpe = Type.CSVC(50, Nil)
      // note: `train` will automatically set the weights if using `CSVC`!
      // List(Weight(label = 0, value = 718/5000.0f), Weight(label = 1, value = (5000 - 718)/5000.0f)))
      conf.kernel = Kernel.Radial(0.5f)
      SVMModel.train(n = neg, config = conf, numCoeff = 24)
    }

    import ExecutionContext.Implicits.global
    println("_" * 33)
    training.monitor(printResult = false)

    // idiotic way to keep the program from exiting immediately
    val sync = new AnyRef
    new Thread {
      override def run(): Unit = sync.synchronized(sync.wait())
      start()
    }

    training.onComplete {
      case Success(modelH) =>
        val stats = cursor.step { implicit tx =>
          val model = modelH()
          import proc.Implicits._
          model.name = "svm-model"
          workspace.root.addLast(model)
          val res = model.stats
          workspace.dispose()
          res
        }
        println("Stats:")
        println(stats)
        sys.exit()

      case Failure(ex) =>
        println("Training failed!")
        ex.printStackTrace()
        cursor.step { implicit tx =>
          workspace.dispose()
        }
        sys.exit(1)
    }
  }
}
