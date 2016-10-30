package de.sciss.negatum

import de.sciss.file._
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Copy, Sys}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.mellite.Mellite
import de.sciss.synth.proc
import de.sciss.synth.proc.{Folder, Workspace}
import SVMModel.Rendering

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/** Uses the first `Negatum` and first `SVMModel` found in the session
  * to re-evaluate the selection in the `Negatum`, adding it as a copy
  * to the session.
  */
object SelectionTest extends App {
  val sessionF = userHome / "mellite" / "sessions" / "Negatum-forModelBuilding.mllt"

  Mellite.initTypes()
  Negatum.init()

  Workspace.read(sessionF, BerkeleyDB.factory(sessionF, createIfNecessary = false)) match {
    case workspace: Workspace.Durable => run(workspace)
    case other => println(s"Not a durable workspace: $other")
  }

  def run[S <: Sys[S]](workspace: Workspace[S]): Unit = {
    implicit val cursor: stm.Cursor[S] = workspace.cursor

    val prediction = cursor.step { implicit tx =>
      val negIn: Negatum[S] = workspace.collectObjects {
        case f: Folder[S] => f.iterator.collectFirst {
          case n: Negatum[S] => n
        }
      } .flatten.head

      val model = workspace.collectObjects {
        case m: SVMModel[S] => m
      } .head

      val cpy = Copy[S, S]
      val negOut = cpy(negIn)
      cpy.finish()
      import proc.Implicits._
      negOut.name = "Out"
      workspace.root.addLast(negOut)

      val rendering = model.predict(negOut)
      rendering.reactNow { implicit tx => {
        case Rendering.Success(selected) =>
          tx.afterCommit(sys.exit())

        case Rendering.Failure(ex) =>
          tx.afterCommit {
            println("Prediction failed!")
            ex.printStackTrace()
            cursor.step { implicit tx =>
              workspace.dispose()
            }
            sys.exit(1)
          }

        case Rendering.Progress(amt) =>
          println(f"progress = $amt%1.1f%%")
      }}
    }

    import ExecutionContext.Implicits.global
//    println("_" * 33)
//    prediction.monitor(printResult = true)

    // idiotic way to keep the program from exiting immediately
    val sync = new AnyRef
    new Thread {
      override def run(): Unit = sync.synchronized(sync.wait())
      start()
    }
  }
}