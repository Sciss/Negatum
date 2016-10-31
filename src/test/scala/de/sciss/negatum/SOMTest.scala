package de.sciss.negatum

import de.sciss.file._

import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.mellite.Mellite
import de.sciss.synth.proc.Durable

object SOMTest extends App {
  Mellite.initTypes()
  Negatum.init()

  type S = Durable
  val dir = File.createTemp("sleepycat_", "db")
  dir.delete()
//  val factory = BerkeleyDB.tmp()
  val factory = BerkeleyDB.factory(dir)
  implicit val cursor = Durable(BerkeleyDB.tmp())

  val r  = new util.Random(1L)
  val t1 = System.currentTimeMillis()
  val somH = cursor.root { implicit tx =>
    val config = SOM.Config()
    config.features       = 48
    config.dimensions     = 2
    config.seed           = 0L
    config.numIterations  = 200
    config.extent         = 128
    SOM(config)
  }
  val t2 = System.currentTimeMillis()
  cursor.step { implicit tx =>
    val som = somH()
    for (i <- 0 until 200) {
      val obj = IntObj.newConst[S](i)
      som.add(key = Vector.fill(48)(r.nextDouble()), value = obj)
    }
  }
  val t3 = System.currentTimeMillis()
  val opt = cursor.step { implicit tx =>
    val som = somH()
    som.query(Seq(0, 0))
  }
  val t4 = System.currentTimeMillis()

  println(s"Creation  took ${t2 - t1}ms.")
  println(s"Insertion took ${t3 - t2}ms.")
  println(s"Query     took ${t4 - t3}ms.")
  println(s"\nQuery result: $opt")

  val stats = cursor.step { implicit tx =>
    val som = somH()
    som.debugStats()
  }
  println(s"Stats: $stats")

  cursor.close()

  def deleteRecursively(f: File): Unit =
    if (f.isFile) f.delete() else if (f.isDirectory) f.children.foreach(deleteRecursively)

  println("Trying to remove temporary db directory:")
  println(dir)
  deleteRecursively(dir)
  if (dir.exists) println("(failed)")
  // val dbSize = dir.children.map(f => if (f.isFile) f.length else 0L).sum
  // println(dbSize)
}
