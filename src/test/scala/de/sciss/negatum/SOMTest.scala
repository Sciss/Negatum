package de.sciss.negatum

import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.mellite.Mellite
import de.sciss.synth.proc.Durable

object SOMTest extends App {
  Mellite.initTypes()
  Negatum.init()

  type S = Durable
  implicit val cursor = Durable(BerkeleyDB.tmp())

  val r  = new util.Random(1L)
  val t1 = System.currentTimeMillis()
  val somH = cursor.root { implicit tx =>
    SOM(SOM.Config(features = 48, dimensions = 2, seed = 0L))
  }
  val t2 = System.currentTimeMillis()
  cursor.step { implicit tx =>
    val som = somH()
    for (i <- 0 until 1000) {
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

  cursor.close()
}
