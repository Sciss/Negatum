package de.sciss.negatum

import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.proc.Durable

object SOMTest extends App {
  Negatum.init()

  type S = Durable
  implicit val cursor = Durable(BerkeleyDB.tmp())

  val t1 = System.currentTimeMillis()
  val somH = cursor.root { implicit tx =>
    SOM(SOM.Config(features = 48, dimensions = 2))
  }
  val t2 = System.currentTimeMillis()
  cursor.step { implicit tx =>
    val som = somH()
    val obj = IntObj.newConst[S](1234)
    som.add(key = Vector.fill(48)(math.random), value = obj)
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
