package de.sciss.negatum

import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.proc.Durable

object SOMTest extends App {
  type S = Durable
  implicit val cursor = Durable(BerkeleyDB.tmp())

  val t1 = System.currentTimeMillis()
  val somH = cursor.root { implicit tx =>
    SOM(SOM.Config(features = 48))
  }
  val t2 = System.currentTimeMillis()
  println(s"Took ${t2 - t1}ms.")
  cursor.close()
}
