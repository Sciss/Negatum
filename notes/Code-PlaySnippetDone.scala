val attr = self.attr
for {
  ens     <- attr.$[Ensemble ]("context")
  ensList <- attr.$[Ensemble ]("listen" )
  recDur  <- attr.$[DoubleObj]("rec-dur")
  proc    <- invoker
} {
  println(s"Removing ${proc}")
  ens.folder.remove(proc)

  ensList.playing match {
    case BooleanObj.Var(vr) => vr() = true
    case _ => println("Ensemble listen - playing not mutable")
  }

  import de.sciss.numbers.Implicits._

  recDur match {
    case DoubleObj.Var(vr) =>
      vr() = math.random.linexp(0, 1, 8, 16).roundTo(0.1)
    case _ => println("Listen rec-dur - not mutable")
  }
}
