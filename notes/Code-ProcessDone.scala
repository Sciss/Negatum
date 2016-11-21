value match {
  case util.Success(_) =>
    println("Rendering success.")
    val attr = self.attr
    for {
      temp     <- attr.$[Proc]("template")
      ens      <- attr.$[Ensemble]("group")
      art      <- attr.$[Artifact]("file")
      //       ensList  <- attr.$[Ensemble]("context")
      playDone <- temp.attr.get("done")
    } {
      // import de.sciss.mellite.ProcActions // not pretty
      import de.sciss.synth.io.AudioFile  // not pretty
      import de.sciss.file._

      val cpy   = Proc[S]
      cpy.graph() = temp.graph()
      // ProcActions.copy(temp)
      val cpyA  = cpy.attr
      cpy.name  = art.value.name
      cpyA.put("done", playDone)
      cpyA.put("amp" , DoubleObj.newVar(0.25))
      val ca    = cpy.attr
      val spec  = AudioFile.readSpec(art.value)
      val cue   = AudioCue.Obj(art, spec, offset = 0L, gain = 1.0)
      ca.put("cue", cue)
      ca.put("gate", BooleanObj.newVar(true))
      val folder = ens.folder
      val sz = folder.size
      if (sz > 4) {
        val headAttr = folder.head.attr
        headAttr.$[BooleanObj]("gate") match {
          case Some(BooleanObj.Var(vr)) => vr() = false
          case _ => headAttr.put("gate", BooleanObj.newVar(false))
        }
      }
      folder.addLast(cpy)
      println("... added new proc.")

      //       ensList.playing match {
      //         case BooleanObj.Var(vr) => vr() = true
      //         case _ => println("Ensemble listen - playing not mutable")
      //       }
    }

  case util.Failure(FScape.Rendering.Cancelled()) =>
    println("Cancelled.")
  case util.Failure(ex) =>
    println("Rendering failed:")
    ex.printStackTrace()
  case other =>
    println(s"Expected Rendering.State, but got $other")
}
