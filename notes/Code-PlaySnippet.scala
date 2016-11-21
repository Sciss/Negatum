(1: GE).poll(0, "snippet play")
val speed = "speed".kr(0.95)
val in  = VDiskIn.ar("cue", loop = 1, speed = speed)
val gate = "gate".kr(1)
val env  = Env.asr(attack = 8, release = 8)
val amp  = "amp".kr(1)
val eg   = EnvGen.ar(env, gate = gate, levelScale = amp)
val sig  = in * eg
Out.ar(0, sig)  // XXX TODO --- proper routing

val done = Done.kr(eg)
Action(done, "done")
StopSelf(done)