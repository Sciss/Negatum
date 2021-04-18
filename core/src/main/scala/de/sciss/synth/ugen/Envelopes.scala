/*
 *  Envelopes.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package ugen

import de.sciss.synth.UGenSource.{ProductType, RefMapIn}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

sealed trait EnvGenCompanion[+A <: EnvGenLike] extends Product with ProductType[A] {
  protected implicit def floatArg(f: Float): UGenSpec.ArgumentValue.Float =
    UGenSpec.ArgumentValue.Float(f)

  protected implicit def intArg(i: Int): UGenSpec.ArgumentValue.Int =
    UGenSpec.ArgumentValue.Int(i)

  def envelopeArgs: Vec[UGenSpec.Argument]
}

trait EnvGenLike extends GE.Lazy with HasDoneFlag with AudioRated {
  def gate: GE
  def levelScale: GE
  def levelBias: GE
  def timeScale: GE

  protected def mkEnv: Env

  final protected def makeUGens: UGenInLike =
    EnvGen.ar(mkEnv, gate = gate, levelScale = levelScale, levelBias = levelBias, timeScale = timeScale)
}

case object EnvGen_ADSR extends EnvGenCompanion[EnvGen_ADSR] {
  import UGenSpec._

  override final val typeId = 700

  val envelopeArgs: Vec[UGenSpec.Argument] = Vector(
    Argument("attack"      , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.01f),rates = Map.empty),
    Argument("decay"       , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.3f), rates = Map.empty),
    Argument("sustainLevel", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.5f), rates = Map.empty),
    Argument("release"     , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty),
    Argument("peakLevel"   , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty)
  )

  override def read(in: RefMapIn, prefix: String, arity: Int): EnvGen_ADSR = {
    require (arity == 9)
    val _attack       = in.readGE()
    val _decay        = in.readGE()
    val _sustainLevel = in.readGE()
    val _release      = in.readGE()
    val _peakLevel    = in.readGE()
    val _gate         = in.readGE()
    val _levelScale   = in.readGE()
    val _levelBias    = in.readGE()
    val _timeScale    = in.readGE()
    new EnvGen_ADSR(_attack, _decay, _sustainLevel, _release, _peakLevel, _gate, _levelScale, _levelBias, _timeScale)
  }
}
final case class EnvGen_ADSR(attack: GE = 0.01f, decay: GE = 0.3f, sustainLevel: GE = 0.5f, release: GE = 1.0f,
                             peakLevel: GE = 1.0f, /* curve: Env.Curve = parametric(-4), */
                             gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.adsr(attack = attack, decay = decay, sustainLevel = sustainLevel, release = release /* , curve = curve */)
}

case object EnvGen_ASR extends EnvGenCompanion[EnvGen_ASR] {
  import UGenSpec._

  override final val typeId = 701

  val envelopeArgs: Vec[UGenSpec.Argument] = Vector(
    Argument("attack" , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.01f),rates = Map.empty),
    Argument("level"  , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty),
    Argument("release", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty)
  )

  override def read(in: RefMapIn, prefix: String, arity: Int): EnvGen_ASR = {
    require (arity == 7)
    val _attack       = in.readGE()
    val _level        = in.readGE()
    val _release      = in.readGE()
    val _gate         = in.readGE()
    val _levelScale   = in.readGE()
    val _levelBias    = in.readGE()
    val _timeScale    = in.readGE()
    new EnvGen_ASR(_attack, _level, _release, _gate, _levelScale, _levelBias, _timeScale)
  }
}
final case class EnvGen_ASR(attack: GE = 0.01f, level: GE = 1.0f, release: GE = 1.0f, /* curve: Env.Curve = parametric(-4), */
                            gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.asr(attack = attack, level = level, release = release /*, curve = curve */)
}

case object EnvGen_CutOff extends EnvGenCompanion[EnvGen_CutOff] {
  import UGenSpec._

  override final val typeId = 702

  val envelopeArgs: Vec[UGenSpec.Argument] = Vector(
    Argument("release", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.1f), rates = Map.empty),
    Argument("level"  , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty)
  )

  override def read(in: RefMapIn, prefix: String, arity: Int): EnvGen_CutOff = {
    require (arity == 6)
    val _release      = in.readGE()
    val _level        = in.readGE()
    val _gate         = in.readGE()
    val _levelScale   = in.readGE()
    val _levelBias    = in.readGE()
    val _timeScale    = in.readGE()
    new EnvGen_CutOff(_release, _level, _gate, _levelScale, _levelBias, _timeScale)
  }
}
final case class EnvGen_CutOff(release: GE = 0.1f, level: GE = 1.0f, /* curve: Env.Curve = linear, */
                               gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.cutoff(release = release, level = level /* , curve = curve */)
}

case object EnvGen_DADSR extends EnvGenCompanion[EnvGen_DADSR] {
  import UGenSpec._

  override final val typeId = 703

  val envelopeArgs: Vec[UGenSpec.Argument] = Vector(
    Argument("delay"       , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.1f), rates = Map.empty),
    Argument("attack"      , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.01f),rates = Map.empty),
    Argument("decay"       , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.3f), rates = Map.empty),
    Argument("sustainLevel", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.5f), rates = Map.empty),
    Argument("release"     , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty),
    Argument("peakLevel"   , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty)
  )

  override def read(in: RefMapIn, prefix: String, arity: Int): EnvGen_DADSR = {
    require (arity == 10)
    val _delay        = in.readGE()
    val _attack       = in.readGE()
    val _decay        = in.readGE()
    val _sustainLevel = in.readGE()
    val _release      = in.readGE()
    val _peakLevel    = in.readGE()
    val _gate         = in.readGE()
    val _levelScale   = in.readGE()
    val _levelBias    = in.readGE()
    val _timeScale    = in.readGE()
    new EnvGen_DADSR(_delay, _attack, _decay, _sustainLevel, _release, _peakLevel, _gate, _levelScale, _levelBias, _timeScale)
  }
}
final case class EnvGen_DADSR(delay: GE = 0.1f, attack: GE = 0.01f, decay: GE = 0.3f, sustainLevel: GE = 0.5f,
                              release: GE = 1.0f, peakLevel: GE = 1.0f, /* curve: Env.Curve = parametric(-4), */
                              gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.dadsr(delay = delay, attack = attack, decay = decay, sustainLevel = sustainLevel,
      release = release /*, curve = curve */)
}

case object EnvGen_Linen extends EnvGenCompanion[EnvGen_Linen] {
  import UGenSpec._

  override final val typeId = 704

  val envelopeArgs: Vec[UGenSpec.Argument] = Vector(
    Argument("attack" , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.01f),rates = Map.empty),
    Argument("sustain", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty),
    Argument("release", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty),
    Argument("level"  , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty)
  )

  override def read(in: RefMapIn, prefix: String, arity: Int): EnvGen_Linen = {
    require (arity == 8)
    val _attack       = in.readGE()
    val _sustain      = in.readGE()
    val _release      = in.readGE()
    val _level        = in.readGE()
    val _gate         = in.readGE()
    val _levelScale   = in.readGE()
    val _levelBias    = in.readGE()
    val _timeScale    = in.readGE()
    new EnvGen_Linen(_attack, _sustain, _release, _level, _gate, _levelScale, _levelBias, _timeScale)
  }
}
final case class EnvGen_Linen(attack: GE = 0.01f, sustain: GE = 1.0f, release: GE = 1.0f, level: GE = 1.0f,
                              /* curve: Env.Curve = linear, */
                              gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.linen(attack = attack, sustain = sustain, release = release, level = level /*, curve = curve */)
}

case object EnvGen_Perc extends EnvGenCompanion[EnvGen_Perc] {
  import UGenSpec._

  override final val typeId = 705

  val envelopeArgs: Vec[UGenSpec.Argument] = Vector(
    Argument("attack" , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.01f),rates = Map.empty),
    Argument("release", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty),
    Argument("level"  , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty)
  )

  override def read(in: RefMapIn, prefix: String, arity: Int): EnvGen_Perc = {
    require (arity == 7)
    val _attack       = in.readGE()
    val _release      = in.readGE()
    val _level        = in.readGE()
    val _gate         = in.readGE()
    val _levelScale   = in.readGE()
    val _levelBias    = in.readGE()
    val _timeScale    = in.readGE()
    new EnvGen_Perc(_attack, _release, _level, _gate, _levelScale, _levelBias, _timeScale)
  }
}
final case class EnvGen_Perc(attack: GE = 0.01f, release: GE = 1.0f, level: GE = 1.0f, /* curve: Env.Curve = parametric(-4), */
                             gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.perc(attack = attack, release = release, level = level /* , curve = curve */)
}

case object EnvGen_Sine extends EnvGenCompanion[EnvGen_Sine] {
  import UGenSpec._

  override final val typeId = 706

  val envelopeArgs: Vec[UGenSpec.Argument] = Vector(
    Argument("dur", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1f), rates = Map.empty),
    Argument("level", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1f), rates = Map.empty)
  )

  override def read(in: RefMapIn, prefix: String, arity: Int): EnvGen_Sine = {
    require (arity == 6)
    val _dur          = in.readGE()
    val _level        = in.readGE()
    val _gate         = in.readGE()
    val _levelScale   = in.readGE()
    val _levelBias    = in.readGE()
    val _timeScale    = in.readGE()
    new EnvGen_Sine(_dur, _level, _gate, _levelScale, _levelBias, _timeScale)
  }
}
final case class EnvGen_Sine(dur: GE = 1.0f, level: GE = 1.0f,
                             gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.sine(dur = dur, level = level)
}

case object EnvGen_Triangle extends EnvGenCompanion[EnvGen_Triangle] {
  import UGenSpec._

  override final val typeId = 707

  val envelopeArgs: Vec[UGenSpec.Argument] = Vector(
    Argument("dur"  , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1f), rates = Map.empty),
    Argument("level", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1f), rates = Map.empty)
  )

  override def read(in: RefMapIn, prefix: String, arity: Int): EnvGen_Triangle = {
    require (arity == 6)
    val _dur          = in.readGE()
    val _level        = in.readGE()
    val _gate         = in.readGE()
    val _levelScale   = in.readGE()
    val _levelBias    = in.readGE()
    val _timeScale    = in.readGE()
    new EnvGen_Triangle(_dur, _level, _gate, _levelScale, _levelBias, _timeScale)
  }
}
final case class EnvGen_Triangle(dur: GE = 1.0f, level: GE = 1.0f,
                                 gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.triangle(dur = dur, level = level)
}
