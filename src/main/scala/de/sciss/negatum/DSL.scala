/*
 *  DSL.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum

import de.sciss.synth.proc._
import Implicits._
import de.sciss.file._
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj, LongObj}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.negatum.Composition.NoSys
import de.sciss.synth.SynthGraph

import scala.language.implicitConversions

object DSL {
  def apply[S <: SSys[S]]: DSL[S] = anyDSL.asInstanceOf[DSL[S]] // new DSL

  private[this] val anyDSL = new DSL[NoSys]
}
final class DSL[S <: SSys[S]] {
  import DSLAux._

  def proc(name: String)(implicit tx: S#Tx): ProcBuilder[S] = new ProcBuilder(name)

  def ensemble(name: String)(implicit tx: S#Tx): EnsembleBuilder[S] = new EnsembleBuilder(name)

  def folder(name: String)(implicit tx: S#Tx): FolderBuilder[S] = new FolderBuilder(name)

  def action(name: String)(implicit tx: S#Tx): ActionBuilder[S] = new ActionBuilder(name)

  def artifactLoc(dir: File)(implicit tx: S#Tx): ArtifactLocBuilder[S] = new ArtifactLocBuilder(dir)

  def negatum(name: String)(implicit tx: S#Tx): NegatumBuilder[S] = new NegatumBuilder(name)

  def timeline(name: String)(implicit tx: S#Tx): TimelineBuilder[S] = new TimelineBuilder(name)

  def int   (value: Int   )(implicit tx: S#Tx): IntObj   .Var[S] = IntObj   .newVar(value)
  def double(value: Double)(implicit tx: S#Tx): DoubleObj.Var[S] = DoubleObj.newVar(value)

  implicit def ObjAttrBuilder(in: Obj[S]): ObjAttrBuilder[S] = new ObjAttrBuilder(in)

  implicit def ArtifactLocOps(in: ArtifactLocation[S]): ArtifactLocOps[S] = new ArtifactLocOps(in)
}
object DSLAux {
  final class ProcBuilder[S <: Sys[S]](private val name: String) extends AnyVal {
    def in(ens: Ensemble[S])(thunk: => Unit)(implicit tx: S#Tx): Proc[S] = in(ens.folder)(thunk)

    def in(f: Folder[S])(thunk: => Unit)(implicit tx: S#Tx): Proc[S] = {
      val exists = f.iterator.collectFirst {
        case p: Proc[S] if p.name == name => p
      }
      exists.getOrElse {
        val p = Proc[S]
        val g = SynthGraph(thunk)
        p.graph() = g
        p.name = name
        f.addLast(p)
        p
      }
    }
  }

  final class EnsembleBuilder[S <: Sys[S]](private val name: String) extends AnyVal {
    def in(f: Folder[S])(initPlay: Boolean)(implicit tx: S#Tx): Ensemble[S] = {
      val exists = f.iterator.collectFirst {
        case ens: Ensemble[S] if ens.name == name => ens
      }
      val res = exists.fold {
        val _res = Ensemble[S](Folder[S], LongObj.newVar[S](0L), BooleanObj.newVar[S](initPlay))
        _res.name = name
        f.addLast(_res)
        _res
      } { ens =>
        if (ens.isPlaying != initPlay) {
          if (initPlay) ens.play() else ens.stop()
        }
        ens
      }
      res
    }
  }

  final class FolderBuilder[S <: Sys[S]](private val name: String) extends AnyVal {
    def in(ens: Ensemble[S])(implicit tx: S#Tx): Folder[S] = in(ens.folder)

    def in(f: Folder[S])(implicit tx: S#Tx): Folder[S] = {
      val exists = f.iterator.collectFirst {
        case ens: Folder[S] if ens.name == name => ens
      }
      val res = exists.getOrElse {
        val _res = Folder[S]
        _res.name = name
        f.addLast(_res)
        _res
      }
      res
    }
  }

//  private val registeredActions = TSet.empty[String]

  final class ActionBuilder[S <: Sys[S]](private val name: String) extends AnyVal {
    def in(f: Folder[S])(body: Action.Body)(implicit tx: S#Tx): Action[S] = {
      val exists = f.iterator.collectFirst {
        case a: Action[S] if a.name == name => a
      }
      exists.getOrElse {
        val a = mkAction(body)
        f.addLast(a)
        a
      }
    }

    def at(kv: (Obj[S], String))(body: Action.Body)(implicit tx: S#Tx): Action /* .Var */ [S] = {
      val (obj, key) = kv
      val attr = obj.attr
      attr.$[Action](key).getOrElse {
        val a = mkAction(body)
        attr.put(key, a)
        a
      }
    }

    private def mkAction(body: Action.Body)(implicit tx: S#Tx): Action[S] = {
//      if (registeredActions.add(name)(tx.peer)) {
//        Action.registerPredef(name, body)
//      }
      val a = Action.predef[S](name)
      a.name = name
      a
    }
  }

  final class NegatumBuilder[S <: Sys[S]](private val name: String) extends AnyVal {
    def in(f: Folder[S])(cue: AudioCue.Obj[S])(implicit tx: S#Tx): Negatum[S] = {
      val exists = f.iterator.collectFirst {
        case n: Negatum[S] if n.name == name => n
      }
      val res = exists.getOrElse {
        val n = Negatum(cue)
        n.name = name
        f.addLast(n)
        n
      }
      res
    }
  }

  final class TimelineBuilder[S <: Sys[S]](private val name: String) extends AnyVal {
    def in(ens: Ensemble[S])(implicit tx: S#Tx): Timeline.Modifiable[S] = in(ens.folder)

    def in(f: Folder[S])(implicit tx: S#Tx): Timeline.Modifiable[S] = {
      val exists = f.iterator.collectFirst {
        case tl: Timeline.Modifiable[S] if tl.name == name => tl
      }
      val res = exists.getOrElse {
        val tl = Timeline[S]
        tl.name = name
        f.addLast(tl)
        tl
      }
      res
    }
  }

  final class ArtifactLocBuilder[S <: Sys[S]](private val dir: File) extends AnyVal {
    def in(f: Folder[S])(implicit tx: S#Tx): ArtifactLocation.Var[S] = {
      val name = dir.name
      val exists = f.iterator.collectFirst {
        case loc: ArtifactLocation[S] if loc.name == name => loc
      }
      exists.fold {
        val loc = ArtifactLocation.newVar[S](dir)
        loc.name = name
        f.addLast(loc)
        loc
      } { loc =>
        val locVr = ArtifactLocation.Var.unapply(loc).getOrElse {
          ArtifactLocation.newVar(loc)
        }
        locVr
      }
    }
  }

  final class ArtifactLocOps[S <: Sys[S]](private val in: ArtifactLocation[S]) extends AnyVal {
    def / (child: String)(implicit tx: S#Tx): Artifact.Modifiable[S] =
      Artifact(in, Artifact.Child(child))
  }

  final class ObjAttrBuilder[S <: Sys[S]](private val in: Obj[S]) extends AnyVal {
    def addTo(ens: Ensemble[S])(implicit tx: S#Tx): Unit = addTo(ens.folder)

    def addTo(f: Folder[S])(implicit tx: S#Tx): Unit = {
      val name   = in.name
      val exists = f.iterator.exists(_.name == name)
      if (!exists) f.addLast(in)
    }

    def update[A <: Obj[S]](key: String, value: => A)(implicit tx: S#Tx): A = {
      val a = in.attr
      a.get(key).asInstanceOf[Option[A]].getOrElse {
        val res = value
        a.put(key, res)
        res
      }
    }

    def adjustDouble(key: String, value: Double)(implicit tx: S#Tx): DoubleObj.Var[S] = {
      val a = in.attr
      val vrOpt = a.$[DoubleObj](key).collect {
        case DoubleObj.Var(vr) => vr
      }
      vrOpt.fold[DoubleObj.Var[S]] {
        val res = DoubleObj.newVar[S](value)
        a.put(key, res)
        res
      } { vr =>
        vr() = value
        vr
      }
    }

    def adjustInt(key: String, value: Int)(implicit tx: S#Tx): IntObj.Var[S] = {
      val a = in.attr
      val vrOpt = a.$[IntObj](key).collect {
        case IntObj.Var(vr) => vr
      }
      vrOpt.fold[IntObj.Var[S]] {
        val res = IntObj.newVar[S](value)
        a.put(key, res)
        res
      } { vr =>
        vr() = value
        vr
      }
    }

    def attrInt(key: String, default: => Int)(implicit tx: S#Tx): Int =
      in.attr.$[IntObj](key).map(_.value).getOrElse(default)
  }
}