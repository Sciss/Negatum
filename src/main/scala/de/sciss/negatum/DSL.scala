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

import de.sciss.synth.proc.{Action, Folder, Implicits, Proc}
import Implicits._
import de.sciss.file._
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{DoubleObj, IntObj}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.SynthGraph

import scala.concurrent.stm.TSet
import scala.language.implicitConversions

object DSL {
  def apply[S <: SSys[S]]: DSL[S] = new DSL
}
final class DSL[S <: SSys[S]] {
  import DSLAux._

  def proc(name: String)(implicit tx: S#Tx): ProcBuilder[S] = new ProcBuilder(name)

  def action(name: String)(implicit tx: S#Tx): ActionBuilder[S] = new ActionBuilder(name)

  def artifactLoc(dir: File)(implicit tx: S#Tx): ArtifactLocBuilder[S] = new ArtifactLocBuilder(dir)

  def int   (value: Int   )(implicit tx: S#Tx): IntObj   .Var[S] = IntObj   .newVar(value)
  def double(value: Double)(implicit tx: S#Tx): DoubleObj.Var[S] = DoubleObj.newVar(value)

  implicit def ObjAttrBuilder(in: Obj[S]): ObjAttrBuilder[S] = new ObjAttrBuilder(in)

  implicit def ArtifactLocOps(in: ArtifactLocation[S]): ArtifactLocOps[S] = new ArtifactLocOps(in)
}
object DSLAux {
  final class ProcBuilder[S <: Sys[S]](private val name: String) extends AnyVal {
    def in(f: Folder[S])(thunk: => Unit)(implicit tx: S#Tx): Proc[S] = {
      val exists = f.iterator.collectFirst {
        case p: Proc[S] if p.name == name => p
      }
      exists.getOrElse {
        val p = Proc[S]
        val g = SynthGraph(thunk)
        p.graph() = g
        p.name = name
        p
      }
    }
  }

  private val registeredActions = TSet.empty[String]

  final class ActionBuilder[S <: Sys[S]](private val name: String) extends AnyVal {
    def at(kv: (Obj[S], String))(body: Action.Body)(implicit tx: S#Tx): Action /* .Var */ [S] = {
      val (obj, key) = kv
      val attr       = obj.attr
      attr.$[Action](key) match {
        case Some(a: Action[S]) =>
//          Action.Var.unapply(a).getOrElse {
//            val av = Action.Var(a)
//            attr.put(key, av)
//            av
//          }
          a
        case None =>
          if (registeredActions.add(name)(tx.peer)) {
            Action.registerPredef(name, body)
          }
          val a   = Action.predef[S](name)
//          val av  = Action.Var(a)
//          av.name = name
//          attr.put(key, av)
//          av
          a.name = name
          attr.put(key, a)
          a
      }
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
        loc
      } { loc =>
        ArtifactLocation.Var.unapply(loc).getOrElse {
          ArtifactLocation.newVar(loc)
        }
      }
    }
  }

  final class ArtifactLocOps[S <: Sys[S]](private val in: ArtifactLocation[S]) extends AnyVal {
    def / (child: String)(implicit tx: S#Tx): Artifact.Modifiable[S] =
      Artifact(in, Artifact.Child(child))
  }

  final class ObjAttrBuilder[S <: Sys[S]](private val in: Obj[S]) extends AnyVal {
    def update[A <: Obj[S]](key: String, value: => A)(implicit tx: S#Tx): A = {
      val a = in.attr
      a.get(key).asInstanceOf[Option[A]].getOrElse {
        val res = value
        a.put(key, res)
        res
      }
    }
  }
}