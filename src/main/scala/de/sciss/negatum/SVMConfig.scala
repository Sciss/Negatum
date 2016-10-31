/*
 *  SVMConfig.scala
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

import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import libsvm.svm_parameter

import scala.annotation.switch
import scala.collection.breakOut
import scala.collection.immutable.{Seq => ISeq}
import scala.language.implicitConversions

object SVMConfig {
  trait Like {
    def tpe: Type
    def kernel: Kernel

    /** In megabytes */
    def cacheSize: Float

    /** Stopping criterion. */
    def epsilon: Float

    /** For using the shrinking heuristics. */
    def shrinking: Boolean

    /** To do probability estimation. */
    def probability: Boolean

    /** Whether features will be normalized based on
      * their statistics (mean and variance).
      */
    def normalize: Boolean
  }

  def apply(): Builder = new Builder

  final class Builder extends Like {
    var tpe         : Type    = Type.CSVC(c = 1.0f, weights = Nil)
    var kernel      : Kernel  = Kernel.Radial(gamma = 0.0f)
    var cacheSize   : Float   = 32f
    var epsilon     : Float   = 1e-3f
    var shrinking   : Boolean = true
    var probability : Boolean = false
    var normalize   : Boolean = true

    def build: SVMConfig = Impl(tpe = tpe, kernel = kernel, cacheSize = cacheSize,
      epsilon = epsilon, shrinking = shrinking, probability = probability, normalize = normalize)

    def read(that: SVMConfig): Unit = {
      this.tpe          = that.tpe
      this.kernel       = that.kernel
      this.cacheSize    = that.cacheSize
      this.epsilon      = that.epsilon
      this.shrinking    = that.shrinking
      this.probability  = that.probability
      this.normalize    = that.normalize
    }
  }

  implicit def build(b: Builder): SVMConfig = b.build

  final case class Weight(label: Int, value: Float)

  object Type {
    object CSVC {
      final val id = 0
    }
    case class CSVC(c: Float, weights: ISeq[Weight] = Nil) extends Type {
      def id: Int = CSVC.id

      override def toString = s"$productPrefix(c = $c, weights = $weights)"
    }

    object NuSVC {
      final val id = 1
    }
    case class NuSVC(nu: Float) extends Type {
      def id: Int = NuSVC.id

      override def toString = s"$productPrefix(nu = $nu)"
    }

    object OneClass {
      final val id = 2
    }
    case class OneClass(nu: Float) extends Type {
      def id: Int = OneClass.id

      override def toString = s"$productPrefix(nu = $nu)"
    }

    object EpsilonSVR {
      final val id = 3
    }
    case class EpsilonSVR(c: Float, p: Float) extends Type {
      def id: Int = EpsilonSVR.id

      override def toString = s"$productPrefix(c = $c, p = $p)"
    }

    object NuSVR {
      final val id = 4
    }
    case class NuSVR(c: Float, nu: Float) extends Type {
      def id: Int = NuSVR.id

      override def toString = s"$productPrefix(c = $c, nu = $nu)"
    }
  }
  sealed trait Type { def id: Int }

  object Kernel {
    case object Linear extends Kernel { final val id = 0 }

    object Poly {
      final val id = 1
    }
    case class Poly(degree: Int, gamma: Float, coef0: Float) extends Kernel {
      def id: Int = Poly.id

      override def toString = s"$productPrefix(degree = $degree, gamma = $gamma, coef0 = $coef0)"
    }

    /** Aka RBF */
    object Radial {
      final val id = 2
    }
    case class Radial(gamma: Float) extends Kernel {
      def id: Int = Radial.id

      override def toString = s"$productPrefix(gamma = $gamma)"
    }

    object Sigmoid {
      final val id = 3
    }
    case class Sigmoid(gamma: Float, coef0: Float) extends Kernel {
      def id: Int = Sigmoid.id

      override def toString = s"$productPrefix(gamma = $gamma, coef0 = $coef0)"
    }

    case object Precomputed extends Kernel { final val id = 4 }
  }
  sealed trait Kernel { def id: Int }

  private[this] final val COOKIE = 0x73766d70 // "svmp"

  implicit object serializer extends ImmutableSerializer[SVMConfig] {
    def write(config: SVMConfig, out: DataOutput): Unit = {
      import config._
      out.writeInt(0x73766d70)
      out.writeByte(tpe.id)
      tpe match {
        case Type.CSVC(c, weights) =>
          out.writeFloat(c)
          out.writeShort(weights.size)
          weights.foreach { w =>
            out.writeInt(w.label)
            out.writeFloat(w.value)
          }
        case Type.NuSVC(nu) =>
          out.writeFloat(nu)
        case Type.OneClass(nu) =>
          out.writeFloat(nu)
        case Type.EpsilonSVR(c, p) =>
          out.writeFloat(c)
          out.writeFloat(p)
        case Type.NuSVR(c, nu) =>
          out.writeFloat(c)
          out.writeFloat(nu)
      }

      out.writeByte(kernel.id)
      kernel match {
        case Kernel.Linear | Kernel.Precomputed =>
        case Kernel.Poly(deg, gamma, coef0) =>
          out.writeShort(deg)
          out.writeFloat(gamma)
          out.writeFloat(coef0)
        case Kernel.Radial(gamma) =>
          out.writeFloat(gamma)
        case Kernel.Sigmoid(gamma, coef0) =>
          out.writeFloat(gamma)
          out.writeFloat(coef0)
      }

      out.writeFloat(cacheSize)
      out.writeFloat(epsilon)
      out.writeBoolean(shrinking)
      out.writeBoolean(probability)
      out.writeBoolean(normalize)
    }

    def read(in: DataInput): SVMConfig = {
      val cookie = in.readInt()
      if (cookie != COOKIE) sys.error(s"Unexpected cookie. Found ${cookie.toHexString} - expected ${COOKIE.toHexString}")
      val tpeId = in.readByte()
      val tpe: Type = (tpeId: @switch) match {
        case Type.CSVC.id =>
          val c           = in.readFloat()
          val numWeights  = in.readShort()
          val weights     = if (numWeights == 0) Vector.empty[Weight] else Vector.fill(numWeights) {
            val lb    = in.readInt()
            val value = in.readFloat()
            Weight(label = lb, value = value)
          }
          Type.CSVC(c = c, weights = weights)
        case Type.NuSVC.id =>
          val nu = in.readFloat()
          Type.NuSVC(nu)
        case Type.OneClass.id =>
          val nu = in.readFloat()
          Type.OneClass(nu)
        case Type.EpsilonSVR.id =>
          val c = in.readFloat()
          val p = in.readFloat()
          Type.EpsilonSVR(c, p)
        case Type.NuSVR.id =>
          val c  = in.readFloat()
          val nu = in.readFloat()
          Type.NuSVR(c, nu)
      }

      val kernelId = in.readByte()
      val kernel: Kernel = (kernelId: @switch) match {
        case Kernel.Linear.id =>
          Kernel.Linear
        case Kernel.Poly.id =>
          val deg   = in.readShort()
          val gamma = in.readFloat()
          val coef0 = in.readFloat()
          Kernel.Poly(deg, gamma, coef0)
        case Kernel.Radial.id =>
          val gamma = in.readFloat()
          Kernel.Radial(gamma)
        case Kernel.Sigmoid.id =>
          val gamma = in.readFloat()
          val coef0 = in.readFloat()
          Kernel.Sigmoid(gamma, coef0)
        case Kernel.Precomputed.id =>
          Kernel.Precomputed
      }

      val cacheSize   = in.readFloat()
      val epsilon     = in.readFloat()
      val shrinking   = in.readBoolean()
      val probability = in.readBoolean()
      val normalize   = in.readBoolean()

      Impl(tpe = tpe, kernel = kernel, cacheSize = cacheSize,
        epsilon = epsilon, shrinking = shrinking, probability = probability, normalize = normalize)
    }
  }

  // ---- impl ----

  private final case class Impl(
     tpe        : Type,
     kernel     : Kernel,
     cacheSize  : Float,
     epsilon    : Float,
     shrinking  : Boolean,
     probability: Boolean,
     normalize  : Boolean
   ) extends SVMConfig {

    override def toString = s"""SVMConfig(
      |    tpe         = $tpe,
      |    kernel      = $kernel,
      |    cacheSize   = $cacheSize,
      |    epsilon     = $epsilon,
      |    shrinking   = $shrinking,
      |    probability = $probability,
      |    normalize   = $normalize
      |  )""".stripMargin

    def toLibSVM: svm_parameter = {
      val res         = new svm_parameter
      res.svm_type    = tpe.id
      res.kernel_type = kernel.id

      kernel match {
        case Kernel.Poly(degree, gamma, coef0) =>
          res.degree  = degree
          res.gamma   = gamma
          res.coef0   = coef0
        case Kernel.Radial(gamma) =>
          res.gamma   = gamma
        case Kernel.Sigmoid(gamma, coef0) =>
          res.gamma   = gamma
          res.coef0   = coef0
        case _ =>
      }

      res.cache_size  = cacheSize
      res.eps         = epsilon

      tpe match {
        case Type.CSVC(c, weights) =>
          res.C             = c
          res.nr_weight     = weights.size
          if (weights.isEmpty) {
            res.weight_label  = new Array(0)
            res.weight        = new Array(0)
          } else {
            res.weight_label  = weights.map(_.label)         (breakOut)
            res.weight        = weights.map(_.value.toDouble)(breakOut)
          }

        case Type.NuSVC(nu) =>
          res.nu  = nu
        case Type.OneClass(nu) =>
          res.nu  = nu
        case Type.EpsilonSVR(c, p) =>
          res.C   = c
          res.p   = p
        case Type.NuSVR(c, nu) =>
          res.C   = c
          res.nu  = nu
      }

      res.shrinking   = if (shrinking  ) 1 else 0
      res.probability = if (probability) 1 else 0

      res
    }
  }
}
sealed trait SVMConfig extends SVMConfig.Like {
  def toLibSVM: svm_parameter
}