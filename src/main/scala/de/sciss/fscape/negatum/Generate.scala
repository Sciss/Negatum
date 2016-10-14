///*
// *  Generate.scala
// *  (Negatum)
// *
// *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU Lesser General Public License v2.1+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.fscape.negatum
//
//import akka.stream.Outlet
//import de.sciss.fscape.graph.UGenInGroup
//import de.sciss.fscape.{GE, Lazy, UGenGraph, UGenIn, UGenInLike}
//import de.sciss.synth.SynthGraph
//
//import scala.collection.immutable.{IndexedSeq => Vec}
//
//final case class Generate(population    : GE = 500 , constProb     : GE = 0.5,
//                          minNumVertices: GE = 64  , maxNumVertices: GE = 256,
//                          nonDefaultProb: GE = 0.95, allowedUGens: String = "default")
//  extends Lazy.Expander[Outlet[SynthGraph]] {
//
//  type U = Outlet[SynthGraph]
//
//  protected def makeUGens(implicit b: UGenGraph.Builder): U =
//    unwrap(Vector(population.expand, constProb.expand, minNumVertices.expand, maxNumVertices.expand,
//      nonDefaultProb.expand))
//
//  private def rewrap(args: Vec[UGenInLike], exp: Int)(implicit b: UGenGraph.Builder): U = {
//    val sq = Vec.tabulate(exp)(i => unwrap(args.map(_.unwrap(i))))
//    ??? // UGenInGroup(sq)
//  }
//
//  protected def makeUGen(args: Vec[UGenIn])(implicit b: UGenGraph.Builder): U = ???
//
//  private def unwrap(args: Vec[UGenInLike])(implicit b: UGenGraph.Builder): U = {
//    var uIns    = Vec.empty[UGenIn]
//    var uInsOk  = true
//    var exp     = 0
//    args.foreach(_.unbubble match {
//      case u: UGenIn => if (uInsOk) uIns :+= u
//      case g: UGenInGroup =>
//        exp     = math.max(exp, g.numOutputs)
//        uInsOk  = false // don't bother adding further UGenIns to uIns
//    })
//    if (uInsOk) {
//      // aka uIns.size == args.size
//      makeUGen(uIns)
//    } else {
//      rewrap(args, exp)
//    }
//  }
//}