/*
 *  SOMImpl.scala
 *  (SVMModel)
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
package impl

import de.sciss.lucre.data.{SkipList, SkipOctree}
import de.sciss.lucre.event.{Dummy, Event, EventLike}
import de.sciss.lucre.geom.IntSpace.{ThreeDim, TwoDim}
import de.sciss.lucre.geom.{DistanceMeasure, IntCube, IntDistanceMeasure2D, IntDistanceMeasure3D, IntPoint2D, IntPoint3D, IntSpace, IntSquare, Space}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.negatum.SOM.Config
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer}
import de.sciss.synth.proc.Folder

import scala.concurrent.blocking
import scala.language.existentials

object SOMImpl {
  private object Node {
    implicit def serializer[S <: Sys[S], D <: Space[D]](implicit space: D): Serializer[S#Tx, S#Acc, Node[S, D]] =
      new Ser[S, D]

    private final class Ser[S <: Sys[S], D <: Space[D]](implicit space: D)
      extends Serializer[S#Tx, S#Acc, Node[S, D]] {

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Node[S, D] = {
        // val index = in.readInt()
        val key   = space.pointSerializer.read(in)
        val value = Obj.read(in, access)
        Node(/* index = index, */ key = key, value = value)
      }

      def write(p: Node[S, D], out: DataOutput): Unit = {
        // out.writeInt(p.index)
        space.pointSerializer.write(p.key, out)
        p.value.write(out)
      }
    }

    implicit def view[S <: Sys[S], D <: Space[D]]: (Node[S, D], S#Tx) => D#PointLike = (p, tx) => p.key
  }
  private final case class Node[S <: Sys[S], D <: Space[D]](/* index: Int, */ key: D#Point, value: Obj[S])

  private object Value {
    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Value[S]] =
      anyValSer.asInstanceOf[ValSer[S]]

    private[this] val anyValSer = new ValSer[NoSys]

    private final class ValSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Value[S]] {
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Value[S] = {
        val sz    = in.readInt()
        val arr   = new Array[Float](sz)
        var i = 0
        while (i < sz) {
          arr(i) = in.readFloat()
          i += 1
        }
        val obj = Obj.read(in, access)
        new Value(features = arr, obj = obj)
      }

      def write(v: Value[S], out: DataOutput): Unit = {
        val arr = v.features
        val sz  = arr.length
        out.writeInt(sz)
        var i = 0
        while (i < sz) {
          out.writeFloat(arr(i))
          i += 1
        }
        v.obj.write(out)
      }
    }
  }
  private final class Value[S <: Sys[S]](val features: Array[Float], val obj: Obj[S])

  private final class Index[S <: Sys[S]](val index: Int, val value: Value[S])

  private final class Lattice(var iter: Int, val data: Array[Float], val occupied: Array[Int]) {
    def copy(): Lattice = new Lattice(iter = iter, data = data.clone(), occupied = occupied.clone())

    def isOccupied(index: Int): Boolean = {
      val occIdx  = index >>> 5
      val bit     = index & 0x1F
      val mask    = 1 << bit
      (occupied(occIdx) & mask) == mask
    }

    def setOccupied(index: Int, state: Boolean): Unit = {
      val occIdx  = index >>> 5
      val bit     = index & 0x1F
      val mask    = 1 << bit
      val valOld  = occupied(occIdx)
      val valNew  = if (state) valOld | mask else valOld & ~mask
      occupied(occIdx) = valNew
    }
  }

  private implicit object LatticeSer extends ImmutableSerializer[Lattice] {
    def read(in: DataInput): Lattice = {
      val iter  = in.readInt()
      val sz    = in.readInt()
      val arr   = new Array[Float](sz)
      var i     = 0
      while (i < sz) {
        arr(i) = in.readFloat()
        i += 1
      }
      val occSz = (sz + 31) >>> 5
      i         = 0
      val occ   = new Array[Int](occSz)
      while (i < occSz) {
        occ(i) = in.readInt()
        i += 1
      }
//      val occ   = new Array[Boolean](sz)
//      i = 0
//      while (i < sz) {
//        var j = in.readInt()
//        val k = i + 32
//        val m = if (k <= sz) k else sz
//        var n = 0
//        while (i < m) {
//          occ(i) = (j & 1) == 1
//          j >>= 1
//          i  += 1
//          n  += 1
//        }
//      }
      new Lattice(iter, arr, occ)
    }

    def write(lattice: Lattice, out: DataOutput): Unit = {
      out.writeInt(lattice.iter)
      val arr = lattice.data
      val sz  = arr.length
      out.writeInt(sz)
      var i = 0
      while (i < sz) {
        out.writeFloat(arr(i))
        i += 1
      }
      val occSz = (sz + 31) >>> 5
      i         = 0
      val occ   = lattice.occupied
      while (i < occSz) {
        out.writeInt(occ(i))
        i += 1
      }
    }
  }

  private object SpaceHelper {
    implicit object TwoDim extends SpaceHelper[IntSpace.TwoDim] {
      def space = IntSpace.TwoDim

      def toPoint(index: Int, config: Config): IntPoint2D = {
        val ext         = config.extent
        val grid        = config.gridStep
        val ext2        = ext << 1
        val numLatSide  = ext2 / grid
        val x           = ((index % numLatSide) * grid) // - ext
        val y           = ((index / numLatSide) * grid) // - ext
        IntPoint2D(x = x, y = y)
      }

      def toPoint(comp: Seq[Int]): IntPoint2D = {
        val Seq(x, y) = comp
        IntPoint2D(x = x, y = y)
      }

      def metric: DistanceMeasure[_, IntSpace.TwoDim] = IntDistanceMeasure2D.euclideanSq

      // def toIndex(p: IntPoint2D, config: Config): Int = ...
    }

    implicit object ThreeDim extends SpaceHelper[IntSpace.ThreeDim] {
      def space = IntSpace.ThreeDim

      def toPoint(index: Int, config: Config): IntPoint3D = {
        val ext         = config.extent
        val grid        = config.gridStep
        val ext2        = ext << 1
        val numLatSide  = ext2 / grid
        val x           = ((index % numLatSide) * grid) // - ext
        val dec1        = index / numLatSide
        val y           = ((dec1  % numLatSide) * grid) // - ext
        val dec2        = dec1 / numLatSide
        val z           = ( dec2                * grid) // - ext
        IntPoint3D(x = x, y = y, z = z)
      }

      def toPoint(comp: Seq[Int]): IntPoint3D = {
        val Seq(x, y, z) = comp
        IntPoint3D(x = x, y = y, z = z)
      }

      def metric: DistanceMeasure[_, IntSpace.ThreeDim] = IntDistanceMeasure3D.euclideanSq

      // def toIndex(p: IntPoint3D, config: Config): Int = ...
    }
  }
  private trait SpaceHelper[D <: Space[D]] {
    implicit def space: D
    def toPoint(index: Int, config: Config): D#Point
    def toPoint(comp: Seq[Int]): D#Point
    def metric: DistanceMeasure[_, D]
    // def toIndex(p: D#Point, config: Config): Int
  }

  def apply[S <: Sys[S]](config: Config)(implicit tx: S#Tx): SOM[S] = {
    import config.{extent, gridStep, seed}

    val random      = new util.Random(seed)
    val dim         = config.dimensions
    val feat        = config.features
    require (dim > 0 && extent > 1 && gridStep > 0 && gridStep <= extent)
    if (extent >= 0x40000000) sys.error("Integer overflow")
    val numLatSide  = (extent << 1) / gridStep
    val numLattice  = {
      var res = 1
      var i = 0
      while (i < dim) {
        val m = res * numLatSide
        if (m < res) sys.error("Integer overflow")
        res = m
        i += 1
      }
      res
    }
    // lattice is a flat array of a virtual size `numLattice`
    // where the neighbouring elements are the feature vector.
    // we don't need to store the "coordinate" in the lattice,
    // because we will simply pass the offset around when we need it.
    val latSz       = numLattice * feat
    val lattice0    = new Array[Float](latSz)
    var i = 0
    while (i < latSz) {
      var j = 0
      while (j < feat) {
        lattice0(i) = random.nextFloat()
        i += 1
        j += 1
      }
    }
    val id          = tx.newID()
    val occSz       = (latSz + 31) >>> 5
    val occupied0   = new Array[Int](occSz) // all zero which is non-occupied
    val lattice     = tx.newVar[Lattice](id, new Lattice(iter = 0, data = lattice0, occupied = occupied0))
    val list        = SkipList.Map.empty[S, Int, Value[S]]()

    if (dim == 2) {
      val quad  = IntSquare(extent, extent, extent)
      val map   = SkipOctree.empty[S, TwoDim, Node[S, TwoDim]](quad)
      new Impl(id, config, lattice = lattice, map = map, list = list)
    } else if (dim == 3) {
      val cube  = IntCube(extent, extent, extent, extent)
      val map   = SkipOctree.empty[S, ThreeDim, Node[S, ThreeDim]](cube)
      new Impl(id, config, lattice = lattice, map = map, list = list)
    } else {
      ???
    }
  }

  private final val SER_VERSION = 1

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, SOM[S]] = anySer.asInstanceOf[Ser[S]]

  private[this] val anySer = new Ser[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, SOM[S]] {
    def tpe: Obj.Type = SOM
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): SOM[S] = {
    val ver     = in.readByte()
    if (ver != SER_VERSION) sys.error(s"Unexpected version $ver -- expected $SER_VERSION")
    val id      = tx.readID(in, access)
    val config  = Config.serializer.read(in)
    val lattice = tx.readVar[Lattice](id, in)
    val list    = SkipList.Map.read[S, Int, Value[S]](in, access)

    import config.{dimensions => dim}
    if (dim == 2) {
      val map = SkipOctree.read  [S, TwoDim, Node[S, TwoDim]](in, access)
      new Impl(id, config, lattice = lattice, map = map, list = list)
    } else if (dim == 3) {
      val map = SkipOctree.read[S, ThreeDim, Node[S, ThreeDim]](in, access)
      new Impl(id, config, lattice = lattice, map = map, list = list)
    } else {
      ???
    }
  }

  private type TreeImpl[S <: Sys[S], D <: Space[D]] = SkipOctree[S, D, Node[S, D]]
  private type ListImpl[S <: Sys[S]]                = SkipList.Map[S, Int, Value[S]]

  private def copyTree[In <: Sys[In], Out <: Sys[Out], D <: Space[D]](
       in: TreeImpl[In, D], out: TreeImpl[Out, D], outImpl: Impl[Out, D])
      (implicit txIn: In#Tx, txOut: Out#Tx, context: Copy[In, Out]): Unit = {

    in.iterator.foreach { nodeIn =>
      val valueOut = context(nodeIn.value)
      val nodeOut  = Node(/* index = nodeIn.index, */ key = nodeIn.key, value = valueOut)
      out.add(nodeOut)
      //      xsOut.foreach { entry =>
      //        outImpl.changed += entry
      //      }
    }
  }

  private def copyList[In <: Sys[In], Out <: Sys[Out], D <: Space[D]](
       in: ListImpl[In], out: ListImpl[Out], outImpl: Impl[Out, D])
      (implicit txIn: In#Tx, txOut: Out#Tx, context: Copy[In, Out]): Unit = {

    in.iterator.foreach { case (index, valueIn) =>
      val objOut    = context(valueIn.obj)
      val valueOut  = new Value(features = valueIn.features, obj = objOut)
      out.add(index -> valueOut)
    }
  }

//  // _not_ sqrt any longer -- since we don't need it to find the NN
//  private def weightDist(w1: Weight, w2: Weight): Double = {
//    // def norm(v: Array[Double], min: Array[Double], max: Array[Double]): Vec[Double] =
//    //  (0 until v.length).map { i => v(i).linlin(min(i), max(i), 0, 1) }
//
//    def sqrDifSum /* Sqrt */(a: Array[Double], b: Array[Double]): Double = {
//      var i = 0
//      var sum = 0.0
//      while (i < a.length) {
//        val d = a(i) - b(i)
//        sum += d * d
//        i += 1
//      }
//      sum // math.sqrt(sum)
//    }
//
//    val w1sn = w1.spectral // norm(w1.spectral, featSpecMin, featSpecMax)
//    val w2sn = w2.spectral // norm(w2.spectral, featSpecMin, featSpecMax)
//    val spectDist = sqrDifSum /* Sqrt */(w1sn, w2sn) // (w1sn zip w2sn).map { tup => (tup._1 - tup._2).squared } .sum.sqrt
//
//    val w1tn = w1.temporal // norm(w1.temporal, featTempMin, featTempMax)
//    val w2tn = w2.temporal // norm(w2.temporal, featTempMin, featTempMax)
//    val tempDist = sqrDifSum /* Sqrt */(w1tn, w2tn) // (w1tn zip w2tn).map { tup => (tup._1 - tup._2).squared } .sum.sqrt
//
//    spectDist + tempDist // / 2
//  }

  private def toArray(in: Vec[Double]): Array[Float] = {
    val sz  = in.size
    val arr = new Array[Float](sz)
    var i = 0
    while (i < sz) {
      arr(i) = in(i).toFloat
      i += 1
    }
    arr
  }

  private def bmu(lat: Array[Float], iw: Array[Float]): Int = {
    var bestDist  = Double.MaxValue
    val features  = iw.length
    var bestNode  = -1
    val sz        = lat.length
    var i         = 0
    while (i < sz) {
      var dist = 0.0
      var j = 0
      val i0 = i
      while (j < features) {
        val d = iw(j) - lat(i)
        dist += d * d
        // abort early if we will not be able to improve `bestDist`
        // XXX TODO --- currently I/O dominates, so we cannot assess whether this
        // improves the situation or not
        if (dist > bestDist) {
          j = features
          i = i0 + features
        } else {
          j += 1
          i += 1
        }
      }
      if (dist < bestDist) {
        bestDist = dist
        bestNode = i0
      }
    }
    if (bestNode < 0) throw new IllegalStateException
    bestNode / features
  }

//  private def neighbourhoodRadiusSqr(iter: Double) = mapRadiusSqr * math.exp(-iter / timeConstant2)

//  def bmuNeighboursSqr(radiusSqr: Double, bmu: PlacedWeight, lattice: Lattice): Iterator[Dist] =
//    lattice.nodes.iterator.map(n => Dist(n, coordDistSqr(n.coord, bmu.coord))).filter(_.radius <= radiusSqr)

  private def toCoord(index: Int, config: Config, res: Array[Int]): Array[Int] = {
    val dim         = config.dimensions
    val ext         = config.extent
    val grid        = config.gridStep
    val ext2        = ext << 1
    val numLatSide  = ext2 / grid
    val arr         = if (res == null) new Array[Int](dim) else res
    var dec         = index
    var i           = 0
    while (i < dim) {
      arr(i) = ((dec % numLatSide) * grid) // - ext
      dec   /= numLatSide
      i     += 1
    }
    arr
  }

  private def nextLattice[S <: Sys[S], D <: Space[D]](lattice: Lattice, dirty: Array[Boolean], config: Config,
                                                      keyArr: Array[Float]): Int = {
    import config.{extent, numIterations}
    import lattice.iter
    val dim           = config.dimensions
    val feat          = config.features
    val latArr        = lattice.data
    val bmuNodeIdx    = bmu(latArr, keyArr)
    // val radiusSqr     = neighbourhoodRadiusSqr(iter)
    val mapRadius     = extent.toDouble
    val mapRadiusSqr  = math.pow(mapRadius, dim)
    // XXX TODO --- with the successive `log` and `exp`, can we not calculate `radiusSqr` directly without these ops?
    val timeConstant  = numIterations / math.log(mapRadius)
    val timeConstant2 = timeConstant / 2
    val radiusSqr     = mapRadiusSqr * math.exp(-iter / timeConstant2)
    val radiusSqr2Rec = 1.0 / (radiusSqr * 2)
    // val inNodeIter    = bmuNeighboursSqr(radiusSqr, bmuNode, lattice)
    val learningRate  = config.learningCoef * math.exp(-iter / numIterations) // decays over time

    val bmuCoord      = toCoord(bmuNodeIdx, config, null)
    val latCoord      = new Array[Int](dim)

    var i = 0
    var j = 0
    while (i < latArr.length) {
      toCoord(index = j, config = config, res = latCoord)
      var dist = 0.0
      var k    = 0
      while (k < dim) {
        val d = bmuCoord(k) - latCoord(k)
        dist += d * d
        k    += 1
      }
      if (dist > radiusSqr) i += feat else {
        // val tTheta = thetaSqr(dist, radiusSqr)
        val tTheta  = math.exp(-dist * radiusSqr2Rec) // learning proportional to distance
        val lt      = (learningRate * tTheta).toFloat
        // adjust(randomInput.weight, dist.node.weight, lRate, tTheta)
        // for each lattice-weight nW and input-weight (keyArr) iW, replace nW by nW + lt * (iW - nW)
        k = 0
        while (k < feat) {
          latArr(i) += lt * (keyArr(k) - latArr(i))
          k += 1
          i += 1
        }
        if (lattice.isOccupied(j)) dirty(j) = true
      }
      j += 1
    }

    bmuNodeIdx
  }

  private[this] final val DEBUG = true
  private def log(what: => String): Unit = if (DEBUG) println(what)

  private final class AddAllImpl[S <: Sys[S], D <: Space[D]](somH: stm.Source[S#Tx, Impl[S, D]],
                                              folderH: stm.Source[S#Tx, Folder[S]], config: Config)
                                              (implicit protected val cursor: stm.Cursor[S])
    extends RenderingImpl[S, Int, Int] {

    override def toString = s"SOM.addAll@${hashCode.toHexString}"

    protected def fillResult(out: Int)(implicit tx: S#Tx): Int = out

    protected def body(): Int = blocking {
      ???
    }
  }

  private final class Impl[S <: Sys[S], D <: Space[D]](val id: S#ID, val config: Config,
                                                       lattice: S#Var[Lattice],
                                                       list: ListImpl[S], map: TreeImpl[S, D])
                                                      (implicit spaceHelper: SpaceHelper[D])
    extends SOM[S] {

    def tpe: Obj.Type = SOM

    def addAll(f: Folder[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): Rendering[S, Int] = ???

    def add(key: Vec[Double], obj: Obj[S])(implicit tx: S#Tx): Unit = {
      val lat       = lattice().copy()  // for InMemory, we might get into trouble if we don't isolate the arrays
      val dirty     = new Array[Boolean](lat.data.length / config.features)
      val keyArr    = toArray(key)
      val newIndex  = nextLattice(lattice = lat, dirty = dirty, config = config, keyArr = keyArr)

      val newValue  = new Value(features = keyArr, obj = obj)
      val newPoint  = spaceHelper.toPoint(newIndex, config)
      log(f"-- will add    $obj%3s at index $newIndex%6d / $newPoint")

      cleanUp(lat, dirty)

      lat.iter     += 1
      lat.setOccupied(newIndex, state = true)
      lattice()     = lat

//      if (latOut.iter == 37) {
//        println("AQUI")
//      }

      val newNode   = Node(newPoint, obj)
      /* val newInList = */ list.add(newIndex -> newValue) // .forall(_ != newValue)
      /* val newInMap  = */ map .add(newNode)
      // assert(map.isDefinedAt(newPoint))

      // assert(newInList == newInMap)
//      if (newInList != newInMap) {
//        println(s"ERROR: iter ${latOut.iter} object $obj (index $newIndex / $newPoint) was ${if (newInList) "new" else "old"} in list, but ${if (newInMap) "new" else "old"} in map.")
//        println(s"List keys = ${list.keysIterator.mkString(", ")}")
//        println(s"Map keys = ${map.iterator.map(_.key).mkString(", ")}")
//        assert(false)
//      }
    }

    private def cleanUp(lat: Lattice, dirty: Array[Boolean])(implicit tx: S#Tx): Unit = {
      var i = 0
      var removed = List.empty[Index[S]]
      while (i < dirty.length) {
        if (dirty(i)) {
          val value     = list.get(i).get
          val newIndex  = bmu(lat = lat.data, iw = value.features)
          if (newIndex != i) {
            list.remove(i)
            val point     = spaceHelper.toPoint(i, config)
            val nodeOpt   = map.removeAt(point)
            assert(nodeOpt.isDefined)
            removed     ::= new Index(newIndex, value)
            lat.setOccupied(i, state = false)
            log(f"clean - remove ${value.obj}%3s at index $i%6d / $point")
          }
        }
        i += 1
      }
      if (removed.nonEmpty) {
        removed.foreach { indexed =>
          val value     = indexed.value
          val obj       = value.obj
          val newIndex  = indexed.index
          val newPoint  = spaceHelper.toPoint(newIndex, config)
          val newNode   = Node(newPoint, obj)
          /* val newInList = */ list.add(newIndex -> value) // .forall(_ != value)
          /* val newInMap = */ map .add(newNode)
          // assert(map.isDefinedAt(newPoint))
          lat.setOccupied(newIndex, state = true)

          log(f"clean - add    $obj%3s at index $newIndex%6d / $newPoint")

          // assert(newInList == newInMap)
//          if (newInList != newInMap) {
//            println(s"ERROR: iter ${lattice.iter} object $obj (index $newIndex / $newPoint) was ${if (newInList) "new" else "old"} in list, but ${if (newInMap) "new" else "old"} in map.")
//            println(s"List keys = ${list.keysIterator.mkString(", ")}")
//            println(s"Map keys = ${map.iterator.map(_.key).mkString(", ")}")
//            assert(false)
//          }
        }
      }
    }

    def debugStats()(implicit tx: S#Tx): String = {
      val lat     = lattice()
      val latSz   = lat.data.length / config.features
      val numOcc  = (0 until latSz).count(lat.isOccupied)
      val s1      = s"list.size = ${list.size}; map.size = ${map.size}; iter = ${lat.iter}"
      val s2      = f"$s1; lattice size = $latSz; occupied = $numOcc (${numOcc.toDouble/latSz*100}%1.1f%%)"
      s2
    }

//    def iteration(implicit tx: S#Tx): Int = iter()

    def query(point: Seq[Int])(implicit tx: S#Tx): Option[Obj[S]] = {
      val p = spaceHelper.toPoint(point)
      map.nearestNeighborOption(p, spaceHelper.metric).map(_.value)
    }

    def event(slot: Int): Event[S, Any] = throw new UnsupportedOperationException

    final def changed: EventLike[S, Any] = Dummy[S, Any]

    def write(out: DataOutput): Unit = {
      out.writeInt(tpe.typeID)
      out.writeByte(SER_VERSION)
      id     .write(out)
      Config.serializer.write(config, out)
      lattice.write(out)
      list   .write(out)
      map    .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id     .dispose()
      lattice.dispose()
      list   .dispose()
      map    .dispose()
    }

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val idOut       = txOut.newID()
      val latticeOut  = txOut.newVar(idOut, lattice())
      val listOut     = SkipList.Map.empty[Out, Int, Value[Out]]()
      import spaceHelper.space
      val mapOut      = SkipOctree.empty[Out, D, Node[Out, D]](map.hyperCube)
      val out         = new Impl(id = idOut, config = config, lattice = latticeOut, map = mapOut, list = listOut)
      context.defer(this, out) {
        copyList(list, listOut, out)
        copyTree(map , mapOut , out)
      }
      out
    }
  }
}