package de.sciss.negatum

import de.sciss.negatum.Delaunay.{TriangleIndex, Vector2}

object DelaunayTest extends App {
  val select   = DelaunaySpace.even  // odd // ++ even // ++ odd
  val selectS  = select.map { case Vector2(x, y) => Vector2(x * 0.5f, y * 0.5f) }

  // Delaunay.mkProcessing(selectS)

  val indices = Delaunay(selectS)
  val lines = indices.flatMap { case TriangleIndex(a, b, c) =>
    List(
      (math.min(a, b), math.max(a, b)),
      (math.min(a, c), math.max(a, c)),
      (math.min(b, c), math.max(b, c))
    )
  }
  val linesS = lines.toSet
  println(s"For input of ${select.size} points, there are ${linesS.size} unique line segments")
}
