package de.sciss.negatum
package impl

import java.text.NumberFormat
import java.util.Locale

import de.sciss.synth.UGenSpec.{Argument, ArgumentType, Input, Output, SignalShape}
import de.sciss.synth.ugen.{BinaryOpUGen, Constant, UnaryOpUGen}
import de.sciss.synth.{UGenGraph, UGenSpec, UndefinedRate, audio, control, scalar, demand}

import scala.collection.{breakOut, SortedMap}
import scala.language.implicitConversions
import scala.util.control.NonFatal

object TopologyDOT  {
  private val nameBoldFont: Boolean   = true
  private val nameFontSize: Int       = 16

  private def escapeHTML(in: String): String =
    in.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;")

  /** Renders to DOT, returning it as a string. */
  def apply(input: SynthGraphT): String = {
    val nf = NumberFormat.getInstance(Locale.US)
    nf.setMaximumFractionDigits(4)
    nf.setGroupingUsed(false)

    val nodeBuilder = new StringBuilder
    val edgeBuilder = new StringBuilder
    nodeBuilder.append("digraph {\n")
    val nl          = "\n"

    input.vertices.zipWithIndex.foreach { case (iu, iui) =>
      val (ugenName0, numIns, numOuts, specOpt) = iu match {
        case u: Vertex.UGen     => (u.toString, u.info.inputs.size, u.info.outputs.size, Some(u.info))
        case c: Vertex.Constant => (c.toString, 0, 1, None)
      }

      val nodeLabel = s"v$iui"
      nodeBuilder.append(s"  node [shape=plaintext] $nodeLabel [label=<\n")
      nodeBuilder.append( "      <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">\n")
      nodeBuilder.append( "      <TR>\n")
      if (numIns > 0) {
        specOpt.get.inputs.zipWithIndex.foreach {
          case (inSpec, inIdx) =>
            val inCell  = inSpec.arg
            val inLabel = s"in$inIdx"
            nodeBuilder.append(s"""        <TD PORT="$inLabel">$inCell</TD>$nl""")
        }
        nodeBuilder.append("      </TR><TR>\n")
      }
      val ugenName1 = ugenName0
      val ugenName  = escapeHTML(ugenName1)
      val nameCell0 = if (!nameBoldFont)     ugenName else s"<B>$ugenName</B>"
      val nameCell  = if (nameFontSize == 0) nameCell0 else s"""<FONT POINT-SIZE="$nameFontSize">$nameCell0</FONT>"""
      val nameCellBg = ""
      nodeBuilder.append(s"""        <TD COLSPAN="${math.max(1, math.max(numIns, numOuts))}"$nameCellBg>$nameCell</TD>$nl""")
      if (numOuts > 0) {
        nodeBuilder.append("      </TR><TR>\n")
        for (outIdx <- 0 until numOuts) {
          val outNameOpt = specOpt.flatMap { spec =>
            val outs = spec.outputs
            if (outIdx < outs.size) {
              val out = outs(outIdx)
              val n = out.name.getOrElse("out")
              Some(if (out.variadic.isDefined) s"$n$outIdx" else n)
            } else outs.lastOption.flatMap { out =>
              val n = out.name.getOrElse("out")
              if (out.variadic.isDefined) Some(s"$n${outIdx - (outs.size - 1)}") else None
            }
          }
          val outName = outNameOpt.getOrElse(if (numOuts == 1) "out" else s"out$outIdx")
          val outLabel = s"out$outIdx"
          nodeBuilder.append(s"""        <TD PORT="$outLabel">$outName</TD>$nl""")
        }
      }
      nodeBuilder.append("      </TR>\n")
      nodeBuilder.append("      </TABLE>\n")
      nodeBuilder.append("    >];\n\n")
    }
    input.edges.foreach { e =>
      assert(e.sourceVertex.isUGen)
      val vIn           = e.sourceVertex.asInstanceOf[Vertex.UGen]
      val vOut          = e.targetVertex
      val vInIdx        = input.vertices.indexOf(vIn )
      val vOutIdx       = input.vertices.indexOf(vOut)
      require(vInIdx  >= 0)
      require(vOutIdx > vInIdx)
      val inIdx         = vIn.info.inputs.indexWhere(_.arg == e.inlet)
      require(inIdx >= 0)
      val outIdx        = 0
      val inNodeLabel   = s"v$vInIdx"
      val outNodeLabel  = s"v$vOutIdx"
      val inLabel       = s"in$inIdx"
      val outLabel      = s"out$outIdx"
      edgeBuilder.append(s"  $outNodeLabel:$outLabel -> $inNodeLabel:$inLabel;\n")
    }

    nodeBuilder.append(edgeBuilder)
    nodeBuilder.append("}")

    nodeBuilder.toString()
  }
}