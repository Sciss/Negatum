/*
 *  ButtonCanDrag.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.negatum
package gui
package impl

import java.awt.datatransfer.Transferable
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.{JComponent, TransferHandler}

import scala.swing.AbstractButton

trait ButtonCanDrag {
  _: AbstractButton =>

  protected def sourceActions: Int

  protected def sourceAction(modifiers: Int): Int

  protected def export(): Option[Transferable]

  private object Transfer extends TransferHandler {
    override def getSourceActions(c: JComponent): Int = sourceActions

    override def createTransferable(c: JComponent): Transferable = export().orNull
  }

  peer.setTransferHandler(Transfer)
  focusable = false

  private var dndInitX    = 0
  private var dndInitY    = 0
  private var dndPressed  = false
  private var dndStarted  = false
  private object Mouse extends MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      dndInitX	  = e.getX
      dndInitY    = e.getY
      dndPressed  = true
      dndStarted	= false
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      dndPressed  = false
      dndStarted	= false
    }

    override def mouseDragged(e: MouseEvent): Unit =
      if (dndPressed && !dndStarted && ((math.abs(e.getX - dndInitX) > 5) || (math.abs(e.getY - dndInitY) > 5))) {
        Transfer.exportAsDrag(peer, e, sourceAction(e.getModifiers))
        dndStarted = true
      }
  }

  peer.addMouseListener      (Mouse)
  peer.addMouseMotionListener(Mouse)
}
