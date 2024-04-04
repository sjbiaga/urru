package urru
package game
package flow

import scala.collection.mutable.{ ListBuffer => MutableList }


case class Cell(override val colors: MutableList[Int],
                clue: Option[Clue] = None)
    extends urru.grid.Game.Cell:
  override val group: Seq[Cell] = Seq(this)
