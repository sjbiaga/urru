package urru
package game
package fold

import scala.collection.{ Map => AnyMap }
import scala.collection.mutable.{ ListBuffer => MutableList }

import Clue.{ Block, Multi }


case class Move(
  override val dir: (Int, Int),
  block: Block,
  override val color: Int,
  override val elapsed: Long
) extends urru.Move[Cell, Clue]:

  // move / redo
  override def apply(clues: Set[Clue])(filter: Point => Boolean) = block
    .block
    .filter(filter)
    .map { pt =>
      pt ->
      Cell(MutableList(color),
           clues.find {
             case Multi(`pt`) => true
             case _ => false
           }
      )
    }.toMap

  // undo
  override def apply(grid: AnyMap[Point, Cell])
                    (remove: Point => AnyMap[Point, Cell]): AnyMap[Point, Cell] =
    var r = grid
    for
      pt <- block.block
    do
      r = this(pt, r)(remove)
    r
