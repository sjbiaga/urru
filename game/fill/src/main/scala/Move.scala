package urru
package game
package fill

import scala.collection.{ Map => AnyMap }
import scala.collection.mutable.{ ListBuffer => MutableList }

import common.grid.{ row, x, col, unary_-, + }

import urru.grid.shape

import Clue.{ Block, Multi }


case class Move(
  override val dir: (Int, Int),
  block: Block,
  override val color: Int,
  override val elapsed: Long
) extends urru.Move[Cell, Clue], shape.By:

  override val by = block.delta + (1 x 1)
  override val at = by + -dir

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

  def apply(ps: Point*): Block =
    Block(block.delta,
          block.min, block.max,
          block.up, block.down,
          block.left, block.right,
          block.color,
          ps.map(_ + block.delta)*)

object Move:

  def apply(block: Block,
            dnd: Boolean = true,
            empty: Boolean = false) =
    new Move(if dnd then (1, -1) else (0, 0),
             if empty then
               Block(block.delta,
                     block.min, block.max,
                     block.up, block.down,
                     block.left, block.right,
                     block.color)
             else block,
             block.color, -1)
