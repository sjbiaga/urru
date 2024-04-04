package urru

import scala.collection.{ Map => AnyMap }

import grid.Game._
import grid.shape


abstract trait Move[C <: Cell, K <: Clue]
    extends shape.Color:

  val dir: (Int, Int)
  val elapsed: Long

  // move / redo ///////////////////////////////////////////////////////////////

  def apply(clues: Set[K])(filter: Point => Boolean): Map[Point, C]

  def apply(grid: AnyMap[Point, C], clues: Set[K]): Map[Point, C] =
    this(clues) { case pt if grid.contains(pt) => grid(pt).colors += color; false
                  case _ => true }

  // undo //////////////////////////////////////////////////////////////////////

  protected def apply(point: Point, grid: AnyMap[Point, C])
                     (remove: Point => AnyMap[Point, C]): AnyMap[Point, C] =
    val cs = grid(point).colors
    if cs.size > 1
    then
      cs.remove(cs.indexOf(color))
      grid
    else
      remove(point)

  def apply(grid: AnyMap[Point, C])
           (remove: Point => AnyMap[Point, C]): AnyMap[Point, C]
