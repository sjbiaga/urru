package urru
package game
package fold


sealed abstract trait Clue
    extends urru.grid.Game.Clue


object Clue:

  import common.grid.{ row, col }

  import urru.grid.shape

  /**
    * Blocks' having shape (may not be compact).
    */
  case class Block(override val min: (Int, Int),
                   override val max: (Int, Int),
                   override val color: Int,
                   override val block: Point*)
      extends Clue, shape.Block, shape.Color:

    def this(color: Int, ps: Point*) =
      this(ps.map(_.row).min -> ps.map(_.col).min
          ,ps.map(_.row).max -> ps.map(_.col).max
          ,color
          ,ps*)

    def apply(that: Block, dir: (Int, Int)): Block =
      assert(that.color == color)
      val (min, max) =
        if dir._1 + dir._2 < 0
        then
          that.min -> this.max
        else /* if dir._1 + dir._2 > 0 then */
          this.min -> that.max

      Block(min, max, color, (this.block ++ that.block)*)

    def apply(size: Point, dir: (Int, Int)): Option[Block] =
      ( dir match
        case (-1, 0) => // up
          Some {
            Block(
              (2*min.row - max.row - 1) -> min.col,
              (min.row - 1) -> max.col,
              color,
              (block.map(2*min.row - _.row - 1) zip block.map(_.col))*
            )
          }
        case (1, 0) => // down
          Some {
            Block(
              (max.row + 1) -> min.col,
              (2*max.row - min.row + 1) -> max.col,
              color,
              (block.map(2*max.row - _.row + 1) zip block.map(_.col))*
            )
          }
        case (0, -1) => // left
          Some {
            Block(
              min.row -> (2*min.col - max.col - 1),
              max.row -> (min.col - 1),
              color,
              (block.map(_.row) zip block.map(2*min.col - _.col - 1))*
            )
          }
        case (0, 1) => // right
          Some {
            Block(
              min.row -> (max.col + 1),
              max.row -> (2*max.col - min.col + 1),
              color,
              (block.map(_.row) zip block.map(2*max.col - _.col + 1))*
            )
          }
        case _ => None
      ).flatMap { case it @ Block(min, max, _, _*) =>
          if 1 <= min.row && min.row <= size.row
          && 1 <= min.col && min.col <= size.col
          && 1 <= max.row && max.row <= size.row
          && 1 <= max.col && max.col <= size.col
          then
            Some(it)
          else
            None
      }

////////////////////////////////////////////////////////////////////////////////

  /**
    * Board's empty cells.
    */
  case class Empty(override val at: Point)
      extends Clue, shape.At

////////////////////////////////////////////////////////////////////////////////

  /**
    * Board's wildcard cells.
    */
  case class Multi(override val at: Point)
      extends Clue, shape.At
