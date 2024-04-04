package urru
package game
package fill


sealed abstract trait Clue
    extends urru.grid.Game.Clue


object Clue:

  import common.grid.{ adj2, row, x, col, unary_!, + }

  import urru.grid.shape

  /**
    * Blocks' having (compact) shape.
    */
  case class Block(delta: Point,
                   override val min: (Int, Int),
                   override val max: (Int, Int),
                   override val up: Seq[Int],
                   override val down: Seq[Int],
                   override val left: Seq[Int],
                   override val right: Seq[Int],
                   override val color: Int,
                   override val block: Point*)
      extends Clue, shape.Block, shape.Color, shape.Sides:

    private def this(delta: Point, color: Int, block: Point*)
                    (up: Seq[Int], down: Seq[Int], left: Seq[Int], right: Seq[Int]) =
      this(delta, up.min x left.min, down.max x right.max,
           up, down, left, right, color, block*)

    private def this(delta: Point, color: Int, ps: Point*)
                    (up_down: Iterable[Seq[Point]], left_right: Iterable[Seq[Point]]) =
      this(delta, color, ps*)
          (up_down.map(_.head.row).toSeq
          ,up_down.map(_.last.row).toSeq
          ,left_right.map(_.head.col).toSeq
          ,left_right.map(_.last.col).toSeq)

    private def this(delta: Point, color: Int, ps: Point*) =
      this(delta, color, ps*)
          (ps.map(_.swap).sorted.map(_.swap).groupBy(_.col).values
          ,ps.groupBy(_.row).values)

    def this(color: Int, ps: Point*) =
      this(0 x 0, color, ps.sorted*)

////////////////////////////////////////////////////////////////////////////////

    def apply(size: Point)(dir: (Int, Int)): Option[Set[Point]] =
      if !dir
      then
        Some(block.toSet)
      else dir match
        case (-1, 0) if 1 < min.row => Some(up.zipWithIndex.map { (row, col) => row-1 x col+1+delta.col }.toSet)
        case (1, 0) if max.row < size.row => Some(down.zipWithIndex.map { (row, col) => row+1 x col+1+delta.col }.toSet)
        case (0, -1) if 1 < min.col => Some(left.zipWithIndex.map { (col, row) => row+1+delta.row x col-1 }.toSet)
        case (0, 1) if max.col < size.col => Some(right.zipWithIndex.map { (col, row) => row+1+delta.row x col+1 }.toSet)
        case (1, 1) if 1 <= min.row && max.row <= size.row && 1 <= min.col && max.col <= size.col => Some(block.toSet)
        case _ => None

    def apply(size: Point, dir: (Int, Int), force: Boolean = false): Option[Block] =
      dir match
        case (-1, 0) if force || 1 < min.row =>
          Some {
            Block(delta + dir,
                  min + dir, max + dir,
                  up.map(_-1), down.map(_-1),
                  left, right,
                  color,
                  block.map(_.adj2(dir))*)
          }
        case (1, 0) if force || max.row < size.row =>
          Some {
            Block(delta + dir,
                  min + dir, max + dir,
                  up.map(_+1), down.map(_+1),
                  left, right,
                  color,
                  block.map(_.adj2(dir))*)
          }
        case (0, -1) if force || 1 < min.col =>
          Some {
            Block(delta + dir,
                  min + dir, max + dir,
                  up, down,
                  left.map(_-1), right.map(_-1),
                  color,
                  block.map(_.adj2(dir))*)
          }
        case (0, 1) if force || max.col < size.col =>
          Some {
            Block(delta + dir,
                  min + dir, max + dir,
                  up, down,
                  left.map(_+1), right.map(_+1),
                  color,
                  block.map(_.adj2(dir))*)
          }
        case _ =>
          None

    def apply(delta: Point): Block =
      var block = this
      var n = delta.row
      while n > 0
      do
        n -= 1
        block = block((0, 0), (1, 0), true).get
      n = delta.col
      while n > 0
      do
        n -= 1
        block = block((0, 0), (0, 1), true).get
      block

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
