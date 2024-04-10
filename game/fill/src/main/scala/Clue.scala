package urru
package game
package fill


sealed abstract trait Clue
    extends urru.grid.Game.Clue


object Clue:

  import common.grid.{ adj2, row, x, col, unary_!, + }

  import urru.grid.shape

  extension(self: Iterable[Seq[Point]])
    private def apply(dir: (Int, Int), fun: Point => Int): Seq[Seq[Int]] =
      self
        .zipWithIndex
        .foldLeft(Seq[Seq[Int]]()) {
          case (r, (it, i)) => r :+
            it.foldLeft(Seq[Int]()) {
              case (r, pt) if it.contains(pt + dir) => r
              case (r, pt) => fun(pt) +: r
            }
        }

  /**
    * Blocks' having (compact) shape.
    */
  case class Block(delta: Point,
                   override val min: (Int, Int),
                   override val max: (Int, Int),
                   override val up: Seq[Seq[Int]],
                   override val down: Seq[Seq[Int]],
                   override val left: Seq[Seq[Int]],
                   override val right: Seq[Seq[Int]],
                   override val color: Int,
                   override val block: Point*)
      extends Clue, shape.Block, shape.Color, shape.Sides:

    private def this(delta: Point, color: Int, block: Point*)
                    (up: Seq[Seq[Int]], down: Seq[Seq[Int]],
                     left: Seq[Seq[Int]], right: Seq[Seq[Int]]) =
      this(delta,
           up.map(_.min).min x left.map(_.min).min,
           down.map(_.max).max x right.map(_.max).max,
           up, down, left, right, color, block*)

    private def this(delta: Point, color: Int, ps: Point*)
                    (up_down: Iterable[Seq[Point]], left_right: Iterable[Seq[Point]]) =
      this(delta, color, ps*)
          (up_down((-1, 0), _.row)
          ,up_down(( 1, 0), _.row)
          ,left_right((0, -1), _.col)
          ,left_right((0,  1), _.col))


    private def this(delta: Point, color: Int, ps: Point*) =
      this(delta, color, ps*)
          (ps.groupBy(_.col).toSeq.sortBy(_._1).collect(_._2)
          ,ps.groupBy(_.row).toSeq.sortBy(_._1).collect(_._2))

    def this(color: Int, ps: Point*) =
      this(0 x 0, color, ps*)

////////////////////////////////////////////////////////////////////////////////

    def apply(size: Point)(dir: (Int, Int)): Option[Set[Point]] =
      if !dir
      then
        Some(block.toSet)
      else dir match
        case (-1, 0) if 1 < min.row => Some {
          up.zipWithIndex.map { (ls, col) => ls.map(_-1 x col+1+delta.col) }.flatten.toSet
        }
        case (1, 0) if max.row < size.row => Some {
          down.zipWithIndex.map { (ls, col) => ls.map(_+1 x col+1+delta.col) }.flatten.toSet
        }
        case (0, -1) if 1 < min.col => Some {
          left.zipWithIndex.map { (ls, row) => ls.map(row+1+delta.row x _-1) }.flatten.toSet
        }
        case (0, 1) if max.col < size.col => Some {
          right.zipWithIndex.map { (ls, row) => ls.map(row+1+delta.row x _+1) }.flatten.toSet
        }
        case (1, 1) if 1 <= min.row && max.row <= size.row && 1 <= min.col && max.col <= size.col => Some(block.toSet)
        case _ => None

    def apply(size: Point, dir: (Int, Int), force: Boolean = false): Option[Block] =
      dir match
        case (-1, 0) if force || 1 < min.row =>
          Some {
            Block(delta + dir,
                  min + dir, max + dir,
                  up.map(_.map(_-1)), down.map(_.map(_-1)),
                  left, right,
                  color,
                  block.map(_.adj2(dir))*)
          }
        case (1, 0) if force || max.row < size.row =>
          Some {
            Block(delta + dir,
                  min + dir, max + dir,
                  up.map(_.map(_+1)), down.map(_.map(_+1)),
                  left, right,
                  color,
                  block.map(_.adj2(dir))*)
          }
        case (0, -1) if force || 1 < min.col =>
          Some {
            Block(delta + dir,
                  min + dir, max + dir,
                  up, down,
                  left.map(_.map(_-1)), right.map(_.map(_-1)),
                  color,
                  block.map(_.adj2(dir))*)
          }
        case (0, 1) if force || max.col < size.col =>
          Some {
            Block(delta + dir,
                  min + dir, max + dir,
                  up, down,
                  left.map(_.map(_+1)), right.map(_.map(_+1)),
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
