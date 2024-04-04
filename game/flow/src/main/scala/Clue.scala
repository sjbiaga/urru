package urru
package game
package flow


sealed abstract trait Clue
    extends urru.grid.Game.Clue


object Clue:

  import common.grid.adj
  import common.pairwise

  import urru.grid.shape

  /**
    * Lines' starts and ends.
    */
  case class Start(override val at: Point,
                   override val by: Point,
                   override val color: Int)
      extends Clue, shape.By, shape.Color

////////////////////////////////////////////////////////////////////////////////

  /**
    * Board's empty cells.
    */
  case class Empty(override val at: Point)
      extends Clue, shape.At

////////////////////////////////////////////////////////////////////////////////

  /**
    * Two lines' crossing.
    */
  case class Cross(override val at: Point)
      extends Clue, shape.At

////////////////////////////////////////////////////////////////////////////////

  /**
    * Lines' blocking.
    */
  case class Strip(override val at: Point,
                   override val by: Point)
      extends Clue, shape.By:
    require(at.adj(by))

    override def hashCode(): Int = (at, by).##
    override def equals(any: Any): Boolean = any match
      case Strip(`at`, `by`) | Strip(`by`, `at`) => true
      case _ => false

////////////////////////////////////////////////////////////////////////////////

  /**
    * Lines surrounding.
    */
  case class Frame(border: Strip*)
      extends Clue

////////////////////////////////////////////////////////////////////////////////

  /**
    * Lines following unique path.
    */
  case class Track(sides: Map[Point, Option[(Strip, Strip)]],
                   path: Point*)
      extends Clue:
    require(path.toSet.size == path.size)
    require(path.pairwise.init.forall(_.adj(_)))
