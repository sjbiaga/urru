package urru
package game
package fill

import scala.collection.mutable.{ HashMap, HashSet }


object Have:

  import scala.Function.const

  import urru.grid.Grid.Id
  import grid.Tense.Have

  case class Board(override val id: Id,
                   override val size: Point,
                   override val grid: HashMap[Point, HashSet[Cell]],
                   override val clues: Set[Clue])
    extends Have[Clue, Cell, Move]:

    def this(id: Id, size: Point)(grid: Map[Point, Cell], clues: Set[Clue]) =
      this(id, size, HashMap.from(grid.map(_ -> HashSet(_))), clues)

    override def move(it: Move) =
      it(clues)(const(true)).foreach { (pt, cell) =>
        if !grid.contains(pt)
        then
          grid(pt) = HashSet()
        grid(pt) += cell
      }
