package urru
package game
package flow

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
      val Move(_, _, by, _, _) = it
      if !grid.contains(by)
      then
        grid(by) = HashSet()
      grid(by) += it(clues)(const(true))(by)


  object http4s:

    import cats.effect.IO

    import io.circe.generic.auto.*

    import org.http4s.circe.{ jsonEncoder, jsonOf }
    import org.http4s.EntityDecoder

    import common.grid.http4s.given
    import Clue.http4s.given
    import Cell.http4s.given

    given EntityDecoder[IO, Board] = jsonOf
