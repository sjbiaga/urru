package urru
package game
package flow

import scala.collection.{ Map => AnyMap }
import scala.collection.mutable.{ ListBuffer => MutableList }

import common.grid.{ adj, row, col }

import urru.grid.shape

import Clue.{ Cross, Start, Strip }


case class Move(
  odd: Int,
  override val at: Point,
  override val by: Point,
  override val color: Int,
  override val elapsed: Long
) extends urru.Move[Cell, Clue], shape.By:
  require(at.adj(by))

  override val dir: (Int, Int) = (by.row - at.row, by.col - at.col)

  // move / redo
  override def apply(clues: Set[Clue])(filter: Point => Boolean) =
    if filter(by)
    then
      Map {
        by -> Cell(MutableList(color),
                   clues.find {
                     case Start(`by`, _,  _) | Cross(`by`)
                        | Strip(`at`, `by`) | Strip(`by`, `at`) => true
                     case _ => false
                   })
      }
    else
      Map.empty

  // undo
  override def apply(grid: AnyMap[Point, Cell])
                    (remove: Point => AnyMap[Point, Cell]): AnyMap[Point, Cell] =
    this(by, grid)(remove)


object Move:

  def apply(size: Point, at: Point, dir: (Int, Int)): Option[Point] =
    dir match
      case (-1, 0) if 1 < at.row => Some(at.adj._1)
      case (1, 0) if at.row < size.row => Some(at.adj._2)
      case (0, -1) if 1 < at.col => Some(at.adj._3)
      case (0, 1) if at.col < size.col => Some(at.adj._4)
      case _ => None

  object http4s:

    import cats.effect.IO

    import io.circe.generic.auto.*

    import org.http4s.circe.{ jsonEncoderOf, jsonOf }
    import org.http4s.{ EntityDecoder, EntityEncoder }

    import common.grid.http4s.given
    import Clue.http4s.given

    given EntityDecoder[IO, Move] = jsonOf
    given EntityEncoder[IO, Move] = jsonEncoderOf
