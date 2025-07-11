package urru
package game
package flow

import scala.collection.mutable.{ HashMap, HashSet }
import scala.collection.mutable.{ ListBuffer => MutableList }

import common.+++
import common.Mutable

import UndoRedo.*
import grid.Tense.*
import Clue.Start
import flow.Have.Board

import grid.Grid.Id
import tense.intensional.Data.Doubt


case class Path(
  override val dual: Id,
  override val number: Long,
  override val depth: Int,
  override val nesting: Int = 0,
  override val parent: Option[Path] = None,
  override val undo: Option[Undo] = None,
  override val redo: Option[Redo] = None,
  override val just: MutableList[Just[Doubt, Clue, Cell, Move]] = MutableList(),
  override val have: HashMap[Int, HashSet[Have[Clue, Cell, Move]]] = HashMap()
) extends urru.Path[Path, Cell, Doubt, Clue, Move, Undo, Redo]
    with util.Visited.Path:

  private lazy val game: Game = urru.grid.Game.duals.get(dual).asInstanceOf[Game]

////////////////////////////////////////////////////////////////////////////////

  override protected def fresh: Path =
    Path(dual,
         game.counters.path_++,
         1,
         nesting + 1)

////////////////////////////////////////////////////////////////////////////////

  override def apply(it: Move) = { in =>
    super.apply(it)(in)

    val id = Id(game.counters.just_++)

    Path(dual,
         game.counters.path_++,
         depth + 1,
         nesting,
         Some(this),
         Some {
           Undo(id, it, in,
                undo,
                Mutable(fresh),
                id.number)
         }
    )
  }

////////////////////////////////////////////////////////////////////////////////

  inline override protected def apply(it: Undo, next: Option[Redo]) =
    val id = Id(game.counters.just_++)

    Redo(id, it, next, Mutable(fresh))

  inline override protected def apply() =
    Path(dual,
         game.counters.path_++,
         depth + 1,
         nesting,
         Some(this))

////////////////////////////////////////////////////////////////////////////////

  inline override protected def apply(self: Path,
                                      undo: Option[Undo],
                                      redo: Option[Redo]): Path =
    self.copy(undo = undo,
              redo = redo,
              have = self.have +++ have)

  inline override protected def apply(grid: Map[Point, Cell], clues: Set[Clue]) =
    new Board(Id(game.counters.have_++), game.size)(grid, clues)

////////////////////////////////////////////////////////////////////////////////


object Path:

  object http4s:

    import scala.util.Try

    import cats.effect.IO

    import io.circe.{ Decoder, Encoder, KeyDecoder, KeyEncoder }
    import io.circe.generic.auto.*
    import io.circe.generic.{ semiauto => sa }

    import org.http4s.circe.{ jsonEncoderOf, jsonOf }
    import org.http4s.{ EntityDecoder, EntityEncoder }

    import common.grid.x

    import common.http4s.given
    import common.grid.http4s.given
    import common.Mutable.http4s.given
    import grid.Grid.http4s.given
    import Clue.http4s.given
    import Data.http4s.given
    import Cell.http4s.given
    import Move.http4s.given
    import UndoRedo.http4s.given

    //https://stackoverflow.com/questions/50457466/circe-decode-to-sealed-trait-extended-by-multiple-case-classes
    given Decoder[Just[Doubt, Clue, Cell, Move]] =
      Decoder[Undo]
        .map[Just[Doubt, Clue, Cell, Move]](identity)
        .or(
          Decoder[Redo]
            .map[Just[Doubt, Clue, Cell, Move]](identity)
        )

    given Encoder[Just[Doubt, Clue, Cell, Move]] = Encoder[String].contramap {
      case it: Undo => summon[Encoder[Undo]](it).toString
      case it: Redo => summon[Encoder[Redo]](it).toString
    }

    //https://stackoverflow.com/questions/50457466/circe-decode-to-sealed-trait-extended-by-multiple-case-classes
    given Decoder[Have[Clue, Cell, Move]] =
      Decoder[Board]
        .map[Have[Clue, Cell, Move]](identity)

    given Encoder[Have[Clue, Cell, Move]] = Encoder[String].contramap {
      case it: Board => summon[Encoder[Board]](it).toString
    }

    given KeyDecoder[(Point, Int)] = KeyDecoder.instance { it =>
      Try {
        val Array(pt, n) = " ".r.split(it)
        val Array(row, col) = "x".r.split(pt)
        (row.toInt -> col.toInt) -> n.toInt
      }.toOption
    }
    given KeyEncoder[(Point, Int)] = KeyEncoder[String].contramap { (pt, n) => s"${pt.x} $n" }

    given Decoder[Path] = sa.deriveDecoder
    given Encoder[Path] = sa.deriveEncoder

    given EntityDecoder[IO, Path] = jsonOf
    given EntityEncoder[IO, Path] = jsonEncoderOf
