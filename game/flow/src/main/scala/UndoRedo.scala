package urru
package game
package flow

import common.Mutable

import urru.grid.Grid.Id
import tense.intensional.Data.Doubt

import UndoRedo.*


sealed trait UndoRedo[UR <: UndoRedo[UR]]
    extends urru.UndoRedo[Path, Cell, Doubt, Clue, Move, Undo, Redo, UR]


object UndoRedo:

  case class Undo(
    override val id: Id,
    override val move: Move,
    override val intensity: Doubt,
    override val next: Option[Undo],
    override val path: Mutable[Path],
    override val number: Long,
    override val identifier: Long = -1L,
    override val elapsed: Mutable[Long] = Mutable(-1L)
  ) extends UndoRedo[Undo]
      with urru.UndoRedo.Undo[Path, Cell, Doubt, Clue, Move, Undo, Redo]
      with util.Visited.Undo:

    override def apply(real: Path, id: Long) =
      copy(path = Mutable(real), identifier = id, elapsed = Mutable(elapsed))

////////////////////////////////////////////////////////////////////////////////

  case class Redo(
    override val id: Id,
    override val undo: Undo,
    override val next: Option[Redo],
    override val path: Mutable[Path],
    override val identifier: Long = -1L,
    override val elapsed: Mutable[Long] = Mutable(-1L)
  ) extends UndoRedo[Redo]
      with urru.UndoRedo.Redo[Path, Cell, Doubt, Clue, Move, Undo, Redo]
      with util.Visited.Redo:

    override val move: Move = undo.move

    override def apply(real: Path, id: Long) =
      copy(path = Mutable(real), identifier = id, elapsed = Mutable(elapsed))

////////////////////////////////////////////////////////////////////////////////


  object http4s:

    import cats.effect.IO

    import io.circe.{ Decoder, Encoder }
    import io.circe.generic.auto.*
    import io.circe.generic.{ semiauto => sa }

    import org.http4s.circe.{ jsonEncoderOf, jsonOf }
    import org.http4s.{ EntityDecoder, EntityEncoder }

    import common.Mutable.http4s.given
    import grid.Grid.http4s.given
    import Clue.http4s.given
    import Data.http4s.given
    import Cell.http4s.given
    import Move.http4s.given
    import Path.http4s.given

    given Decoder[Undo] = sa.deriveDecoder
    given Encoder[Undo] = sa.deriveEncoder

    given EntityDecoder[IO, Undo] = jsonOf
    given EntityEncoder[IO, Undo] = jsonEncoderOf

    given Decoder[Redo] = sa.deriveDecoder
    given Encoder[Redo] = sa.deriveEncoder

    given EntityDecoder[IO, Redo] = jsonOf
    given EntityEncoder[IO, Redo] = jsonEncoderOf
