package urru
package game
package fold

import common.Mutable

import urru.grid.Grid.Id
import tense.intensional.Data.Doubt


sealed trait UndoRedo[UR <: UndoRedo[UR]]
    extends urru.UndoRedo[Path, Cell, Doubt, Clue, Move, UR]


object UndoRedo:

  case class Undo(
    override val id: Id,
    override val move: Move,
    override val intensity: Doubt,
    override val next: Option[Undo],
    override val path: Mutable[Path],
    override val number: Long,
    override val identifier: Mutable[Long] = Mutable(-1L),
    var elapsed: Long = -1
  ) extends UndoRedo[Undo]
      with urru.UndoRedo.Undo[Path, Cell, Doubt, Clue, Move, Undo]:

    override def apply(real: Path, id: Long) =
      copy(path = Mutable(real), identifier = Mutable(id))

////////////////////////////////////////////////////////////////////////////////

  case class Redo(
    override val id: Id,
    override val undo: Undo,
    override val next: Option[Redo],
    override val path: Mutable[Path],
    override val identifier: Mutable[Long] = Mutable(-1L)
  ) extends UndoRedo[Redo]
      with urru.UndoRedo.Redo[Path, Cell, Doubt, Clue, Move, Redo, Undo]:

    override val move: Move = undo.move

    override def apply(real: Path, id: Long) =
      copy(path = Mutable(real), identifier = Mutable(id))
