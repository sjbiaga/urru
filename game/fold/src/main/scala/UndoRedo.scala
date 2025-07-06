package urru
package game
package fold

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
      with urru.UndoRedo.Undo[Path, Cell, Doubt, Clue, Move, Undo, Redo]:

    override def apply(real: Path, id: Long) =
      copy(path = Mutable(real), identifier = Mutable(id), elapsed = Mutable(elapsed))

////////////////////////////////////////////////////////////////////////////////

  case class Redo(
    override val id: Id,
    override val undo: Undo,
    override val next: Option[Redo],
    override val path: Mutable[Path],
    override val identifier: Long = -1L,
    override val elapsed: Mutable[Long] = Mutable(-1L)
  ) extends UndoRedo[Redo]
      with urru.UndoRedo.Redo[Path, Cell, Doubt, Clue, Move, Undo, Redo]:

    override val move: Move = undo.move

    override def apply(real: Path, id: Long) =
      copy(path = Mutable(real), identifier = Mutable(id), elapsed = Mutable(elapsed))
