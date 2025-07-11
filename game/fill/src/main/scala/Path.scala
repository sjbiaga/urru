package urru
package game
package fill

import scala.collection.mutable.{ HashMap, HashSet }
import scala.collection.mutable.{ ListBuffer => MutableList }

import common.+++
import common.Mutable

import UndoRedo.*
import grid.Tense.*

import urru.grid.Grid.Id
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
) extends urru.Path[Path, Cell, Doubt, Clue, Move, Undo, Redo]:

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
                                      u: Option[Undo],
                                      r: Option[Redo]): Path =
    self.copy(undo = u,
              redo = r,
              have = self.have +++ have)

  inline override protected def apply(grid: Map[Point, Cell], clues: Set[Clue]) =
    new fill.Have.Board(Id(game.counters.have_++), game.size)(grid, clues)
