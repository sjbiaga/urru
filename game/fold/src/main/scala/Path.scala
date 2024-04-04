package urru
package game
package fold

import scala.collection.mutable.{ HashMap, HashSet }
import scala.collection.mutable.{ ListBuffer => MutableList, StringBuilder }

import common.+++
import common.Mutable

import UndoRedo._
import grid.Tense._

import urru.grid.Grid.Id
import tense.intensional.Data.Doubt

import Path._


case class Path(
  override val dual: Id,
  override val number: Long = 0,
  override val depth: Int = -1,
  override val nesting: Int = -1,
  override val replica: Boolean = false,
  override val parent: Option[Path] = None,
  override val undo: Option[Undo] = None,
  override val redo: Option[Redo] = None,
  override val just: MutableList[Just[Doubt, Clue, Cell, Move]] = MutableList(),
  override val have: HashMap[Int, HashSet[Have[Clue, Cell, Move]]] = HashMap(),
  override protected val pisc: MutableList[StringBuilder] = MutableList(StringBuilder())
) extends urru.Path[Path, Cell, Doubt, Clue, Move, Redo, Undo]:

  private lazy val game: Game = urru.grid.Game.duals.get(dual).asInstanceOf[Game]

////////////////////////////////////////////////////////////////////////////////

  override protected def apply(): Path =
    Path(dual,
         game.counters.path_++,
         0,
         nesting + 1,
         true)

////////////////////////////////////////////////////////////////////////////////

  override def apply(it: Move) = { in =>
    super.apply(it)(in)

    val id = Id(game.counters.just_++)

    Path(dual,
         game.counters.path_++,
         depth + 1,
         nesting,
         replica,
         Some(this),
         Some {
           Undo(id, it, in,
                undo,
                Mutable(Path(dual)),
                Mutable(id.number))
         }
    )
  }

////////////////////////////////////////////////////////////////////////////////

  inline override protected def apply(it: Undo, next: Option[Redo]) =
    val id = Id(game.counters.just_++)

    Redo(id, it, next, Mutable(Path(dual)))

  inline override protected def apply(it: Redo) =
    Path(dual,
         game.counters.path_++,
         depth + 1,
         nesting,
         replica,
         Some(this))

////////////////////////////////////////////////////////////////////////////////

  inline override protected def apply(self: Path,
                                      u: Option[Undo],
                                      r: Option[Redo]): Path =
    self.copy(undo = u,
              redo = r,
              have = self.have +++ have)

  inline override protected def apply(grid: Map[Point, Cell], clues: Set[Clue]) =
    new fold.Have.Board(Id(game.counters.have_++), game.size)(grid, clues)
