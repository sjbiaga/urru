package urru

import scala.collection.mutable.{ ListBuffer => MutableList }

import common.Mutable
import Mutable.given

import grid.Game
import Game._

import grid.Grid._

import grid.Tense.Just
import UndoRedo._


abstract trait UndoRedo[
  B <: Path[B, C, D, K, M, ?, ?],
  C <: Cell,
  D,
  K <: Clue,
  M <: Move[C, K],
  UR <: UndoRedo[B, C, D, K, M, UR]
] extends Just[D, K, C, M]:

  val next: Option[UR]
  val path: Mutable[B]
  val identifier: Mutable[Long]

  final override def grid: Map[Point, C] =
    duals.get(path.dual).asInstanceOf[Game[?, B, C, D, K, M, ?]].grid.toMap
  final override def clues: Set[K] =
    duals.get(path.dual).asInstanceOf[Game[?, B, C, D, K, M, ?]].clues

  def apply(real: B, id: Long): UR // clone

  override def move(it: M) = { in =>
    path ::= path(it)(in)
  }

  override def undo(id: Long) =
    path ::= path.ur.Undo(Some(id))

  override def redo(id: Long) =
    path ::= path.ur.Redo(Some(id))

object UndoRedo:

  abstract trait Undo[
    B <: Path[B, C, D, K, M, ?, ?],
    C <: Cell,
    D,
    K <: Clue,
    M <: Move[C, K],
    U <: Undo[B, C, D, K, M, U]
  ] extends UndoRedo[B, C, D, K, M, U]:

    val intensity: D
    val number: Long


  abstract trait Redo[
    B <: Path[B, C, D, K, M, ?, ?],
    C <: Cell,
    D,
    K <: Clue,
    M <: Move[C, K],
    R <: Redo[B, C, D, K, M, R, U],
    U <: Undo[B, C, D, K, M, U]
  ] extends UndoRedo[B, C, D, K, M, R]:

    val undo: U
