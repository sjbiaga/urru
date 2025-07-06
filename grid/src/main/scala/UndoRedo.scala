package urru

import scala.collection.mutable.{ ListBuffer => MutableList }

import common.Mutable
import Mutable.given

import grid.Game
import Game.*

import grid.Grid.*

import grid.Tense.Just
import UndoRedo.*


abstract trait UndoRedo[
  B <: Path[B, C, D, K, M, U, R],
  C <: Cell,
  D,
  K <: Clue,
  M <: Move[C, K],
  U <: Undo[B, C, D, K, M, U, R] & UndoRedo[B, C, D, K, M, U, R, U],
  R <: Redo[B, C, D, K, M, U, R] & UndoRedo[B, C, D, K, M, U, R, R],
  UR <: UndoRedo[B, C, D, K, M, U, R, UR]
] extends Just[D, K, C, M]:

  val next: Option[UR]
  val path: Mutable[B]
  val identifier: Long
  val elapsed: Mutable[Long]

  final override def grid: Map[Point, C] =
    duals.get(path.dual).asInstanceOf[Game[?, ?, C, D, K, M, ?, ?, ?]].grid.toMap
  final override def clues: Set[K] =
    duals.get(path.dual).asInstanceOf[Game[?, ?, C, D, K, M, ?, ?, ?]].clues

  def apply(real: B, id: Long): UR // clone

  override def move(it: M) = { in =>
    path ::= path(it)(in)
  }

  override def undo(p: (Long, Long)) =
    path ::= path.ur.Undo(Some(p))

  override def redo(p: (Long, Long)) =
    path ::= path.ur.Redo(Some(p))

object UndoRedo:

  abstract trait Undo[
    B <: Path[B, C, D, K, M, U, R],
    C <: Cell,
    D,
    K <: Clue,
    M <: Move[C, K],
    U <: Undo[B, C, D, K, M, U, R],
    R <: Redo[B, C, D, K, M, U, R]
  ] extends UndoRedo[B, C, D, K, M, U, R, U]:

    val intensity: D
    val number: Long


  abstract trait Redo[
    B <: Path[B, C, D, K, M, U, R],
    C <: Cell,
    D,
    K <: Clue,
    M <: Move[C, K],
    U <: Undo[B, C, D, K, M, U, R],
    R <: Redo[B, C, D, K, M, U, R]
  ] extends UndoRedo[B, C, D, K, M, U, R, R]:

    val undo: U
