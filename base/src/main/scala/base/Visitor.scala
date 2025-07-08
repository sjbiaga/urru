package urru
package base

import cats.{ Apply, Eval, Monoid }
import cats.data.State

import common.Tree

import Visitor.*


abstract trait Visitor[
  G <: Visited[G, B, U, R],
  B <: Visited[G, B, U, R],
  U <: Visited[G, B, U, R],
  R <: Visited[G, B, U, R],
  F[_]: Apply,
  A: Monoid
]:

  final def apply(self: Visited[G, B, U, R],
                  phases: State[Phase, Eval[F[A]]],
                  strategies: Map[Entity, Strategy],
                  depth: Int): State[Phase, Eval[F[A]]] =
    self(this, phases, strategies, depth + 1)

  def apply(self: Visited[G, B, U, R],
            depth: Int,
            phase: Phase,
            strategy: Strategy,
            detail: Option[Any] = None): Boolean = true

  def game(self: G,
           transition: (Phase, Phase),
           strategies: Map[Entity, Strategy],
           depth: Int,
           detail: Option[Any] = None): F[A]

  def path(self: B,
           transition: (Phase, Phase),
           strategies: Map[Entity, Strategy],
           depth: Int,
           detail: Option[Any] = None): F[A]

  def undo(self: U,
           transition: (Phase, Phase),
           strategies: Map[Entity, Strategy],
           depth: Int,
           detail: Option[Any] = None): F[A]

  def redo(self: R,
           transition: (Phase, Phase),
           strategies: Map[Entity, Strategy],
           depth: Int,
           detail: Option[Any] = None): F[A]


object Visitor:

  export Entity.*, Phase.*, Strategy.*

  enum Entity:
    case GAME, PATH, UNDO, REDO

  enum Phase:
    case OPEN, NEXT, LOOP, CLOSE

  type Strategy = Int

  object Strategy:
    val INSIDEOUT = 1 << 1
    val UPSIDEDOWN = 1 << 2
    val ASCENDING = 1 << 3
    val DESCENDING = 1 << 4

  abstract trait Visited[
    G <: Visited[G, B, U, R],
    B <: Visited[G, B, U, R],
    U <: Visited[G, B, U, R],
    R <: Visited[G, B, U, R],
  ]:

    def apply[
      F[_]: Apply,
      A: Monoid
    ](visitor: Visitor[G, B, U, R, F, A],
      phases: State[Phase, Eval[F[A]]],
      strategies: Map[Entity, Strategy],
      depth: Int
    ): State[Phase, Eval[F[A]]]
