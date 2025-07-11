
package urru
package game
package flow
package util

import scala.math.abs

import cats.{ Eval, Id }
import cats.data.State
import cats.instances.int.*
import cats.instances.set.*
import cats.syntax.flatMap.*

import base.Visitor.{ Visited => _, * }
import common.Tree
import common.grid.{ row, col }

import UndoRedo.*

import sΠ.Gather

import Tree.*
import Tree.Implicits.*
import Tree.given


object Visitor:

  object `Just Counter`:

    private val strategies = Map(
      GAME -> ASCENDING,
      PATH -> (ASCENDING | INSIDEOUT),
      UNDO -> 0,
      REDO -> 0
    )

    def apply(game: Game, i: Int): Int =

      new base.Visitor[Game, Path, Undo, Redo, Id, Int] {

        override def apply(self: Visited,
                           depth: Int, phase: Phase, strategy: Strategy, detail: Option[Any]): Boolean =
          (self, phase, detail) match
            case (_: Game, LOOP, Some((`i`, 0))) => true
            case (_: Game, LOOP, _) => false
            case (_: Path, NEXT, _) => true
            case (_, NEXT, _) => false
            case _ => true

        override def undo(self: Undo,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Int =
          transition match
            case (_, LOOP) => 1
            case _ => 0

        override def redo(self: Redo,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Int =
          transition match
            case (_, LOOP) => 1
            case _ => 0

      }
        .apply(game, State.pure(Eval.always(0)), strategies, 1).runA(OPEN)
        .flatten
        .value


  object `Loser Versus Player`:

    import Versus.{ apply, Data, Parameter, UR, Key }

    private val strategies = Map(
      GAME -> ASCENDING,
      PATH -> (ASCENDING | UPSIDEDOWN),
      UNDO -> 0,
      REDO -> 0
    )

    def apply(game: Game, i: Int): Tree[Parameter] =

      new base.Visitor[Game, Path, Undo, Redo, Id, Tree[Data]] {

        override def apply(self: Visited,
                           depth: Int, phase: Phase, strategy: Strategy, detail: Option[Any]): Boolean =
          (self, phase, detail) match
            case (_: Game, LOOP, Some((`i`, 0))) => true
            case (_: Game, LOOP, _) => false
            case (_: Path, NEXT, _) => true
            case (_, NEXT, _) => false
            case _ => true

        override def game(self: Game,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Data] =
          detail match
            case Some((item: Int, _)) =>
              GAME -> transition -> Data(depth, Parameter(Some(item), None))
            case _ =>
              GAME -> transition

        override def path(self: Path,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Data] =
          detail match
            case Some(_) =>
              PATH -> transition -> Data(depth, Parameter(None, None))
            case _ =>
              PATH -> transition

        override def undo(self: Undo,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Data] =
          transition match
            case (_, LOOP) =>
              UNDO -> transition -> Data(depth, Parameter(None, Some(UR(self))))
            case _ =>
              UNDO -> transition

        override def redo(self: Redo,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Data] =
          transition match
            case (_, LOOP) =>
              REDO -> transition -> Data(depth, Parameter(None, Some(UR(self))))
            case _ =>
              REDO -> transition

      }
        .apply(game, State.pure(Eval.always(Empty)), strategies, 1).runA(OPEN)
        .flatten
        .map { it =>
          assert(Validate(it)())
          Gather(it)()
        }
        .value


  object `Identifier Collector`:

    private val strategies = Map(
      GAME -> ASCENDING,
      PATH -> (ASCENDING | UPSIDEDOWN),
      UNDO -> 0,
      REDO -> 0
    )

    def apply(game: Game, i: Int)(max: Int = game.size.row * game.size.col, min: Int = 1): Set[Long] =

      new base.Visitor[Game, Path, Undo, Redo, Id, Set[Long]] {

        override def apply(self: Visited,
                           depth: Int, phase: Phase, strategy: Strategy, detail: Option[Any]): Boolean =
          (self, phase, detail) match
            case (_: Game, LOOP, Some((`i`, 0))) => true
            case (_: Game, LOOP, _) => false
            case (selfʹ: Path, LOOP, _) => selfʹ.nesting > 0 || selfʹ.depth < max
            case (selfʹ: Path, NEXT, _) => selfʹ.nesting > 0 || selfʹ.depth > min
            case (_, NEXT, _) => false
            case _ => true

        override def undo(self: Undo,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Set[Long] =
          transition match
            case (_, LOOP) => Set(self.identifier)
            case _ => Set.empty

        override def redo(self: Redo,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Set[Long] =
          transition match
            case (_, LOOP) => Set(self.identifier)
            case _ => Set.empty

      }
        .apply(game, State.pure(Eval.always(Set.empty)), strategies, 1).runA(OPEN)
        .flatten
        .value


  object `Loser Versus Player: With Identifier Filter`:

    import Versus.{ apply, Data, Parameter, UR, Key }

    private val strategies = Map(
      GAME -> ASCENDING,
      PATH -> (ASCENDING | UPSIDEDOWN),
      UNDO -> 0,
      REDO -> 0
    )

    def apply(game: Game, i: Int, ids: Set[Long]): Tree[Parameter] =

      new base.Visitor[Game, Path, Undo, Redo, Id, Tree[Data]] {

        override def apply(self: Visited,
                           depth: Int, phase: Phase, strategy: Strategy, detail: Option[Any]): Boolean =
          (self, phase, detail) match
            case (_: Game, LOOP, Some((`i`, 0))) => true
            case (_: Game, LOOP, _) => false
            case (self: Undo, LOOP, _) => ids.contains(self.identifier)
            case (self: Redo, LOOP, _) => ids.contains(self.identifier)
            case (_: Path, NEXT, _) => true
            case (_, NEXT, _) => false
            case _ => true

        override def game(self: Game,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Data] =
          detail match
            case Some((item: Int, _)) =>
              GAME -> transition -> Data(depth, Parameter(Some(item), None))
            case _ =>
              GAME -> transition

        override def path(self: Path,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Data] =
          detail match
            case Some(_) =>
              PATH -> transition -> Data(depth, Parameter(None, None))
            case _ =>
              PATH -> transition

        override def undo(self: Undo,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Data] =
          transition match
            case (_, LOOP) =>
              UNDO -> transition -> Data(depth, Parameter(None, Some(UR(self))))
            case _ =>
              UNDO -> transition

        override def redo(self: Redo,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Data] =
          transition match
            case (_, LOOP) =>
              REDO -> transition -> Data(depth, Parameter(None, Some(UR(self))))
            case _ =>
              REDO -> transition

      }
        .apply(game, State.pure(Eval.always(Empty)), strategies, 1).runA(OPEN)
        .flatten
        .map { it =>
          assert(Validate(it)())
          Gather(it)()
        }
        .value

