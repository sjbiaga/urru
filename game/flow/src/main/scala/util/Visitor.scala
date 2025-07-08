package urru
package game
package flow
package util

import scala.math.abs

import cats.{ Eval, Id }
import cats.data.State
import cats.syntax.flatMap.*

import base.Visitor.{ Visited => _, * }
import common.Tree

import UndoRedo.*

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

      new base.Visitor[Game, Path, Undo, Redo, Id, Tree[Int | Null]] {

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
                          depth: Int, detail: Option[Any]): Tree[Int | Null] =
          detail match
            case Some(_) =>
              GAME -> transition -> 0
            case _ =>
              GAME -> transition

        override def path(self: Path,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Int | Null] =
          detail match
            case Some(_) =>
              PATH -> transition -> 0
            case _ =>
              PATH -> transition

        override def undo(self: Undo,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Int | Null] =
          transition match
            case (_, LOOP) =>
              UNDO -> transition -> 1
            case _ =>
              UNDO -> transition

        override def redo(self: Redo,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Int | Null] =
          transition match
            case (_, LOOP) =>
              REDO -> transition -> 1
            case _ =>
              REDO -> transition

      }
        .apply(game, State.pure(Eval.always(Empty)), strategies, 1).runA(OPEN)
        .flatten
        .map { it =>
          def counter(tree: Tree[Int | Null], count: Int = 0): Int =
            tree match
              case Empty => count
              case Leaf(i: Int) => count + i
              case Node(i: Int, children*) => children.foldRight(count + i)(counter)
          counter(it)
        }
        .value


  object `Loser Versus Player`:

    import Versus.{ Data, Parameter, UR, Key }

    private val strategies = Map(
      GAME -> ASCENDING,
      PATH -> (ASCENDING | UPSIDEDOWN),
      UNDO -> 0,
      REDO -> 0
    )

    def apply(game: Game, i: Int): Tree[Data] =

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
        .value


  object `Identifier Collector`:

    private val strategies = Map(
      GAME -> ASCENDING,
      PATH -> (ASCENDING | UPSIDEDOWN),
      UNDO -> 0,
      REDO -> 0
    )

    case class Data(depth: Int, identifier: Option[Long])
        extends common.Tree.Validate.HasDepth

    def apply(game: Game, i: Int, d: Int = 1): Set[Long] =

      new base.Visitor[Game, Path, Undo, Redo, Id, Tree[Data]] {

        private var ur: List[Unit] = Nil

        override def apply(self: Visited,
                           depth: Int, phase: Phase, strategy: Strategy, detail: Option[Any]): Boolean =
          (self, phase, detail) match
            case (_: Game, LOOP, Some((`i`, 0))) => true
            case (_: Game, LOOP, _) => false
            case (selfʹ: Path, NEXT, _) => ur.nonEmpty || selfʹ.depth > d
            case (_, NEXT, _) => false
            case (_: Undo | _: Redo, OPEN, _) =>
              ur ::= ()
              true
            case (_: Undo | _: Redo, CLOSE, _) =>
              ur = ur.tail
              true
            case _ => true

        override def game(self: Game,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Data] =
          detail match
            case Some((item: Int, _)) =>
              GAME -> transition -> Data(depth, None)
            case _ =>
              GAME -> transition

        override def path(self: Path,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Data] =
          detail match
            case Some(_) =>
              PATH -> transition -> Data(depth, None)
            case _ =>
              PATH -> transition

        override def undo(self: Undo,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Data] =
          transition match
            case (_, LOOP) =>
              UNDO -> transition -> Data(depth, Some(self.identifier))
            case _ =>
              UNDO -> transition

        override def redo(self: Redo,
                          transition: (Phase, Phase), strategies: Map[Entity, Strategy],
                          depth: Int, detail: Option[Any]): Tree[Data] =
          transition match
            case (_, LOOP) =>
              REDO -> transition -> Data(depth, Some(self.identifier))
            case _ =>
              REDO -> transition

      }
        .apply(game, State.pure(Eval.always(Empty)), strategies, 1).runA(OPEN)
        .flatten
        .map { it =>
          def collector(tree: Tree[Data], result: Set[Long] = Set.empty): Set[Long] =
            tree match
              case Empty => result
              case Leaf(Data(_, id)) =>
                id.map(result + _).getOrElse(result)
              case Node(Data(_, id), children*) =>
                val resultʹ = id.map(result + _).getOrElse(result)
                children.foldRight(resultʹ)(collector)
          collector(it)
        }
        .value


  object `Loser Versus Player: With Identifier Filter`:

    import Versus.{ Data, Parameter, UR, Key }

    private val strategies = Map(
      GAME -> ASCENDING,
      PATH -> (ASCENDING | UPSIDEDOWN),
      UNDO -> 0,
      REDO -> 0
    )

    def apply(game: Game, i: Int, ids: Set[Long]): Tree[Data] =

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
        .value

