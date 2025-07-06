package urru
package game
package flow
package util

import scala.math.abs

import cats.Eval
import cats.data.State
import cats.syntax.flatMap.*

import base.Visitor.{ Visited => _, * }
import common.Tree

import UndoRedo.*

import Tree.*
import Tree.given


object Visitor:

  object `Just Counter`:

    private val strategies = Map(
      GAME -> ASCENDING,
      PATH -> (ASCENDING | INSIDEOUT),
      UNDO -> 0,
      REDO -> 0
    )

    def apply(game: Game, i: Int): Option[Int] =

      new Visitor[Option, Int | Null] {

        override def apply(self: Visited, depth: Int, phase: Phase, strategy: Strategy, detail: Option[Any]): Boolean =
          (self, phase, detail) match
             case (_: Game, LOOP, Some((`i`, 0))) => true
             case (_: Game, LOOP, _) => false
             case (_: Path, NEXT, _) => true
             case (_, NEXT, _) => false
             case _ => true

        override def game(self: Game, transition: (Phase, Phase), strategies: Map[Entity, Strategy], depth: Int, detail: Option[Any]): Option[Tree[Int | Null]] =
          detail match
            case Some(_) =>
              Some(GAME -> transition -> 0)
            case _ =>
              Some(GAME -> transition)

        override def path(self: Path, transition: (Phase, Phase), strategies: Map[Entity, Strategy], depth: Int, detail: Option[Any]): Option[Tree[Int | Null]] =
          detail match
            case Some(_) =>
              Some(PATH -> transition -> 0)
            case _ =>
              Some(PATH -> transition)

        override def undo(self: Undo, transition: (Phase, Phase), strategies: Map[Entity, Strategy], depth: Int, detail: Option[Any]): Option[Tree[Int | Null]] =
          transition match
            case (_, LOOP) =>
              Some(UNDO -> transition -> 1)
            case _ =>
              Some(UNDO -> transition)

        override def redo(self: Redo, transition: (Phase, Phase), strategies: Map[Entity, Strategy], depth: Int, detail: Option[Any]): Option[Tree[Int | Null]] =
          transition match
            case (_, LOOP) =>
              Some(REDO -> transition -> 1)
            case _ =>
              Some(REDO -> transition)

      }
        .apply(game, State.pure(Eval.always(Some(Empty))), strategies, 1).runA(OPEN)
        .flatten
        .value
        .map { it =>
          def counter(tree: Tree[Int | Null], count: Int = 0): Int =
            tree match
              case Empty => count
              case Leaf(i: Int) => count + i
              case Node(i: Int, children*) => children.foldRight(count + i)(counter)
          counter(it)
        }


  object `Loser Versus Player`:

    import Versus.{ Data, Parameter, UR, Key }

    private val strategies = Map(
      GAME -> ASCENDING,
      PATH -> (ASCENDING | UPSIDEDOWN),
      UNDO -> 0,
      REDO -> 0
    )

    def apply(game: Game, i: Int): Option[Tree[Data]] =

      new Visitor[Option, Data] {

        override def apply(self: Visited, depth: Int, phase: Phase, strategy: Strategy, detail: Option[Any]): Boolean =
          (self, phase, detail) match
             case (_: Game, LOOP, Some((`i`, 0))) => true
             case (_: Game, LOOP, _) => false
             case (_: Path, NEXT, _) => true
             case (_, NEXT, _) => false
             case _ => true

        override def game(self: Game, transition: (Phase, Phase), strategies: Map[Entity, Strategy], depth: Int, detail: Option[Any]): Option[Tree[Data]] =
          detail match
            case Some((item: Int, _)) =>
              Some(GAME -> transition -> Data(depth, Parameter(Some(item), None)))
            case _ =>
              Some(GAME -> transition)

        override def path(self: Path, transition: (Phase, Phase), strategies: Map[Entity, Strategy], depth: Int, detail: Option[Any]): Option[Tree[Data]] =
          detail match
            case Some(_) =>
              Some(PATH -> transition -> Data(depth, Parameter(None, None)))
            case _ =>
              Some(PATH -> transition)

        override def undo(self: Undo, transition: (Phase, Phase), strategies: Map[Entity, Strategy], depth: Int, detail: Option[Any]): Option[Tree[Data]] =
          transition match
            case (_, LOOP) =>
              Some(UNDO -> transition -> Data(depth, Parameter(None, Some(UR(self)))))
            case _ =>
              Some(UNDO -> transition)

        override def redo(self: Redo, transition: (Phase, Phase), strategies: Map[Entity, Strategy], depth: Int, detail: Option[Any]): Option[Tree[Data]] =
          transition match
            case (_, LOOP) =>
              Some(REDO -> transition -> Data(depth, Parameter(None, Some(UR(self)))))
            case _ =>
              Some(REDO -> transition)

      }.apply(game, State.pure(Eval.always(Some(Empty))), strategies, 1).runA(OPEN).flatten.value
