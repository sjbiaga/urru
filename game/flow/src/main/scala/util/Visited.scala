package urru
package game
package flow
package util

import cats.{ Apply, Eval, Monoid }

import cats.data.State
import cats.syntax.flatMap.*

import base.Visitor.{ Visited => _, * }

import common.Tree

import Tree.Empty
import Tree.Implicits.*

import UndoRedo.*


type Visited = base.Visitor.Visited[Game, Path, Undo, Redo]


object Visited:

  private val D = cats.data.IndexedStateT.catsDataDeferForIndexedStateT[Eval, Phase, Phase]

  abstract trait Game extends Visited { this: flow.Game =>

    override def apply[
      F[_]: Apply,
      A >: Null
    ](visitor: Visitor[F, A],
      phases: State[Phase, Eval[F[Tree[A]]]],
      strategies: Map[Entity, Strategy],
      depth: Int
    ): State[Phase, Eval[F[Tree[A]]]] =
      val s = strategies(Entity.GAME)

      given Conversion[Strategy, Boolean] = _ & s ne 0

      val F = Apply[F]
      val A = Monoid[Tree[A]]

      var acc = phases

      {
        val accʹ = acc
        acc = D.defer {
          accʹ.transform { (phase, rhs) =>
            val lhs = visitor.game(this, phase -> OPEN, strategies, depth)
            OPEN -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }
      }

      for
        i <- 0 until state.size
        item = state(i)
        j <- if ASCENDING then 0 to item.path.size-1
             else if DESCENDING then item.path.size-1 to 0 by -1
             else 0 until 0
        if visitor(this, depth, LOOP, s & (ASCENDING | DESCENDING), Some(i -> j))
        path = item.path(j)
      do
        val accʹ = acc
        acc = D.defer {
          visitor(path, accʹ, strategies, depth)
        }
        val accʹʹ = acc
        acc = D.defer {
          accʹʹ.transform { (phase, rhs) =>
            val lhs = visitor.game(this, phase -> LOOP, strategies, depth, Some(i -> j))
            LOOP -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }

      {
        val accʹ = acc
        acc = D.defer {
          accʹ.transform { (phase, rhs) =>
            val lhs = visitor.game(this, phase -> CLOSE, strategies, depth)
            CLOSE -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }
      }

      acc
  }

////////////////////////////////////////////////////////////////////////////////

  abstract trait Path extends Visited { this: flow.Path =>

    override def apply[
      F[_]: Apply,
      A >: Null
    ](visitor: Visitor[F, A],
      phases: State[Phase, Eval[F[Tree[A]]]],
      strategies: Map[Entity, Strategy],
      depth: Int
    ): State[Phase, Eval[F[Tree[A]]]] =
      val s = strategies(Entity.PATH)

      given Conversion[Strategy, Boolean] = _ & s ne 0

      val F = Apply[F]
      val A = Monoid[Tree[A]]

      var acc = phases

      {
        val accʹ = acc
        acc = D.defer {
          accʹ.transform { (phase, rhs) =>
            val lhs = visitor.path(this, phase -> OPEN, strategies, depth)
            OPEN -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }
      }

      if INSIDEOUT && this.depth > 1 && visitor(this, depth, NEXT, s & INSIDEOUT)
      then
        val accʹ = acc
        acc = D.defer {
          visitor(parent.get, accʹ, strategies, depth)
        }
        val accʹʹ = acc
        acc = D.defer {
          accʹʹ.transform { (phase, rhs) =>
            val lhs = visitor.path(this, phase -> NEXT, strategies, depth, Some(()))
            NEXT -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }

      for
        i <- if ASCENDING then 0 to just.size-1
             else if DESCENDING then just.size-1 to 0 by -1
             else 0 until 0
        it = just(i)
        ur_idx: (Undo | Redo, Either[Int, Int]) = it match { case undo: Undo => undo -> Left(i)
                                                             case redo: Redo => redo -> Right(i) }
        (ur, _) = ur_idx
        if visitor(this, depth, LOOP, s & (ASCENDING | DESCENDING), Some(ur_idx))
      do
        val accʹ = acc
        acc = D.defer {
          visitor(ur, accʹ, strategies, depth)
        }
        val accʹʹ = acc
        acc = D.defer {
          accʹʹ.transform { (phase, rhs) =>
            val lhs = visitor.path(this, phase -> LOOP, strategies, depth, Some(ur_idx))
            LOOP -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }

      if UPSIDEDOWN && this.depth > 1 && visitor(this, depth, NEXT, s & UPSIDEDOWN, Some(()))
      then
        val accʹ = acc
        acc = D.defer {
          visitor(parent.get, accʹ, strategies, depth)
        }
        val accʹʹ = acc
        acc = D.defer {
          accʹʹ.transform { (phase, rhs) =>
            val lhs = visitor.path(this, phase -> NEXT, strategies, depth, Some(()))
            NEXT -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }

      {
        val accʹ = acc
        acc = D.defer {
          accʹ.transform { (phase, rhs) =>
            val lhs = visitor.path(this, phase -> CLOSE, strategies, depth)
            CLOSE -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }
      }

      acc
  }

////////////////////////////////////////////////////////////////////////////////

  abstract trait Undo extends Visited { this: flow.UndoRedo.Undo =>

    override def apply[
      F[_]: Apply,
      A >: Null
    ](visitor: Visitor[F, A],
      phases: State[Phase, Eval[F[Tree[A]]]],
      strategies: Map[Entity, Strategy],
      depth: Int
    ): State[Phase, Eval[F[Tree[A]]]] =
      val s = strategies(Entity.PATH)

      given Conversion[Strategy, Boolean] = _ & s ne 0

      val F = Apply[F]
      val A = Monoid[Tree[A]]

      var acc = phases

      {
        val accʹ = acc
        acc = D.defer {
          accʹ.transform { (phase, rhs) =>
            val lhs = visitor.undo(this, phase -> OPEN, strategies, depth)
            OPEN -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }
      }

      if INSIDEOUT && next.isDefined && visitor(this, depth, NEXT, s & INSIDEOUT)
      then
        val accʹ = acc
        acc = D.defer {
          visitor(next.get, accʹ, strategies, depth)
        }
        val accʹʹ = acc
        acc = D.defer {
          accʹʹ.transform { (phase, rhs) =>
            val lhs = visitor.undo(this, phase -> NEXT, strategies, depth)
            NEXT -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }

      if visitor(this, depth, LOOP, s)
      then
      {
        val accʹ = acc
        acc = D.defer {
          visitor(path, accʹ, strategies, depth)
        }
        val accʹʹ = acc
        acc = D.defer {
          accʹʹ.transform { (phase, rhs) =>
            val lhs = visitor.undo(this, phase -> LOOP, strategies, depth)
            LOOP -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }
      }

      if UPSIDEDOWN && next.isDefined && visitor(this, depth, NEXT, s & UPSIDEDOWN)
      then
        val accʹ = acc
        acc = D.defer {
          visitor(next.get, accʹ, strategies, depth)
        }
        val accʹʹ = acc
        acc = D.defer {
          accʹʹ.transform { (phase, rhs) =>
            val lhs = visitor.undo(this, phase -> NEXT, strategies, depth)
            NEXT -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }

      {
        val accʹ = acc
        acc = D.defer {
          accʹ.transform { (phase, rhs) =>
            val lhs = visitor.undo(this, phase -> CLOSE, strategies, depth)
            CLOSE -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }
      }

      acc
  }

////////////////////////////////////////////////////////////////////////////////

  abstract trait Redo extends Visited { this: flow.UndoRedo.Redo =>

    override def apply[
      F[_]: Apply,
      A >: Null
    ](visitor: Visitor[F, A],
      phases: State[Phase, Eval[F[Tree[A]]]],
      strategies: Map[Entity, Strategy],
      depth: Int
    ): State[Phase, Eval[F[Tree[A]]]] =
      val s = strategies(Entity.PATH)

      given Conversion[Strategy, Boolean] = _ & s ne 0

      val F = Apply[F]
      val A = Monoid[Tree[A]]

      var acc = phases

      {
        val accʹ = acc
        acc = D.defer {
          accʹ.transform { (phase, rhs) =>
            val lhs = visitor.redo(this, phase -> OPEN, strategies, depth)
            OPEN -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }
      }

      if INSIDEOUT && next.isDefined && visitor(this, depth, NEXT, s & INSIDEOUT)
      then
        val accʹ = acc
        acc = D.defer {
          visitor(next.get, accʹ, strategies, depth)
        }
        val accʹʹ = acc
        acc = D.defer {
          accʹʹ.transform { (phase, rhs) =>
            val lhs = visitor.redo(this, phase -> NEXT, strategies, depth)
            NEXT -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }

      if visitor(this, depth, LOOP, s)
      then
      {
        val accʹ = acc
        acc = D.defer {
          visitor(path, accʹ, strategies, depth)
        }
        val accʹʹ = acc
        acc = D.defer {
          accʹʹ.transform { (phase, rhs) =>
            val lhs = visitor.redo(this, phase -> LOOP, strategies, depth)
            LOOP -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }
      }

      if UPSIDEDOWN && next.isDefined && visitor(this, depth, NEXT, s & UPSIDEDOWN)
      then
        val accʹ = acc
        acc = D.defer {
          visitor(next.get, accʹ, strategies, depth)
        }
        val accʹʹ = acc
        acc = D.defer {
          accʹʹ.transform { (phase, rhs) =>
            val lhs = visitor.redo(this, phase -> NEXT, strategies, depth)
            NEXT -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }

      {
        val accʹ = acc
        acc = D.defer {
          accʹ.transform { (phase, rhs) =>
            val lhs = visitor.redo(this, phase -> CLOSE, strategies, depth)
            CLOSE -> F.map2Eval(lhs, rhs)(A.combine)
          }
        }
      }

      acc
  }

////////////////////////////////////////////////////////////////////////////////
