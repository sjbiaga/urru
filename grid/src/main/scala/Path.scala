package urru

import scala.annotation.tailrec
import scala.collection.mutable.{ HashMap, HashSet }
import scala.collection.mutable.{ ListBuffer => MutableList }
import scala.util.control.TailCalls.{ done, tailcall, TailRec }

import cats.effect.IO
import fs2.Stream
import Stream.repeatEval

import common.grid.{ row, x, col, unary_! }
import common.Mutable.given

import grid.shape

import grid.Game.*
import UndoRedo.*

import grid.Grid.Id
import grid.Tense.*
import grid.tense.intensional.Grid


abstract trait Path[
  B <: Path[B, C, D, K, M, U, R],
  C <: Cell,
  D,
  K <: Clue,
  M <: Move[C, K],
  U <: Undo[B, C, D, K, M, U, R],
  R <: Redo[B, C, D, K, M, U, R]
]:

  val dual: Id
  val number: Long

  val depth: Int
  val nesting: Int

  val replica: Boolean

  val parent: Option[B]

  val undo: Option[U]
  val redo: Option[R]

  val just: MutableList[Just[D, K, C, M]]
  val have: HashMap[Int, HashSet[Have[K, C, M]]]

  protected given Conversion[Feature, Boolean] = duals.get(dual).features(_)

  protected def apply(): B // fresh

////////////////////////////////////////////////////////////////////////////////

  object tense:

// Past Perfect ////////////////////////////////////////////////////////////////

    object Have:

      @tailrec
      def apply(it: M): Unit =
        have.foreach(_._2.foreach(_.move(it)))

        if depth > 1
        then
          parent.get.tense.Have(it)

// Past Simple /////////////////////////////////////////////////////////////////

    object Just:

      @tailrec
      def apply(fun: Just[D, K, C, M] => Unit): Unit =
        for
          i <- 0 until just.size
        do
          fun(just(i))

        if depth > 1
        then
          parent.get.tense.Just(fun)

// Past Simple ///////////////////////////////////////////////////////// time //

      def travel(color: Int): Stream[IO, (Int, Seq[(D, U Either R, Int, Int, Int)])] =
        var path: Option[(Int, Seq[(D, U Either R, Int, Int, Int)])] = None

        var call = tailcall { travel(color, { (_, it) => path = Some(color -> it); true }) }

        var i = 0

        repeatEval {
          (if i == 0 then IO.cede else IO.unit) >> IO {
            i = (i + 1) % 32

            val resume = call.resume

            if resume.isLeft then call = resume.left.get()

            val item = path

            path = None

            item -> resume.isLeft
          }
        }
        .takeWhile(_._2)
        .filterNot(_._1 eq None)
        .map(_._1.get)

      private def travel(color: Int,
                         block: ((Int, Seq[(D, U Either R, Int, Int, Int)])) => Boolean
      ): TailRec[Boolean] =
        for
          r0 <- done(block(color -> Nil))
          r <- tailcall {
            if !r0
            then
              done(r0)
            else
              travel(color, 0, block, Nil)
          }
        yield
          r

      private def travel(color: Int,
                         i: Int,
                         block: ((Int, Seq[(D, U Either R, Int, Int, Int)])) => Boolean,
                         path: Seq[(D, U Either R, Int, Int, Int)]
      ): TailRec[Boolean] =
        if i == just.size
        then
          if depth > 1
          then
            for
              r0 <- done(block(color -> Nil))
              r <- tailcall {
                if !r0
                then
                  done(r0)
                else
                  parent.get.tense.Just.travel(color, block)
              }
            yield
              r
          else
            done(true)
        else
          val (n, intensity: D) =
            just(i) match
              case it: U => it.number -> it.intensity
              case it: R => it.undo.number -> it.undo.intensity
          var j = i - 1
          while j >= 0 && {
            just(j) match
              case it: U => it.number == n
              case it: R => it.undo.number == n
          } do
            j -= 1
          val urru =
            just(i) match
              case it: U => Left(it)
              case it: R => Right(it)
          val node = (intensity, urru, depth, nesting, i-j)
          val path1 = node +: path
          done(!block(color -> path1)).flatMap {
            if _
            then
              done(false)
            else
              val it: B = just(i).asInstanceOf[UndoRedo[B, C, D, K, M, U, R, ?]].path
              tailcall { it.tense.Just.travel(color, 0, block, path1) }.flatMap {
                if _
                then
                  tailcall { travel(color, i + 1, block, path) }
                else
                  done(false)
              }
          }

// Past Simple /////////////////////////////////////////////////////// repeal //

      def repeal(id: Long): TailRec[Unit] =
        for
          _ <- done { just
                       .filterInPlace { case it: UndoRedo[B, C, D, K, M, U, R, ?] =>
                         it.identifier != id
                       }
                    }
          _ <- repeal(0, id)
          _ <- parent.map(_.tense.Just.repeal(id)).getOrElse(done(()))
        yield
          ()

      def repeal(i: Int, id: Long): TailRec[Unit] =
        done(i == just.size).flatMap {
          if _
          then
            done(())
          else
            for
              _ <- tailcall { repeal(i + 1, id) }
              it = just(i).asInstanceOf[UndoRedo[B, C, D, K, M, U, R, ?]]
              _ <- tailcall { it.path.tense.Just.repeal(id) }
            yield
              ()
        }

// move ////////////////////////////////////////////////////////////////////////

  def apply(it: M): D => B = { in =>
    if Feature.Just
    then
      tense.Just(_.move(it)(in))

    if !replica
    then
      if Feature.Have
      then
        tense.Have(it)

    null.asInstanceOf[B]
  }


// undo / redo /////////////////////////////////////////////////////////////////

  protected def apply(it: U, next: Option[R]): R

  protected def apply(it: R): B

  protected def apply(self: B,
                      undo: Option[U],
                      redo: Option[R]): B

  protected def apply(grid: Map[Point, C], clues: Set[K]): Have[K, C, M]

  object ur:

    object move:

      object Undo:

        def apply(u: Option[U] = undo): B =
          val path = Path.this()
          if u.isEmpty
          then
            path
          else
            val it = u.get
            path(it.move)(it.intensity)

      object Redo:

        @tailrec
        def apply(path: B, r: Option[R] = redo): B =
          if r.isEmpty
          then
            path
          else
            val it = r.get
            this(path(it.move)(it.undo.intensity), it.next)

    extension(self: Option[U])
      def replay(r: Option[R] = redo): B =
        if r.isEmpty
        then
          move.Redo(move.Undo())
        else
          val it = r.get
          replay(it.next).ur.Undo(None)

    private def repeal: Unit =
      var i = 0
      while i < just.size
      do
        val it = just(i).asInstanceOf[UndoRedo[B, C, D, K, M, U, R, ?]]
        parent.get.tense.Just.repeal(it.identifier).result
        val n =
          just(i) match
            case it: U => it.number
            case it: R => it.undo.number
        i += 1
        while i < just.size && {
          just(i) match
            case it: U => it.number == n
            case it: R => it.undo.number == n
        } do
          i += 1

    object Undo:

      def apply(p: Option[(Long, Long)]): B =
        require(parent.nonEmpty)
        require(undo.nonEmpty)
        assert(undo.get.path.parent.isEmpty)

        val it = undo.get
        val self = parent.get

        p match

          case Some((elapsed, id)) =>
            it.elapsed ::= elapsed

            if Feature.Just
            then
              self.tense.Just(_.undo(elapsed -> id))

              self.just += it(undo.replay().ur.Undo(None), id)

            if !replica
            then
              repeal

              if Feature.Have
              then
                if !self.have.contains(depth)
                then
                  self.have(depth) = HashSet.empty
                self.have(depth) += Path.this(it.grid, it.clues)

          case _ =>

        Path.this(self, it.next, Some(Path.this(it, redo)))

    object Redo:

      def apply(p: Option[(Long, Long)]): B =
        require(redo.nonEmpty)
        assert(redo.get.path.parent.isEmpty)

        val it = redo.get
        val self = Path.this.asInstanceOf[B]

        p match

          case Some((elapsed, id)) =>
            it.elapsed ::= elapsed

            if Feature.Just
            then
              tense.Just(_.redo(elapsed -> id))

              just += it(undo.replay().ur.Redo(None), id)

          case _ =>

        Path.this(Path.this(it), Some(it.undo), it.next)
