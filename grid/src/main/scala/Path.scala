package urru

import scala.annotation.tailrec
import scala.collection.mutable.{ HashMap, HashSet }
import scala.collection.mutable.{ ListBuffer => MutableList, StringBuilder }
import scala.util.control.NonLocalReturns.{ returning, throwReturn => thr }
import scala.util.control.TailCalls.{ done, tailcall, TailRec }

import cats.effect.IO
import fs2.Stream
import Stream.repeatEval

import common.grid.{ row, x, col, unary_! }
import common.Mutable.given

import grid.shape

import grid.Game._
import UndoRedo._

import grid.Grid.Id
import grid.Tense._
import grid.tense.intensional.Grid


abstract trait Path[
  B <: Path[B, C, D, K, M, R, U],
  C <: Cell,
  D,
  K <: Clue,
  M <: Move[C, K],
  R <: Redo[B, C, D, K, M, R, U],
  U <: Undo[B, C, D, K, M, U]
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

  protected val pisc: MutableList[StringBuilder]

  inline final def feat(feature: Feature) =
    duals.get(dual).features.contains(feature)

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

      def travel(color: Int): Stream[IO, (Int, Seq[(D, Boolean, Int, Int, Int)])] =
        var path: Option[(Int, Seq[(D, Boolean, Int, Int, Int)])] = None

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
                         block: ((Int, Seq[(D, Boolean, Int, Int, Int)])) => Boolean
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
                         block: ((Int, Seq[(D, Boolean, Int, Int, Int)])) => Boolean,
                         path: Seq[(D, Boolean, Int, Int, Int)]
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
            just(j + 1) match
              case it: U => true
              case it: R => false
          val node = (intensity, urru, depth, nesting, i-j)
          val path1 = node +: path
          if !block(color -> path1)
          then
            done(false)
          else
            for
              r1 <- tailcall {
                val it: B = just(i).asInstanceOf[UndoRedo[B, C, D, K, M, ?]].path
                it.tense.Just.travel(color, 0, block, path1)
              }
              r <- tailcall {
                if !r1
                then
                  done(r1)
                else
                  travel(color, i + 1, block, path)
              }
            yield
              r

// Past Simple /////////////////////////////////////////////////////// repeal //

      def repeal(id: Long): TailRec[Unit] =
        just
          .filterInPlace { case it: UndoRedo[B, C, D, K, M, ?] =>
            !(it.identifier === id)
          }

        for
          _ <- repeal(0, id)
          _ <- parent.map(_.tense.Just.repeal(id)).getOrElse(done(()))
        yield
          ()

      def repeal(i: Int, id: Long): TailRec[Unit] =
        if i == just.size
        then
          done(())
        else
          val it = just(i).asInstanceOf[UndoRedo[B, C, D, K, M, ?]]
          for
            _ <- tailcall { it.path.tense.Just.repeal(id) }
            _ <- tailcall { repeal(i + 1, id) }
          yield
            ()

// move ////////////////////////////////////////////////////////////////////////

  def apply(it: M): D => B = { in =>
    if feat(Feature.Have)
    then
      tense.Just(_.move(it)(in))

    if !replica
    then
      if feat(Feature.Have)
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

    private object move:

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
        val it = just(i).asInstanceOf[UndoRedo[B, C, D, K, M, ?]]
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

    import PiScala.apply

    object Undo:

      def apply(id: Option[Long]): B =
        require(parent.nonEmpty)
        require(undo.nonEmpty)
        assert(undo.get.path.parent.isEmpty)

        val it = undo.get
        val self = parent.get

        if id.nonEmpty
        then
          if feat(Feature.Just)
          then
            self.tense.Just(_.undo(id.get))

            self.just += it(undo.replay().ur.Undo(None), id.get)

          else if feat(Feature.Pisc)
          then
            self.just += it

          if !replica
          then
            if feat(Feature.Pisc)
            then
              if depth > 1
              then
                self(it, self.just.size >= 2 && self.just(self.just.size - 2).isInstanceOf[R]) // PiScala

            repeal

            if feat(Feature.Have)
            then
              if !self.have.contains(depth)
              then
                self.have(depth) = HashSet.empty
              self.have(depth) += Path.this(it.grid, it.clues)

        Path.this(self, it.next, Some(Path.this(it, redo)))

    object Redo:

      def apply(id: Option[Long]): B =
        require(redo.nonEmpty)
        assert(redo.get.path.parent.isEmpty)

        val it = redo.get
        val self = Path.this.asInstanceOf[B]

        if id.nonEmpty
        then
          if feat(Feature.Just)
          then
            tense.Just(_.redo(id.get))

            just += it(undo.replay().ur.Redo(None), id.get)

          else if feat(Feature.Pisc)
          then
            just += it

        Path.this(Path.this(it), Some(it.undo), it.next)

  /**
    * @see [[https://github.com/sjbiaga/pisc]]
    */
  object PiScala:

    import Pisc._

    def apply(): (String, String) =
      var self = Path.this.asInstanceOf[B]
      var temp = self.pisc.map(_.result()).map(StringBuilder(_))
      while self.depth > 1
      do
        val p = self.parent.get
        val pisc = p.pisc.map(_.result()).map(StringBuilder(_))
        self(temp, pisc, p.just.nonEmpty && p.just.last.isInstanceOf[R])
        temp = pisc
        self = p
      if temp.head.isEmpty
      then
        temp.head.append("𝟎")
      temp.head.result() -> temp.tail.map(_.result()).mkString("\n")

    extension(self: B)
      private[Path] def apply(temp: MutableList[StringBuilder],
                              pisc: MutableList[StringBuilder],
                              redo: Boolean): Unit =
        val it = self.undo.get.move
        val dnd = feat(Feature.DnD)
        val id = uuid(if !it.dir then "DnD" else "Move") + s"(${args(dnd)})"
        val dir = dirs(it)
        var arg = s"${self.depth-1}"
        if it.isInstanceOf[shape.By]
        then
          arg = s"/*$arg -> ${it.asInstanceOf[shape.By].by}*/"
        if pisc(0).nonEmpty
        then
          pisc(0).append(" + ")
        if redo
        then
          pisc(0).append(s"redo<$dir> . ")
        pisc(0).append(s"$dir<$arg> . $id")
        if temp.head.isEmpty
        then
          temp.head.append("𝟎")
        pisc += StringBuilder(s"$id = ${temp.head}")
        pisc ++= temp.tail

      private[Path] def apply(it: U, redo: Boolean): Unit =
        val dnd = feat(Feature.DnD)
        val id = uuid("Urru") + s"(${args(dnd)})"
        val dir = dirs(it.move)
        var arg = s"${depth-1}"
        if it.move.isInstanceOf[shape.By]
        then
          arg = s"/*$arg -> ${it.move.asInstanceOf[shape.By].by}*/"
        if redo
        then
          if self.pisc(0).nonEmpty
          then
            self.pisc(0).append(" + ")
          self.pisc(0).append(s"redo<$dir> . $dir<$arg> . $id")
        if self.pisc(0).nonEmpty
        then
          self.pisc(0).append(" + ")
        self.pisc(0).append(s"undo<$dir> . $dir<$arg> . $id")
        if pisc.head.isEmpty
        then
          pisc.head.append("𝟎")
        self.pisc += StringBuilder(s"$id = ${pisc.head}")
        self.pisc ++= pisc.tail

////////////////////////////////////////////////////////////////////////////////
