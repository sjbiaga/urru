package urru
package game
package flow

import scala.collection.mutable.{ HashMap, HashSet }
import scala.collection.mutable.{ ListBuffer => MutableList }
import scala.util.control.NonLocalReturns.{ returning, throwReturn => thr }

import cats.effect.IO
import fs2.Stream

import common.grid.{ row, x, col }
import common.{ Mutable, Spiral, :- }
import Spiral.magic0
import Mutable.given

import Clue._
import UndoRedo._

import tense.intensional.Data.Doubt

import urru.grid.Grid.Id
import urru.grid.Game.{ Counters, Feature }
import Game._


case class Game(
  override val id: Id,
  override val size: Point,
  override val grid: HashMap[Point, Cell],
  override val clues: Set[Clue],
  override val features: Map[Feature, Boolean],
  override val state: MutableList[Flow],
  override val hints: HashSet[Clue] = HashSet(),
  override val counters: Counters = Counters(0, 0, 0),
  override val pending: MutableList[(Int, HashMap[Int, Int])] = MutableList(),
  override protected val batch: Mutable[Boolean] = Mutable(false),
  var nowStart: (Int, Start) = (0, null),
  var showAxes: Boolean = true,
  var showJust: Option[Boolean] = None,
  var gameOver: Boolean = false,
  var startTime: Long = -1,
  var minusTime: Long = -1
) extends urru.grid.Game[Play, Path, Cell, Doubt, Clue, Move, Flow]:

  override protected[flow] val init: (Set[Point], Map[Point, Cell], Seq[Flow]) =
    (Game(size, clues, Spiral(size, 0)), grid.toMap, state.toSeq)

  private val crosses = clues
    .filter(_.isInstanceOf[Cross])
    .toSet

  urru.grid.Game.duals.put(id, this)

  restart

////////////////////////////////////////////////////////////////////////////////

  def this(id: Id, size: Point, clues: Set[Clue], feats: Feature*)
          (grid: Map[Point, Cell], start: Start*) =
    this(id,
         size,
         HashMap.from(grid),
         clues,
         Feature.values.foldLeft(Map[Feature, Boolean]()) { (ft, it) => ft + (it -> feats.contains(it)) },
         MutableList.from(start.map { it => new Flow(Seq(it.at), false) }))

////////////////////////////////////////////////////////////////////////////////

  def apply(c: Int): this.type =
    val start = clues
      .find {
        case Start(_, _, `c`) => true
        case _ => false
      }
      .get.asInstanceOf[Start]
    val odd = state.map(_.play).indexWhere(_.head == start.at) % 2
    nowStart = odd -> start
    this

  def tip: Point =
    val (odd, start) = nowStart
    val i = -start.color-1
    state(2*i+odd).play.last

////////////////////////////////////////////////////////////////////////////////

  override def apply(it: Move, mutable: Boolean): Option[Map[Int, Int]] =
    val Move(odd, (row1, col1), by, _, _) = it
    val i = -it.color-1
    val i0 = 2*i+odd
    val i1 = 2*i+1-odd

    var pre = Map[Int, Int]()

    def ncol(pt: Point)(cb: => Unit): Unit =
      if crosses.exists {
        case Cross(`by`) => true
        case _ => false
      }
      then
        val (row2, col2) = pt
        if row1 != row2 && col1 != col2
        then // not collinear
          cb

    returning {
      state
        .map(_.play)
        .zipWithIndex
        .foreach {
          case (_, `i0`) =>
          case (ls, `i1`) =>
            var n = -ls.indexOf(by)
            if n <= 0
            then
              if n < 0
              then
                ncol(ls(-n-1)) { pre = null }
                if pre eq null then thr(())
              n += ls.size - 1
              if n > 0
              then
                pre = pre + (i1 -> n)
          case (ls, j) =>
            var n = -ls.indexOf(by)
            if n < 0
            then
              ncol(ls(-n-1)) { n = 0 }
            if n < 0
            then
              n += ls.size
              pre = pre + (j -> n)
        }
    }

    if pre ne null
    then

      if mutable
      then

        if pending.exists { (j, m) => j == i0 || m.contains(i0) }
        then
          pending.clear

        if pre.nonEmpty
        then
          (i0, null) +=: pending
        else
          (-1, null) +=: pending

      Some(pre)

    else
      None

////////////////////////////////////////////////////////////////////////////////

  import tense.intensional.Data
  import Data._

  private def apply(): Move => Doubt = {
    case _ if !features(Feature.Just) =>
      Doubt(Set.empty)

    case it @ Move(odd, _, by, _, _) =>
      val i = -it.color-1

      var color = 0

      val clash = MutableList[Data[?]]()
      this(it, mutable = false)
        .foreach(_.headOption
          .map { (j, n) =>
            val ls = state(j).play
            val at = ls.drop(ls.size-n-1).head
            if j == 2*i+1-odd
            then
              clash += Backtrack(it, at, true)
            else
              color = -j/2-1
              clash += Backlash(it, at, color, state(j).over)
          }.getOrElse {
            var ls = state(2*i+odd).play
            val at = ls.take(1 max (ls.size-1)).last
            val j = 2*i+1-odd
            ls = state(j).play
            clash += Backtrack(it, at, ls.last == by)
          }
        )

      val cross = MutableList[Data[?]]()
      crosses.foreach {
        case clue @ Cross(`by`) =>
          if !grid.contains(by)
          then
            cross += HalfCross(it, clue)
          else
            color =
              if color == 0
              then
                grid(by).colors.head
              else if clash.head.asInstanceOf[Backlash].side
              then
                (Set.from(grid(by).colors) - color).head
              else
                0
            if color != 0
            then
              cross += FullCross(it, clue, color)
        case _ =>
      }

      val track = MutableList[Data[?]]()
      if !grid.contains(by)
      then
        clues.foreach {
          case clue @ Track(_, path*) if path.contains(by) =>
            val ls = state(2*i+odd).play :+ by
            ( if ls.contains(path.head)
              then path.filter(grid.contains).lastOption
              else path.filter(grid.contains).headOption
            ).foreach { at =>
              val color = grid(at).colors.head
              track += Pullout(it, clue, at, color)
            }
          case _ =>
        }

      Doubt((clash ++ cross ++ track).toSet)
  }

////////////////////////////////////////////////////////////////////////////////

  override def move(it: Move) = { in =>
    val Move(odd, (row1, col1), by, _, _) = it
    val i = -it.color-1
    val item = state(2*i+odd)

    grid ++= it(grid, crosses)

    val open = state(2*i+1-odd).play.last != by

    if !open
    then
      state(2*i+1-odd).over = !open

    item.path(0) = item.path(0)(it)(in)
    item.play = item.play :+ by
    item.over = !open

    check
  }

  override def move(dir: (Int, Int))(elapsed: Long): Boolean =
    val (odd, start) = nowStart
    val color = start.color
    val i = -color-1

    if !state(2*i+odd).over
    then
      val at = tip
      Move(size, at, dir).flatMap {
        case by if { (color, at, by)(clues, grid, state(2*i+odd).play) } =>
          val it = Move(odd, at, by, color, elapsed)
          val in = this()(it)
          Some(it -> in)
        case _ =>
          None
      }.map {
        case (it, in) if this(it, this(it)) =>
          if {
            state(2*i+odd).path(0).redo.map { r =>
              if r.move == it
              && r.undo.intensity == in
              then
                minusTime -= r.move.elapsed
                minusTime += r.undo.elapsed + elapsed
                true
              else
                false
            }.getOrElse(false)
          }
          then
            redo(2*i+odd)
          else
            move(it)(in)
          true
        case _ =>
          false
      }.getOrElse(false)
    else
      false

////////////////////////////////////////////////////////////////////////////////

  object Just:

    import cats.effect.kernel.Concurrent

    def travel(using Concurrent[IO]): Stream[IO, (Int, Seq[(Doubt, Boolean, Int, Int, Int)])] =
      if !features(Feature.Just)
      then
        Stream.empty
      else
        Stream
          .emits {
            state
              .zipWithIndex
              .map { (it, i) =>
                val color = -i/2-1
                Stream.emits(it.path.map(_.tense.Just.travel(color)))
              }
          }
          .parJoinUnbounded
          .parJoinUnbounded

////////////////////////////////////////////////////////////////////////////////

  override def undo(i: Int) =
    val item = state(i)
    val it = item.path(0).undo.get
    val Move(odd, _, _, _, _) = it.move

    it.move(grid)(grid -= _)

    state(2*(i/2)+1-odd).over = false

    item.path(0) = item.path(0).ur.Undo(!batch :- it.number)
    item.play = item.play.init
    item.over = false

    check

  override def undo()(elapsed: Long): Boolean =
    val (odd, start) = nowStart
    val i = -start.color-1

    state(2*i+odd).path(0).undo.map { it =>
      undo(2*i+odd)

      batch ::= pending.nonEmpty && pending(0)._1 == 2*i+odd

      if batch
      then
        pending(0)._2.foreach { (j, n) =>
          for
            _ <- 1 to n
          do
            redo(j)
        }

        pending.remove(0)

        check

      else
        minusTime += it.move.elapsed
        it.elapsed = elapsed

        pending.clear

      batch ::= false

      true

    }.getOrElse(false)

////////////////////////////////////////////////////////////////////////////////

  override def redo(i: Int) =
    val item = state(i)
    val it = item.path(0).redo.get
    val Move(odd, at, by, color, _) = it.move

    grid ++= it.move(grid, crosses)

    val open = state(2*(i/2)+1-odd).play.last != by

    if !open
    then
      state(2*(i/2)+1-odd) = state(2*(i/2)+1-odd).copy(over = !open)

    item.path(0) = item.path(0).ur.Redo(!batch :- it.undo.number)
    item.play = item.play :+ by
    item.over = !open

    if !batch
    then
      check

  override def redo()(elapsed: Long): Boolean =
    val (odd, start) = nowStart
    val i = -start.color-1

    state(2*i+odd).path(0).redo.map { it =>
      if it.undo.intensity == this()(it.move)
      && this(it.move, this(it.move))
      then
        redo(2*i+odd)

        minusTime -= it.move.elapsed
        minusTime += it.undo.elapsed + elapsed

        true

      else
        false

    }.getOrElse(false)

////////////////////////////////////////////////////////////////////////////////

  override def restart =
    pending.clear

    grid.clear
    grid ++= init._2

    for
      i <- 0 until state.size
    do
      val item = state(i)
      Path(id, 0, 1) +=: item.path
      state(i) = init._3(i).copy(path = item.path)

    check

    minusTime = 0

    this(-1)

  override def status = gameOver && super.full

  def toggle: Unit =
    val (odd, Start(_, by, _)) = nowStart
    val start = clues
      .find { case Start(`by`, _, _) => true case _ => false }
      .get.asInstanceOf[Start]
    nowStart = 1-odd -> start

  private def check: Unit =
    gameOver = state.map(_.over).forall(identity)

////////////////////////////////////////////////////////////////////////////////


object Game:

  import scala.collection.{ Map => AnyMap }

  // moveable
  extension(self: (Int, Point, Point))
    def apply(clues: Set[Clue], grid: AnyMap[Point, Cell], ls: Play): Boolean =
      val (row1, col1) = ls.init.lastOption.getOrElse((0,0))
      val (color, at, by @ (row2, col2)) = self
      !ls.contains(by)
      &&
      !clues.exists {
        case Start(`by`, _, `color`) => false
        case Start(`by`, _, _)
           | Strip(`at`, `by`) | Strip(`by`, `at`) => true
        case _ => false
      }
      &&
      clues.forall {
        case Cross(`at`) => row1 == row2 || col1 == col2
        case _ => true
      }

  // empty or framed = closed
  private def apply(size: Point, clues: Set[Clue], spiral: Spiral): Set[Point] =
    ( for
        row <- 1 to size.row
        col <- 1 to size.col
      yield
        row x col
    ).foldLeft(Set[Point]()) {
      case (r, it) if r.contains(it) => r
      case (r, it)
          if clues.exists {
            case Start(`it`, _, _) => true
            case _ => false
          } => r + it
      case (r, it) =>
        val path: ((Point, Point), Set[Point], Boolean) => (Option[Boolean], Boolean) =
          { case ((at, pt), _, _)
                if clues.exists {
                  case Empty(`pt`) | Strip(`at`, `pt`) | Strip(`pt`, `at`) => true
                  case _ => false
                } => Some(false) -> false
            case ((_, pt), _, _)
                if clues.exists {
                  case Start(`pt`, _, _) => true
                  case _ => false
                } => None -> true
            case _ => Some(true) -> false
          }
        spiral(true, false, identity)(magic0(it)(size))(path) match
          case (ps, true) => r ++ ps
          case _ => r
      }

  private def apply(clues: Set[Clue]): Seq[Start] = clues
    .filter(_.isInstanceOf[Start])
    .map(_.asInstanceOf[Start])
    .toSeq
    .sortBy(-_.color-1)

  private def apply(start: Seq[Start]): Map[Point, Cell] =
    start.foldLeft(Map.empty) {
      case (r, it @ Start(at, _, color)) =>
        r + (at -> Cell(MutableList(color), Some(it)))
    }

  def apply(number: Long, size: Point, clues: Set[Clue], feats: Feature*): Game =
    val id = Id(number)
    val start = this(clues)
    val grid = this(start)
    val open = this(size, clues, Spiral(size, 0))
    val empty = (grid.keySet -- open).map(Empty(_))
    new Game(id, size, clues ++ empty, feats*)(grid, start*)
