package urru
package game
package fill

import scala.collection.mutable.{ HashMap, HashSet, LinkedHashMap, Queue }
import scala.collection.mutable.{ ListBuffer => MutableList }

import cats.effect.IO
import fs2.Stream

import common.grid.{ row, x, col, unary_-, unary_! }
import common.{ Mutable, :- }

import Clue._
import UndoRedo._

import tense.intensional.Data.Doubt

import urru.grid.Grid.Id
import urru.grid.Game.{ Counters, Feature }
import Game._
import DnD._


case class Game(
  override val id: Id,
  override val size: Point,
  override val grid: HashMap[Point, Cell],
  override val clues: Set[Clue],
  override val features: Map[Feature, Boolean],
  override val state: MutableList[Fill],
  override val hints: HashSet[Clue] = HashSet(),
  override val counters: Counters = Counters(0, 0, 0),
  override val pending: MutableList[(Int, HashMap[Int, Int])] = MutableList(),
  override protected val batch: Mutable[Boolean] = Mutable(false),
  var selectionMode: Int = 0,
  var nowPlay: Play = null,
  var showAxes: Boolean = true,
  var showJust: Option[Boolean] = None,
  var showPad: Boolean = true,
  var gameOver: Boolean = false,
  var startTime: Long = -1,
  var minusTime: Long = -1
) extends urru.grid.Game[Seq[Play], Path, Cell, Doubt, Clue, Move, Fill]:

  override protected[fill] val init: (Set[Point], Map[Point, Cell], Seq[Fill]) =
    (Game(size, clues), Map.empty, state.toSeq)

  val wildcards = clues
    .filter(_.isInstanceOf[Multi])
    .toSet

  urru.grid.Game.duals.put(id, this)

  restart

////////////////////////////////////////////////////////////////////////////////

  def this(id: Id, size: Point, clues: Set[Clue], feats: Feature*)
          (blocks: Block*) =
    this(id,
         size,
         HashMap(),
         clues,
         Feature.values.foldLeft(Map[Feature, Boolean]()) { (ft, it) => ft + (it -> feats.contains(it)) } + (Feature.DnD -> true ),
         MutableList.from(blocks.map { it => new Fill(Seq(Move(it)), false) }))

////////////////////////////////////////////////////////////////////////////////

  def apply(c: Int): this.type =
    val i = -c-1
    nowPlay = state(i).play.head
    this

////////////////////////////////////////////////////////////////////////////////

  override def apply(it: Move, mutable: Boolean): Option[Map[Int, Int]] =
    val i = -it.color-1
    val Move(dir, _, _, _) = it

    val pre = LinkedHashMap[Int, Int]()

    if dir != (0, 0)
    then

      val play = state
        .map(_.play)
        .zipWithIndex
        .filterNot(_._1.head.pad)

      def shift(i: Int, block: Set[Point]): Boolean =
        val ns = HashMap[Int, Int]()
        play
          .foldLeft(true) {
            case (false, _) => false
            case (_, (_, `i`)) => true
            case (_, (ls, j)) =>
              var n = -1
              ls
                .map(_.block)
                .foldLeft(false) {
                  case (true, _) => true
                  case (_, Block(_, _, _, _, _, _, _, _, ps*)) =>
                    n += 1
                    (block & ps.toSet).forall { pt =>
                      wildcards.exists {
                        case Multi(`pt`) => true
                        case _ => false
                      }
                    }
                }
              if n > 0
              then
                if pre.contains(j)
                then
                  false
                else
                  pre(j) = n
                  while n > 0
                  do
                    n -= 1
                    val ps = state(j).play.drop(pre(j) - n).head.block.block.toSet
                    if !shift(j, ps)
                    then
                      n = -1
                  n == 0
              else
                true
          }

      pre += i -> 0

      if !shift(i, it.block.block.toSet)
      then
        return None

      pre -= i

    if mutable
    then

      if pending.exists { (j, m) => j == i || m.contains(i) }
      then
        pending.clear

      if pre.nonEmpty
      then
        (i, null) +=: pending
      else
        (-1, null) +=: pending

    Some(pre.toMap)

////////////////////////////////////////////////////////////////////////////////

  import tense.intensional.Data
  import Data._

  private[fill] def apply(): Move => Doubt = {
    case _ if !features(Feature.Just) =>
      Doubt(Set.empty)

    case it =>
      val i = -it.color-1

      val clash = MutableList[Data[?]]()
      this(it, mutable = false)
        .foreach { pre =>
          if pre.isEmpty
          then
            clash += Backtrack(it)
          else
            val pair = HashMap[Int, Seq[Point]]()
            pre.foreach { (j, _) =>
              val item = state(j).play
              val ps = item.head.block.block.toSet

              val color = -j-1
              pair += color -> ps.toSeq
            }
            clash += Backlash(it, pair.toMap)
        }

      Doubt(clash.toSet)
  }

////////////////////////////////////////////////////////////////////////////////

  override def move(it: Move) = { in =>
    val i = -it.color-1
    val item = state(i)

    item.play.head(grid)(grid -= _)

    item.path(0) = item.path(0)(it)(in)
    item.play = it +: item.play

    grid ++= item.play.head(grid, wildcards)

    check
  }

  private[fill] def apply(it: Move, in: Doubt)(elapsed: Long): Boolean =
    val i = -it.color-1
    val item = state(i)

    if this(it, this(it))
    then
      if {
        item.path(0).redo.exists { r =>
          if r.move == it
          && r.undo.intensity == in
          then
            minusTime -= r.move.elapsed
            minusTime += r.undo.elapsed + elapsed
            true
          else
            false
        }
      }
      then
        redo(i)
      else
        move(it)(in)

      this(-i-1)

      true

    else

      false

  override def move(dir: (Int, Int))(elapsed: Long): Boolean =
    val i = -nowPlay.color-1
    val item = state(i)

    if !nowPlay.pad && !item.over
    then
      nowPlay.block(size, dir).flatMap {
        case block if nowPlay.block(size, dir, clues) =>
          if item.path(0).undo.exists(_.move.dir == -dir)
          then
            undo()(elapsed) :- None

          else
            val it = Move(dir, block, block.color, elapsed)
            val in = this()(it)
            Some(Some(it -> in))

        case _ =>
          None

      }.exists {
        case Some((it, in)) =>
          this(it, in)(elapsed)

        case _ =>
          true
      }

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
                val color = -i-1
                Stream.emits(it.path.map(_.tense.Just.travel(color)))
              }
          }
          .parJoinUnbounded
          .parJoinUnbounded

////////////////////////////////////////////////////////////////////////////////

  override def undo(i: Int) =
    val item = state(i)
    val it = item.path(0).undo.get

    item.play.head(grid)(grid -= _)

    item.path(0) = item.path(0).ur.Undo(!batch :- it.number)
    item.play = item.play.tail

    grid ++= item.play.head(grid, wildcards)

    check

  override def undo()(elapsed: Long): Boolean =
    val i = -nowPlay.color-1
    val item = state(i)

    if item.path(0).depth > 1
    then
      val it = item.path(0).undo.get
      val Move(dir, _, _, _) = it.move

      if (if !dir then dir != (0, 0)
                    || nowPlay(item.play.last.block.block*)(size, dir, clues, grid)
                  else nowPlay.block(size, -dir, clues, grid))
      then
        undo(i)

        batch ::= pending.nonEmpty && pending(0)._1 == i

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

        this(-i-1)

        true

      else
        false

    else
      false

////////////////////////////////////////////////////////////////////////////////

  override def redo(i: Int) =
    val item = state(i)
    val it = item.path(0).redo.get

    item.play.head(grid)(grid -= _)

    item.path(0) = item.path(0).ur.Redo(!batch :- it.undo.number)
    item.play = it.move +: item.play

    grid ++= item.play.head(grid, wildcards)

    if !batch
    then
      check

  override def redo()(elapsed: Long): Boolean =
    val i = -nowPlay.color-1
    val item = state(i)

    item.path(0).redo.exists { it =>
      val Move(dir, _, _, _) = it.move

      if !dir || nowPlay.block(size, dir, clues, grid)
      then

        if it.undo.intensity == this()(it.move)
        && this(it.move, this(it.move))
        then
          redo(i)

          minusTime -= it.move.elapsed
          minusTime += it.undo.elapsed + elapsed

          this(-i-1)

          true

        else
          false

      else
        false

    }

////////////////////////////////////////////////////////////////////////////////

  override def restart =
    pending.clear

    selectionMode = 0

    grid.clear

    for
      i <- 0 until state.size
    do
      var item = state(i)
      Path(id, 0, 0) +=: item.path
      state(i) = init._3(i).copy(path = item.path)
      item = state(i)
      grid ++= item.play.head(grid, wildcards) // trick
      nowPlay = item.play.head
      this.dragOut

    check

    minusTime = 0

    this(-1)

  override def status: Boolean = gameOver && super.full

  def switch: this.type =
    var i = -nowPlay.color-1
    i = (i + 1) % state.size
    this(-i-1)

  private def check: Unit =
    for
      i <- 0 until state.size
      it = state(i).play.head
    do
      state(i).over = !it.pad && !it.block(size, clues)

    gameOver = full //&& state.map(_.over).forall(identity)

////////////////////////////////////////////////////////////////////////////////


object Game:

  import scala.collection.{ Map => AnyMap }

  extension(self: Block)
    // moveable w/o grid
    def apply(size: Point, dir: (Int, Int), clues: Set[Clue]): Boolean =
      val i = -self.color-1
      self(size)(dir).exists {
        _.forall { pt =>
          !clues.exists {
            case Empty(`pt`) => true
            case _ => false
          }
        }
      }

    // moveable w/ grid
    def apply(size: Point, dir: (Int, Int), clues: Set[Clue], grid: AnyMap[Point, Cell]): Boolean =
      val i = -self.color-1
      self(size)(dir).exists { ps =>
        ps.forall { pt =>
          !clues.exists {
            case Empty(`pt`) => true
            case _ => false
          }
        }
        && (ps & grid.keySet).forall { pt =>
          clues.exists {
            case Multi(`pt`) => true
            case _ => false
          }
        }
      }

    // draggable
    def apply(size: Point, clues: Set[Clue]): Boolean =
      var drag = false
      for
        dir <- List((-1, 0), (1, 0), (0, -1), (0, 1))
        if !drag
      do
        drag = self(size, dir, clues)
      drag

  private def apply(size: Point, clues: Set[Clue]): Set[Point] =
    clues
      .foldLeft {
        ( for
            row <- 1 to size.row
            col <- 1 to size.col
          yield
            row x col
        ).toSet
      } {
        case (r, Empty(it)) => r - it
        case (r, _) => r
      }

  private def apply(clues: Set[Clue]): Seq[Block] = clues
    .filter(_.isInstanceOf[Block])
    .map(_.asInstanceOf[Block])
    .toSeq
    .sortBy(-_.color-1)

  def apply(number: Long, size: Point, clues: Set[Clue], feats: Feature*): Game =
    val id = Id(number)
    val blocks = this(clues)
    new Game(id, size, clues, feats*)(blocks*)
