package urru
package game
package fold

import scala.collection.mutable.{ HashMap, HashSet }
import scala.collection.mutable.{ ListBuffer => MutableList }

import cats.effect.IO
import fs2.Stream

import common.grid.{ row, x, col }
import common.{ Mutable, :- }
import Mutable.given

import Clue.*
import UndoRedo.*

import tense.intensional.Data.Doubt

import urru.grid.Grid.Id
import urru.grid.Game.{ Counters, Feature, Savepoint }
import Game.*


case class Game(
  override val id: Id,
  override val size: Point,
  override val grid: HashMap[Point, Cell],
  override val clues: Set[Clue],
  override val features: Map[Feature, Boolean],
  override val state: MutableList[Fold],
  override protected[fold] val init: (Set[Point], Map[Point, Cell], Seq[Fold]),
  override val hints: HashSet[Clue] = HashSet(),
  override val counters: Counters = Counters(0, 0, 0),
  override val savepoint: Savepoint = Savepoint(),
  override val pending: MutableList[(Int, HashMap[Int, Int])] = MutableList(),
  override protected val batch: Mutable[Boolean] = Mutable(false),
  var nowColor: Int = 0,
  var showAxes: Boolean = true,
  var showJust: Option[Boolean] = None,
  var gameOver: Boolean = false,
  var startTime: Long = -1
) extends urru.grid.Game[Play, Path, Cell, Doubt, Clue, Move, Undo, Redo, Fold]:

  val wildcards = clues
    .filter(_.isInstanceOf[Multi])
    .toSet

  urru.grid.Game.duals.put(id, this)

////////////////////////////////////////////////////////////////////////////////

  private def this(id: Id,
                   size: Point,
                   grid: HashMap[Point, Cell],
                   clues: Set[Clue],
                   features: Map[Feature, Boolean],
                   state: MutableList[Fold]) =
    this(id, size, grid, clues, features, state,
         (Game(size, clues), grid.toMap, state.toSeq))

  def this(id: Id, size: Point, clues: Set[Clue], feats: Feature*)
          (grid: Map[Point, Cell], blocks: Block*) =
    this(id,
         size,
         HashMap.from(grid),
         clues,
         Feature.values.foldLeft(Map[Feature, Boolean]()) { (ft, it) => ft + (it -> feats.contains(it)) },
         MutableList.from(blocks.map { it => new Fold(List(it), false) }))

////////////////////////////////////////////////////////////////////////////////

  inline def apply(c: Int): this.type =
    nowColor = c
    this

////////////////////////////////////////////////////////////////////////////////

  override def apply(it: Move, mutable: Boolean): Option[Map[Int, Int]] =
    val i = -it.color-1

    val block = it.block.block.toSet

    val pre = HashMap[Int, Int]()

    if {
      state
        .map(_.play)
        .zipWithIndex
        .foldLeft(true) {
          case (false, _) => false
          case (_, (_, `i`)) => true
          case (_, (ls, j)) =>
            var n = -1
            val r = ls
              .foldLeft(false) {
                case (true, _) => true
                case (_, Block(_, _, _, ps*)) =>
                  n += 1
                  (block & ps.toSet).forall { pt =>
                    wildcards.exists {
                      case Multi(`pt`) => true
                      case _ => false
                    }
                  }
              }
            if r && n > 0 then pre(j) = n
            r
        }
    }
    then

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

    else
      None

////////////////////////////////////////////////////////////////////////////////

  import tense.intensional.Data
  import Data.*

  private def apply(): Move => Doubt = {
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
            pre.foreach { (j, n) =>
              val item = state(j).play
              val ps = item.head.block.toSet -- item.drop(n).head.block.toSet
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

    grid ++= it(grid, wildcards)

    val block = item.play.head(it.block, it.dir)

    item.path(0) = item.path(0)(it)(in)
    item.play = block :: item.play

    check
  }

  override def move(dir: (Int, Int))(elapsed: Long): Boolean =
    val i = -nowColor-1
    val item = state(i)

    if !item.over
    then
      item.play.head(size, dir).flatMap {
        case block if block(clues) =>
          val it = Move(dir, block, block.color, elapsed)
          val in = this()(it)
          Some(it -> in)
        case _ =>
          None
      }.exists {
        case (it, in) if this(it, this(it)) =>
          if item.path(0).redo.exists { r => r.move == it && r.undo.intensity == in }
          then
            redo(elapsed -> i)
          else
            move(it)(in)
          this(-i-1)
          true
        case _ =>
          false
      }
    else
      false

////////////////////////////////////////////////////////////////////////////////

  object Just:

    def travel: Stream[IO, (Int, Seq[(Doubt, Undo Either Redo, Int, Int, Int)])] =
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
          .flatten
          .flatten

////////////////////////////////////////////////////////////////////////////////

  override def undo(p: (Long, Int)) =
    val (elapsed, i) = p
    val item = state(i)
    val it = item.path(0).undo.get

    it.move(grid)(grid -= _)

    item.path(0) = item.path(0).ur.Undo(!batch :- elapsed -> it.number)
    item.play = item.play.tail

    check

  override def undo()(elapsed: Long): Boolean =
    val i = -nowColor-1

    state(i).path(0).undo.exists { it =>
      undo(elapsed -> i)

      batch ::= pending.nonEmpty && pending(0)._1 == i

      if batch
      then
        pending(0)._2.foreach { (j, n) =>
          for
            _ <- 1 to n
          do
            redo(-1L -> j)
        }

        pending.remove(0)

        check

      else
        pending.clear

      batch ::= false

      this(-i-1)

      true

    }

////////////////////////////////////////////////////////////////////////////////

  override def redo(p: (Long, Int)) =
    val (elapsed, i) = p
    val item = state(i)
    val it = item.path(0).redo.get

    grid ++= it.move(grid, wildcards)

    val block = item.play.head(it.move.block, it.move.dir)

    item.path(0) = item.path(0).ur.Redo(!batch :- elapsed -> it.undo.number)
    item.play = block :: item.play

    if !batch
    then
      check

  override def redo()(elapsed: Long): Boolean =
    val i = -nowColor-1

    state(i).path(0).redo.exists { it =>
      if it.undo.intensity == this()(it.move)
      && this(it.move, this(it.move))
      then
        redo(elapsed -> i)

        this(-i-1)

        true
      else
        false

    }

////////////////////////////////////////////////////////////////////////////////

  override def restart =
    pending.clear

    grid.clear
    grid ++= init._2

    for
      i <- 0 until state.size
      item = state(i)
    do
      Path(id, 0, 1) +=: item.path
      state(i) = init._3(i).copy(path = item.path)

    check

    this(-1)

  override def status: Boolean = gameOver && super.full

  def switch: this.type =
    var i = -nowColor-1
    i = (i + 1) % state.size
    this(-i-1)

  private def check: Unit =
    val play = state.map(_.play)
    for
      i <- 0 until state.size
      it = state(i).play.head
    do
      state(i).over = !it(size, clues) || !{
        val color = -i-1
        var fold = false
        for
          dir <- List((-1, 0), (1, 0), (0, -1), (0, 1))
          if !fold
        do
          it(size, dir).foreach { block =>
            fold = this(Move(dir, block, color, -1), mutable = false).nonEmpty
          }
        fold
      }

    gameOver = full || state.map(_.over).forall(identity)

////////////////////////////////////////////////////////////////////////////////


object Game:

  import scala.collection.{ Map => AnyMap }

  // moveable
  extension(self: Block)
    def apply(clues: Set[Clue]): Boolean =
      val i = -self.color-1
      val block = self.block.toSet
      block.forall { pt =>
        !clues.exists {
          case Empty(`pt`) => true
          case _ => false
        }
      }

  // foldable
  extension(self: Block)
    def apply(size: Point, clues: Set[Clue]): Boolean =
      var fold = false
      for
        dir <- List((-1, 0), (1, 0), (0, -1), (0, 1))
        if !fold
      do
        self(size, dir).foreach { block => fold = block(clues) }
      fold

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

  private def apply(clues: Set[Clue], blocks: Seq[Block]): Map[Point, Cell] = blocks
    .foldLeft(Map.empty) { (r1, it) =>
      var r2 = r1
      for
        pt <- it.block
      do
        if r1.contains(pt) // multi
        then
          r2(pt).colors += it.color
        else
          r2 = r2 + {
            pt ->
            Cell(
              MutableList(it.color),
              clues.find {
                case Multi(`pt`) => true
                case _ => false
              }
            )
          }
      r2
    }

  def apply(number: Long, size: Point, clues: Set[Clue], feats: Feature*): Game =
    val id = Id(number)
    val blocks = this(clues)
    val grid = this(clues, blocks)
    val game = new Game(id, size, clues, feats*)(grid, blocks*)
    game.restart
    game
