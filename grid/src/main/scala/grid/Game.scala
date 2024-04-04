package urru
package grid

import scala.collection.mutable.{ HashMap, HashSet }
import scala.collection.mutable.{ ListBuffer => MutableList }

import common.grid.{ row, col }
import common.Mutable
import Mutable.given

import tense.intensional.Grid
import UndoRedo._

import Game._


abstract trait Game[T,
  B <: Path[B, C, D, K, M, ?, ?],
  C <: Cell,
  D,
  K <: Clue,
  M <: Move[C, K],
  I <: Item[T, B]
] extends Grid[D, K, C, M, Int, HashMap]:

  def size: Point
  def state: MutableList[I]
  val hints: HashSet[K]

  val counters: Counters
  val pending: MutableList[(Int, HashMap[Int, Int])]
  protected val batch: Mutable[Boolean]

  protected val init: (Set[Point], Map[Point, Cell], Seq[I])

  val features: Map[Feature, Boolean]

  final def clues_hints = (clues -- hints) ++ (hints -- clues)

  protected def full: Boolean =
    grid.keySet == init._1

  def apply(move: M, mutable: Boolean = true): Option[Map[Int, Int]]

  def apply(it: M, pre: Option[Map[Int, Int]]): Boolean =
    pre.map { m =>
      val i = pending(0)._1
      pending.remove(0)

      batch ::= i >= 0

      if batch
      then
        (i, HashMap()) +=: pending

      for
        (j, n) <- m
      do
        if batch then pending(0)._2(j) = n
        for
          _ <- 1 to n
        do
          undo(j)

      batch ::= false

      true
    }.getOrElse(false)

  def move(dir: (Int, Int))(elapsed: Long): Boolean
  def undo()(elapsed: Long): Boolean
  def redo()(elapsed: Long): Boolean

  def restart: this.type

  def status: Boolean


object Game:

  abstract trait Cell:
    val colors: MutableList[Int]
    val group: Seq[Cell]

  abstract trait Clue

  abstract trait Data

  object Data:

    type Id[T] = T
    type Op[T] = Option[T]
    type Mp[T] = Map[T, Seq[Point]]

    abstract trait Upon[K <: Clue, F[_]]:

      val clue: F[K]

////////////////////////////////////////////////////////////////////////////////

  case class Counters(var path: Long,
                      var have: Long,
                      var just: Long):
     inline def path_++ : Long = { path += 1; path }
     inline def have_++ : Long = { have += 1; have }
     inline def just_++ : Long = { just += 1; just }

////////////////////////////////////////////////////////////////////////////////

  import java.util.concurrent.ConcurrentHashMap

  val duals = new ConcurrentHashMap[grid.Grid.Id, Game[?, ?, ?, ?, ?, ?, ?]]()

////////////////////////////////////////////////////////////////////////////////

  enum Feature:

    case Just
    case Have
    case Pisc
    case DnD
