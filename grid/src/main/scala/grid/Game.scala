package urru
package grid

import scala.collection.mutable.{ HashMap, HashSet }
import scala.collection.mutable.{ ListBuffer => MutableList }

import common.grid.{ row, col }
import common.Mutable
import Mutable.given

import tense.intensional.Grid
import UndoRedo.*

import Game.*


abstract trait Game[T,
  B <: Path[B, C, D, K, M, U, R],
  C <: Cell,
  D,
  K <: Clue,
  M <: Move[C, K],
  U <: Undo[B, C, D, K, M, U, R],
  R <: Redo[B, C, D, K, M, U, R],
  I <: Item[T, B]
] extends Grid[D, K, C, M, Int, HashMap]:

  def size: Point
  def state: MutableList[I]
  val hints: HashSet[K]

  val counters: Counters
  val savepoint: Savepoint
  val pending: MutableList[(Int, HashMap[Int, Int])]
  protected val batch: Mutable[Boolean]

  protected val init: (Set[Point], Map[Point, Cell], Seq[I])

  val features: Map[Feature, Boolean]

  final def clues_hints = (clues -- hints) ++ (hints -- clues)

  protected def full: Boolean =
    grid.keySet == init._1

  def apply(move: M, mutable: Boolean = true): Option[Map[Int, Int]]

  def apply(it: M, pre: Option[Map[Int, Int]]): Boolean =
    pre.exists { m =>
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
          undo(-1L -> j)

      batch ::= false

      true
    }

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

  object Counters:

    object http4s:

      import cats.effect.IO

      import io.circe.generic.auto.*

      import org.http4s.circe.{ jsonEncoderOf, jsonOf }
      import org.http4s.{ EntityDecoder, EntityEncoder }

      given EntityDecoder[IO, Counters] = jsonOf
      given EntityEncoder[IO, Counters] = jsonEncoderOf

////////////////////////////////////////////////////////////////////////////////

  case class Savepoint(var current: Option[String] = None,
                       var previous: Option[String] = None)

  object Savepoint:

    object http4s:

      import cats.effect.IO

      import io.circe.generic.auto.*

      import org.http4s.circe.{ jsonEncoderOf, jsonOf }
      import org.http4s.{ EntityDecoder, EntityEncoder }

      given EntityDecoder[IO, Savepoint] = jsonOf

////////////////////////////////////////////////////////////////////////////////

  import java.util.concurrent.ConcurrentHashMap

  val duals = new ConcurrentHashMap[grid.Grid.Id, Game[?, ?, ?, ?, ?, ?, ?, ?, ?]]()

////////////////////////////////////////////////////////////////////////////////

  enum Feature:

    case Just, Have, DnD

  object Feature:

    object http4s:

      import scala.util.Try

      import cats.effect.IO

      import io.circe.generic.auto.*

      import io.circe.{ KeyDecoder, KeyEncoder }

      import org.http4s.circe.{ jsonEncoderOf, jsonOf }
      import org.http4s.{ EntityDecoder, EntityEncoder }

      given EntityDecoder[IO, Feature] = jsonOf
      given EntityEncoder[IO, Feature] = jsonEncoderOf

      given KeyDecoder[Feature] = KeyDecoder.instance(it => Try(Feature.valueOf(it)).toOption)
      given KeyEncoder[Feature] = KeyEncoder[String].contramap(_.toString)
