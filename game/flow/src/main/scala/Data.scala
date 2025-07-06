package urru
package game
package flow

import tense.extensional.Data.Fact
import tense.intensional.Data.Doubt


case class Data(fact: Fact, doubt: Doubt)
    extends grid.Game.Data


package tense:

  import common.grid.{ adj, row, x, col }

  import grid.Game.Data.*
  import grid.shape

  import Clue.*

  package extensional:

    sealed abstract trait Data[K <: Clue, F[_]]
        extends grid.Game.Data, Upon[K, F]:

      val colors: Seq[Int]


    object Data:

      case class Fact(some: List[Data[?, ?]], more: Fact*)


      case class Overlapping(override val clue: Option[Start],
                             override val colors: Int*)
          extends Data[Start, Op]


      case class DoubleCross(override val clue: Cross,
                             override val colors: Int*)
          extends Data[Cross, Id]


      case class Trespassing(override val clue: Strip,
                             override val colors: Int*)
          extends Data[Strip, Id]


      case class IntoThePit(override val clue: Empty,
                            override val colors: Int*)
          extends Data[Empty, Id]


      case class OffCourse(override val clue: Frame,
                           override val block: Seq[Point],
                           override val colors: Int*)
          extends Data[Frame, Id], shape.Block:

        override val min: (Int, Int) = block.map(_.row).min -> block.map(_.col).min
        override val max: (Int, Int) = block.map(_.row).max -> block.map(_.col).max


      case class Collision(override val clue: Track,
                           override val at: Point,
                           override val by: Point,
                           override val colors: Int*)
          extends Data[Track, Id], shape.By


  package intensional:

    sealed abstract trait Data[F[_]]
        extends grid.Game.Data
        with grid.tense.intensional.Self[Clue, Cell, Move]
        with shape.At:

      override val at: Point = move.by

      // the "pair" color
      val pair: F[Int]

      // the least applicable conditions
      def apply(grid: Map[Point, Cell], over: Boolean*): Boolean


    object Data:

      case class Doubt(data: Set[Data[?]])

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

      /**
        * The move occurred without hindrance: you doubt that this move
        * should occur.
        */
      case class Backtrack(override val move: Move,
                           override val at: Point, // prior to move.at
                           over: Boolean)
          extends Data[Op]:

        override val pair: Option[Int] = None

        val turn = at != move.at && !(move.by.row == at.row || move.by.col == at.col)

        override def apply(_grid: Map[Point, Cell], over: Boolean*): Boolean =
          val Move(odd, _, _, color, _) = move
          val i = -color-1
          this.over == over(2*i+odd)

////////////////////////////////////////////////////////////////////////////////

      /**
        * The move occurred at a cross clue free in both directions: you doubt
        * that the color or direction of this move should cross the clue.
        */
      case class HalfCross(override val move: Move,
                           override val clue: Cross)
          extends Data[Op], Upon[Cross, Id]:
        require(at == clue.at)

        override val pair: Option[Int] = None

        override def apply(grid: Map[Point, Cell], over: Boolean*): Boolean =
          !grid.contains(at)

////////////////////////////////////////////////////////////////////////////////

      /**
        * The move occurred at a cross clue free in only one direction: you doubt
        * that the color or direction of this move should cross the clue opposite
        * to the already crossing pair.
        */
      case class FullCross(override val move: Move,
                           override val clue: Cross,
                           override val pair: Int)
          extends Data[Id], Upon[Cross, Id]:
        require(at == clue.at)

        override def apply(grid: Map[Point, Cell], over: Boolean*): Boolean =
          grid.contains(at) && grid(at).colors.contains(pair)

////////////////////////////////////////////////////////////////////////////////

      /**
        * The move clashed with the pair: you doubt that this move
        * should be in place instead.
        */
      case class Backlash(override val move: Move,
                          override val at: Point, // pair prior to move.by
                          override val pair: Int,
                          over: Boolean)
          extends Data[Id]:

        val side = !(move.at.row == at.row || move.at.col == at.col)

        override def apply(grid: Map[Point, Cell], over: Boolean*): Boolean =
          val j = -pair-1
          grid.contains(at) && grid(at).colors.contains(pair) && this.over == over(2*j)

////////////////////////////////////////////////////////////////////////////////

      /**
        * The move entered a track already "occupied" by the pair
        * on the other side: you doubt that this move should occupy
        * the track instead.
        */
      case class Pullout(override val move: Move,
                         override val clue: Track,
                         override val at: Point,
                         override val pair: Int)
          extends Data[Id], Upon[Track, Id]:

        override def apply(grid: Map[Point, Cell], over: Boolean*): Boolean =
          grid.contains(at) && grid(at).colors.contains(pair)


object Data:

  object http4s:

    import cats.effect.IO

    import io.circe.{ Decoder, Encoder }
    import io.circe.generic.auto.*
    import io.circe.generic.{ semiauto => sa }

    import org.http4s.circe.{ jsonEncoderOf, jsonOf }
    import org.http4s.{ EntityDecoder, EntityEncoder }

    import tense.extensional.Data.*
    import tense.intensional.Data.*

    import common.grid.http4s.given
    import grid.Grid.http4s.given
    import Clue.http4s.given
    import Cell.http4s.given

    given EntityDecoder[IO, Overlapping] = jsonOf
    given EntityDecoder[IO, DoubleCross] = jsonOf
    given EntityDecoder[IO, Trespassing] = jsonOf
    given EntityDecoder[IO, IntoThePit] = jsonOf
    given EntityDecoder[IO, OffCourse] = jsonOf
    given EntityDecoder[IO, Collision] = jsonOf

    given Decoder[Fact] = sa.deriveDecoder
    given EntityDecoder[IO, Fact] = jsonOf

    given EntityDecoder[IO, Backtrack] = jsonOf
    given EntityDecoder[IO, HalfCross] = jsonOf
    given EntityDecoder[IO, FullCross] = jsonOf
    given EntityDecoder[IO, Backlash] = jsonOf
    given EntityDecoder[IO, Pullout] = jsonOf

    given Decoder[Doubt] = sa.deriveDecoder
    given EntityDecoder[IO, Doubt] = jsonOf

    given EntityDecoder[IO, Data] = jsonOf

    given EntityEncoder[IO, Overlapping] = jsonEncoderOf
    given EntityEncoder[IO, DoubleCross] = jsonEncoderOf
    given EntityEncoder[IO, Trespassing] = jsonEncoderOf
    given EntityEncoder[IO, IntoThePit] = jsonEncoderOf
    given EntityEncoder[IO, OffCourse] = jsonEncoderOf
    given EntityEncoder[IO, Collision] = jsonEncoderOf

    given Encoder[Fact] = sa.deriveEncoder
    given EntityEncoder[IO, Fact] = jsonEncoderOf

    given EntityEncoder[IO, Backtrack] = jsonEncoderOf
    given EntityEncoder[IO, HalfCross] = jsonEncoderOf
    given EntityEncoder[IO, FullCross] = jsonEncoderOf
    given EntityEncoder[IO, Backlash] = jsonEncoderOf
    given EntityEncoder[IO, Pullout] = jsonEncoderOf

    given Encoder[Doubt] = sa.deriveEncoder
    given EntityEncoder[IO, Doubt] = jsonEncoderOf

    given EntityEncoder[IO, Data] = jsonEncoderOf
