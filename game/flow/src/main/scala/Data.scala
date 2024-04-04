package urru
package game
package flow

import tense.extensional.Data.Fact
import tense.intensional.Data.Doubt


case class Data(fact: Fact, doubt: Doubt)
    extends grid.Game.Data


package tense:

  import common.grid.{ adj, row, x, col }

  import grid.Game.Data._
  import grid.shape

  import Clue._

  package extensional:

    sealed abstract trait Data[K <: Clue, F[_]]
        extends urru.grid.Game.Data, Upon[K, F]:

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
