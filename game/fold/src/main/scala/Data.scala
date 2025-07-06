package urru
package game
package fold

import io.github.greenleafoss.mongo.GreenLeafJsonProtocol
import spray.json.JsonFormat

import tense.extensional.Data.Fact
import tense.intensional.Data.Doubt


case class Data(fact: Fact, doubt: Doubt)
    extends urru.grid.Game.Data


package tense:

  import grid.Game.Data.*
  import Clue.*

  package extensional:

    sealed abstract trait Data[K <: Clue, F[_]]
        extends urru.grid.Game.Data:

      val clue: F[K]
      val colors: Seq[Int]


    object Data:

      import grid.shape

      case class Fact(some: List[Data[?, ?]], more: Fact*)


      type Id[T] = T
      type Op[T] = Option[T]

      case class Overlapping(override val clue: Option[Multi] = None,
                             override val colors: Int*)
          extends Data[Multi, Op]


      case class IntoThePit(override val clue: Empty,
                            override val colors: Int*)
        extends Data[Empty, Id]


  package intensional:

    sealed abstract trait Data[F[_]]
        extends urru.grid.Game.Data
        with grid.tense.intensional.Self[Clue, Cell, Move]:

      // the "pair" color
      val pair: F[Int]

      // the least applicable conditions
//      def apply(grid: Map[Point, Cell]): Boolean


    object Data:

      case class Doubt(data: Set[Data[?]])

////////////////////////////////////////////////////////////////////////////////

      /**
        * The move occurred without hindrance: you doubt that this move
        * should occur.
        */
      case class Backtrack(override val move: Move)
          extends Data[Op]:

        override val pair: Option[Int] = None

        // override def apply(_grid: Map[Point, Cell]): Boolean =
        //   val Move(odd, _, _, color) = move
        //   val i = -color-1
        //   this.over == over(2*i+odd)

////////////////////////////////////////////////////////////////////////////////

      /**
        * The move clashed with many a pair: you doubt that this move
        * should be in place instead.
        */
      case class Backlash(override val move: Move,
                          override val pair: Map[Int, Seq[Point]])
          extends Data[Mp]
