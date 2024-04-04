package urru
package grid

import scala.collection.mutable.{ HashMap, HashSet }

import grid.Game
import Game._
import grid.Grid
import Grid._


sealed abstract trait Tense[C, V, M, R, F[_], W[_, _]]
    extends Grid[C, Point, V, F, W]
    with GridOps[M, R]


object Tense:

  type Intensional[T] = T
  type Extensional[T] = HashSet[T]


  abstract trait Have[C, V, M]
      extends tense.extensional.Grid[C, V, M]
      with Tense[C, V, M, Unit, Extensional, HashMap]


  abstract trait Just[
    D,
    C <: Clue,
    V <: Cell,
    M <: Move[V, C]
  ] extends tense.intensional.Grid[D, C, V, M, Long, Map]
      with Tense[C, V, M, D => Unit, Intensional, Map]
      with tense.intensional.Self[C, V, M]


package tense:

  package object extensional:

    abstract trait Grid[C, V, M]
        extends urru.grid.Grid[C, Point, V, Tense.Extensional, HashMap]
        with GridOps[M, Unit]:

      val size: Point


  package object intensional:

    abstract trait Grid[D, C, V, M, P, W[_, _]]
        extends urru.grid.Grid[C, Point, V, Tense.Intensional, W]
        with GridOps[M, D => Unit]
        with PathOps[P]

////////////////////////////////////////////////////////////////////////////////

    abstract trait Self[C <: Clue, V <: Cell, M <: Move[V, C]]:

      val move: M
