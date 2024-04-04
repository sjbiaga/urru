package urru
package common


package object grid:

  type Point = (Int, Int)

  extension (row: Int)
    inline infix def x(col: Int): Point = (row, col)

  extension (self: (Int, Int))
    inline def row: Int = self._1
    inline def col: Int = self._2
    inline def unary_- : (Int, Int) = (-row, -col)
    inline def unary_! : Boolean = row + col == 0
    inline def +(that: (Int, Int)): (Int, Int) = (row + that.row, col + that.col)
    inline def -(that: (Int, Int)): (Int, Int) = (row - that.row, col - that.col)

  extension (self: Point)
    @inline def adj(that: Point) =
        (self.row == that.row || self.col == that.col) &&
        ((self.row == that.row) -> (math.abs(self.col - that.col) == 1)) &&
        ((self.col == that.col) -> (math.abs(self.row - that.row) == 1))
    @inline def adj: (Point, Point, Point, Point) =
      ((self.row-1) x self.col, (self.row+1) x self.col, self.row x (self.col-1), self.row x (self.col+1))
    inline def adj1: List[Point] = adj.toList
    @inline def adj2: Map[(Int, Int), Point] =
      Map(
        (-1, 0) -> adj._1,
        (1, 0) -> adj._2,
        (0, -1) -> adj._3,
        (0, 1) -> adj._4,
      )
