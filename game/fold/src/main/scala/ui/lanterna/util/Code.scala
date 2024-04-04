package urru
package game
package fold
package ui.lanterna
package util

import scala.annotation.tailrec
import scala.collection.mutable.{ ListBuffer => MutableList }

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.terminal.Terminal


case class Code(encoding: Code.Value, color: Int, minmax: (Option[Boolean], Option[Boolean]) = None->None):

  override def toString(): String = s"Code($encoding, $color)"


object Code extends Enumeration:

  lazy val colors = Map[Int, TextColor](
   //   1 -> TextColor.ANSI.WHITE,
   //   0 -> TextColor.ANSI.DEFAULT,
   //  -1 -> TextColor.ANSI.YELLOW_BRIGHT,
   //  -2 -> TextColor.ANSI.BLUE_BRIGHT,
   //  -3 -> TextColor.ANSI.GREEN_BRIGHT,
   //  -4 -> TextColor.ANSI.MAGENTA_BRIGHT,
   //  -5 -> TextColor.ANSI.CYAN_BRIGHT,
   //  -6 -> TextColor.ANSI.RED_BRIGHT,
   //  -7 -> TextColor.ANSI.BLUE,
   //  -8 -> TextColor.ANSI.MAGENTA,
   //  -9 -> TextColor.ANSI.CYAN,
   // -10 -> TextColor.ANSI.GREEN,
   // -11 -> TextColor.ANSI.YELLOW,
   // -12 -> TextColor.ANSI.RED,
   // -13 -> TextColor.ANSI.WHITE_BRIGHT,
   //   1 -> TextColor.ANSI.WHITE,
   //   0 -> TextColor.ANSI.DEFAULT,
   //  -1 -> TextColor.ANSI.YELLOW_BRIGHT,
   //  -1 -> TextColor.Factory.fromString("#4b0082"), // Indigo
   //  -2 -> TextColor.ANSI.BLUE_BRIGHT,
   //  -3 -> TextColor.ANSI.GREEN_BRIGHT,
   //  -3 -> TextColor.Factory.fromString("#ff7f50"), // Coral
   //  -3 -> TextColor.Factory.fromString("#d2691e"), // Chocolate
   //  -4 -> TextColor.ANSI.MAGENTA_BRIGHT,
   //  -4 -> TextColor.Factory.fromString("#9400d3"), // DarkViolet
   //  -5 -> TextColor.ANSI.CYAN_BRIGHT,
   //  -5 -> TextColor.Factory.fromString("#636300"),
   //  -6 -> TextColor.ANSI.RED_BRIGHT,
   //  -6 -> TextColor.Factory.fromString("#a52a2a"), // Brown
   //  -7 -> TextColor.ANSI.BLUE,
   //  -8 -> TextColor.ANSI.MAGENTA,
   //  -9 -> TextColor.ANSI.CYAN,
   //  -9 -> TextColor.Factory.fromString("#008b8b"), // DarkCyan
   //  -9 -> TextColor.Factory.fromString("#008080"), // Teal
   // -10 -> TextColor.ANSI.GREEN,
   // -10 -> TextColor.Factory.fromString("#006400"), // DarkGreen
   // -11 -> TextColor.ANSI.YELLOW,
   // //-11 -> TextColor.Factory.fromString("#f5fffa"), // MintCream
   // -11 -> TextColor.Factory.fromString("#666699"),
   // -12 -> TextColor.ANSI.RED,
   // -12 -> TextColor.Factory.fromString("#8b0000"), // DarkRed
   // -13 -> TextColor.ANSI.WHITE_BRIGHT,
   // -13 -> TextColor.Factory.fromString("#808000"), // Olive
     1 -> TextColor.ANSI.WHITE,
     0 -> TextColor.ANSI.DEFAULT,
    -1 -> TextColor.Factory.fromString("#4b0082"), // Indigo
    -2 -> TextColor.Factory.fromString("#d2691e"), // Chocolate
    -3 -> TextColor.Factory.fromString("#006400"), // DarkGreen
    -4 -> TextColor.Factory.fromString("#dc143c"), // Crimson
    -5 -> TextColor.ANSI.BLUE,
    -6 -> TextColor.ANSI.YELLOW_BRIGHT,
    -7 -> TextColor.ANSI.MAGENTA,
    -8 -> TextColor.Factory.fromString("#808000"), // Olive
    -9 -> TextColor.Factory.fromString("#008080"), // Teal
   -10 -> TextColor.Factory.fromString("#9400d3"), // DarkViolet
   -11 -> TextColor.Factory.fromString("#8b0000"), // DarkRed
   -12 -> TextColor.Factory.fromString("#666699"),
   -13 -> TextColor.Factory.fromString("#a52a2a"), // Brown
  )

  import urru.common.grid.{ row, col }

  import Clue._

  def apply(game: Game): List[List[Code]] =
    val size = game.size

    val codes: MutableList[MutableList[Code]] = MutableList.fill((size.row+1)*2 + 1)
                                                                (MutableList.fill((size.col+1)*2 + 1)
                                                                                 (null))
    for
      row <- 1 to size.row
      col <- 1 to size.col
    do
      codes(row*2+1)(col*2+1) = Code(FREE, 1)

    game.clues_hints.foreach {
      case _: Block =>

      case Empty((row, col)) =>
        codes(row*2+1)(col*2+1) = Code(BLANK, 1)

      case Multi((row, col)) =>
        codes(row*2+1)(col*2+1) = Code(MULTI, 1)
    }

    codes.map(_.toList).toList

////////////////////////////////////////////////////////////////////////////////

  def apply(game: Game, clues: List[List[Code]], just: List[List[Code]]): List[List[Code]] =
    val size = game.size

    val codes: MutableList[MutableList[Code]] = MutableList.fill((size.row+1)*2 + 1)
                                                                (MutableList.fill((size.col+1)*2 + 1)
                                                                                 (null))

    // grid ////////////////////////////////////////////////////////////////////

    if game.showAxes
    then
      for
        row <- 1 to size.row
        col <- 1 to size.col
      do
        codes(row*2)(col*2) = Code(CROSS_GRID, 1)
        codes(row*2)(col*2+1) = Code(HORIZONTAL_GRID, 1)
        codes(row*2+1)(col*2) = Code(VERTICAL_GRID, 1)

      for
        col <- 1 to size.col
      do
        codes(2)(col*2) = Code(TOP_GRID, 1)
        codes((size.row+1)*2)(col*2) = Code(BOTTOM_GRID, 1)
        codes((size.row+1)*2)(col*2+1) = Code(HORIZONTAL_GRID, 1)

      for
        row <- 1 to size.row
      do
        codes(row*2)(2) = Code(LEFT_GRID, 1)
        codes(row*2)((size.col+1)*2) = Code(RIGHT_GRID, 1)
        codes(row*2+1)((size.col+1)*2) = Code(VERTICAL_GRID, 1)

      codes(2)(2) = Code(TOP_LEFT_GRID, 1)
      codes(2)((size.col+1)*2) = Code(TOP_RIGHT_GRID, 1)
      codes((size.row+1)*2)(2) = Code(BOTTOM_LEFT_GRID, 1)
      codes((size.row+1)*2)((size.col+1)*2) = Code(BOTTOM_RIGHT_GRID, 1)

    else
      for
        row <- 1 to size.row
        col <- 1 to size.col
        i <- 0 until 2
        j <- 0 until 2
      do
        codes(row*2+i)(col*2+j) = Code(BLANK, 0)

      for
        col <- 1 to size.col
        j <- 0 until 2
      do
        codes(2)(col*2+j) = Code(HORIZONTAL_GRID, 0)
        codes((size.row+1)*2)(col*2+j) = Code(HORIZONTAL_GRID, 0)

      for
        row <- 1 to size.row
        i <- 0 until 2
      do
        codes(row*2+i)(2) = Code(VERTICAL_GRID, 0)
        codes(row*2+i)((size.col+1)*2) = Code(VERTICAL_GRID, 0)

      codes(2)(2) = Code(TOP_LEFT_GRID, 1)
      codes(2)((size.col+1)*2) = Code(TOP_RIGHT_GRID, 1)
      codes((size.row+1)*2)(2) = Code(BOTTOM_LEFT_GRID, 1)
      codes((size.row+1)*2)((size.col+1)*2) = Code(BOTTOM_RIGHT_GRID, 1)

    // clues ///////////////////////////////////////////////////////////////////

    for
      row <- 0 to size.row
      col <- 0 to size.col
    do
      for
        i <- 0 until 2
        j <- 0 until 2
        if clues(row*2+i)(col*2+j) ne null
      do
        codes(row*2+i)(col*2+j) = clues(row*2+i)(col*2+j)

    for
      row <- 0 to size.row
      i <- 0 until 2
      if clues(row*2+i)((size.col+1)*2) ne null
    do
      codes(row*2+i)((size.col+1)*2) = clues(row*2+i)((size.col+1)*2)

    for
      col <- 0 to size.col
      j <- 0 until 2
      if clues((size.row+1)*2)(col*2+j) ne null
    do
      codes((size.row+1)*2)(col*2+j) = clues((size.row+1)*2)(col*2+j)

    for
      row <- 0 to size.row
      col <- 0 to size.col
    do
      for
        i <- 0 until 2
        j <- 0 until 2
        if codes(row*2+i)(col*2+j) eq null
      do
        codes(row*2+i)(col*2+j) = Code(BLANK, 0)

    for
      row <- 0 to size.row
      i <- 0 until 2
      if codes(row*2+i)((size.col+1)*2) eq null
    do
      codes(row*2+i)((size.col+1)*2) = Code(BLANK, 0)

    for
      col <- 0 to size.col
      j <- 0 until 2
      if codes((size.row+1)*2)(col*2+j) eq null
    do
      codes((size.row+1)*2)(col*2+j) = Code(BLANK, 0)

    // blocks //////////////////////////////////////////////////////////////////

    game.state
      .map(_.play.head)
      .foreach { case Block(_, _, color, block*) =>
        block.foreach { (row, col) =>
          codes(row*2+1)(col*2+1) = Code(BLOCK, color)
        }
      }

    for
      row <- 0 to size.row
      col <- 0 to size.col
    do
      for
        i <- 0 until 2
        j <- 0 until 2
        clue = clues(row*2+i)(col*2+j)
        if (clue ne null) && (clue.encoding eq MULTI)
      do
        codes(row*2+i)(col*2+j) = clue

    val r = codes.map(_.toList).toList

    if game.showJust.isEmpty
    then
      r
    else
      r.map { it => it ++ it }

////////////////////////////////////////////////////////////////////////////////

  def apply(size: Point, codes: List[List[Code]]): List[Terminal => Unit] =

//    val (m, n) = (size.row+1)*2 -> (size.col+1)*2
    val (m, n) = (codes.size, codes.head.size)

    val r = MutableList[Terminal => Unit]()

    for
      y <- 0 until m
    do
      var x = 0

      while x < n
      do
        while x < n && (codes(y)(x).encoding eq BLANK)
        do
          x += 1

        if x < n
        then

          val j = x
          val c = codes(y)(j).color

          while x < n && codes(y)(x).color == c && (codes(y)(x).encoding ne BLANK)
          do
            x += 1

          val s = codes(y).drop(j).take(x-j).map(_.encoding).mkString("")

          r += { it =>
            it.setCursorPosition(j, y)
            it.setForegroundColor(colors(c))
            it.putString(s)
          }

    r.toList

////////////////////////////////////////////////////////////////////////////////

  val BLANK = Value(" ")

  val BLOCK = Value("█")

  val MULTI = Value("◌")

  val FREE = Value("░")
//  val FREE = Value("▒")

  val HORIZONTAL_GRID = Value("─")
  val VERTICAL_GRID = Value("│")

  val CROSS_GRID = Value("┼")

  val TOP_LEFT_GRID = Value("┌")
  val TOP_RIGHT_GRID = Value("┐")
  val BOTTOM_LEFT_GRID = Value("└")
  val BOTTOM_RIGHT_GRID = Value("┘")

  val TOP_GRID = Value("┬")
  val BOTTOM_GRID = Value("┴")
  val LEFT_GRID = Value("├")
  val RIGHT_GRID = Value("┤")
