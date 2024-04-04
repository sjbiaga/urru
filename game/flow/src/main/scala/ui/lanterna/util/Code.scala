package urru
package game
package flow
package ui.lanterna
package util

import scala.annotation.tailrec
import scala.collection.mutable.{ ListBuffer => MutableList }

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.terminal.Terminal


case class Code(encoding: Code.Value,
                color: Int,
                minmax: (Option[Boolean], Option[Boolean]) = None->None):

  override def toString(): String = s"Code($encoding, $color)"


object Code extends Enumeration:

  lazy val colors = Map[Int, TextColor](
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
    -3 -> TextColor.ANSI.BLUE_BRIGHT,
    -4 -> TextColor.Factory.fromString("#006400"), // DarkGreen
    -5 -> TextColor.Factory.fromString("#dc143c"), // Crimson
    -6 -> TextColor.ANSI.BLUE,
    -7 -> TextColor.ANSI.YELLOW_BRIGHT,
    -8 -> TextColor.ANSI.MAGENTA,
    -9 -> TextColor.Factory.fromString("#808000"), // Olive
   -10 -> TextColor.Factory.fromString("#008080"), // Teal
   -11 -> TextColor.Factory.fromString("#9400d3"), // DarkViolet
   -12 -> TextColor.Factory.fromString("#8b0000"), // DarkRed
   -13 -> TextColor.Factory.fromString("#666699"),
//   -14 -> TextColor.Factory.fromString("#ba55d3"), // LimeGreen
   -14 -> TextColor.Factory.fromString("#a52a2a"), // Brown
  )

  lazy val LETTERS = Map[Int, Code.Value](
    -1 -> A_START,
    -2 -> B_START,
    -3 -> C_START,
    -4 -> D_START,
    -5 -> E_START,
    -6 -> F_START,
    -7 -> G_START,
    -8 -> H_START,
    -9 -> I_START,
   -10 -> J_START,
   -11 -> K_START,
   -12 -> L_START,
   -13 -> M_START,
   -14 -> N_START)

  lazy val letters = Map[Int, Code.Value](
    -1 -> a_START,
    -2 -> b_START,
    -3 -> c_START,
    -4 -> d_START,
    -5 -> e_START,
    -6 -> f_START,
    -7 -> g_START,
    -8 -> h_START,
    -9 -> i_START,
   -10 -> j_START,
   -11 -> k_START,
   -12 -> l_START,
   -13 -> m_START,
   -14 -> n_START)

  private lazy val ls_+ = X_CROSS :: (LETTERS.values.toList ++ letters.values.toList)

  lazy val axes = Map[Int, Code.Value](
     0 -> AXIS_0,
     1 -> AXIS_1,
     2 -> AXIS_2,
     3 -> AXIS_3,
     4 -> AXIS_4,
     5 -> AXIS_5,
     6 -> AXIS_6,
     7 -> AXIS_7,
     8 -> AXIS_8,
     9 -> AXIS_9)

  import urru.common.grid.{ row, x, col }

  import Clue._

  def apply(game: Game): List[List[Code]] =
    val size = game.size

    val codes: MutableList[MutableList[Seq[Code]]] = MutableList.fill((size.row+1) * 3)
                                                                     (MutableList.fill((size.col+1) * 3)
                                                                                      (Nil))

    game.clues_hints.foreach {
      case Start(at @ (row, col), _, c) =>
        val odd = game.state.map(_.play).indexWhere(_.head == at) % 2
        codes(row*3+1)(col*3+1) = Seq(Code(if game.pending.isEmpty || odd == 0 then LETTERS(c) else letters(c), c))

      case Cross((row, col)) =>
        codes(row*3+1)(col*3+1) = Seq(Code(X_CROSS, 1))

      case Strip((row, col), _) if row < 1 || row > size.row || col < 1 || col > size.col =>
      case Strip(_, (row, col)) if row < 1 || row > size.row || col < 1 || col > size.col =>

      case Strip((row1, col1), (row, col2)) if row1 == row =>
        var col = col1 min col2
        var j = col*3+2
        for
          i <- 0 until 3
        do
          codes(row*3+i)(j) :+= Code(VERTICAL_STRIP, 1, minmax = (None, Some(false)))
        col = col1 max col2
        j = col*3+0
        for
          i <- 0 until 3
        do
          codes(row*3+i)(j) :+= Code(VERTICAL_STRIP, 1, minmax = (None, Some(true)))

      case Strip((row1, col1), (row2, col)) if col1 == col =>
        var row = row1 min row2
        var i = row*3+2
        for
          j <- 0 until 3
        do
          codes(i)(col*3+j) :+= Code(HORIZONTAL_STRIP, 1, minmax = (Some(false), None))
        row = row1 max row2
        i = row*3+0
        for
          j <- 0 until 3
        do
          codes(i)(col*3+j) :+= Code(HORIZONTAL_STRIP, 1, minmax = (Some(true), None))

      case _ =>
    }

    // closed ends /////////////////////////////////////////////////////////////
    game.clues_hints.foreach {
      case Strip((row1, col1), (row, col2))
          if row1 == row
          && 1 <= col1 && col1 <= size.col
          && 1 <= col2 && col2 <= size.col
          && codes(3*row+2)(3*(col1 min col2)+2).size == 1
          && codes(3*row+2)(3*(col1 max col2)+0).size == 1
          && {
            val row2 = row + 1
            !game.clues_hints.exists {
              case Strip((`row2`, `col1`), (`row2`, `col2`)) | Strip((`row2`, `col2`), (`row2`, `col1`)) => true
              case _ => false
            }
          } =>
        codes(3*row+2)(3*(col1 min col2)+2) = Seq(Code(BOTTOM_LEFT_STRIP, 1))
        codes(3*row+2)(3*(col1 max col2)+0) = Seq(Code(BOTTOM_RIGHT_STRIP, 1))

      case Strip((row1, col1), (row, col2))
          if row1 == row
          && 1 <= col1 && col1 <= size.col
          && 1 <= col2 && col2 <= size.col
          && codes(3*row+0)(3*(col1 min col2)+2).size == 1
          && codes(3*row+0)(3*(col1 max col2)+0).size == 1
          && {
            val row2 = row - 1
            !game.clues_hints.exists {
              case Strip((`row2`, `col1`), (`row2`, `col2`)) | Strip((`row2`, `col2`), (`row2`, `col1`)) => true
              case _ => false
            }
          } =>
        codes(3*row+0)(3*(col1 min col2)+2) = Seq(Code(TOP_LEFT_STRIP, 1))
        codes(3*row+0)(3*(col1 max col2)+0) = Seq(Code(TOP_RIGHT_STRIP, 1))

      case Strip((row1, col1), (row2, col))
          if col1 == col
          && 1 <= row1 && row1 <= size.row
          && 1 <= row2 && row2 <= size.row
          && codes(3*(row1 min row2)+2)(3*col+2).size == 1
          && codes(3*(row1 max row2)+0)(3*col+2).size == 1
          && {
            val col2 = col + 1
            !game.clues_hints.exists {
              case Strip((`row1`, `col2`), (`row2`, `col2`)) | Strip((`row2`, `col2`), (`row1`, `col2`)) => true
              case _ => false
            }
          } =>
        codes(3*(row1 min row2)+2)(3*col+2) = Seq(Code(TOP_RIGHT_STRIP, 1))
        codes(3*(row1 max row2)+0)(3*col+2) = Seq(Code(BOTTOM_RIGHT_STRIP, 1))

      case Strip((row1, col1), (row2, col))
          if col1 == col
          && 1 <= row1 && row1 <= size.row
          && 1 <= row2 && row2 <= size.row
          && codes(3*(row1 min row2)+2)(3*col+0).size == 1
          && codes(3*(row1 max row2)+0)(3*col+0).size == 1
          && {
            val col2 = col - 1
            !game.clues_hints.exists {
              case Strip((`row1`, `col2`), (`row2`, `col2`)) | Strip((`row2`, `col2`), (`row1`, `col2`)) => true
              case _ => false
            }
          } =>
        codes(3*(row1 min row2)+2)(3*col+0) = Seq(Code(TOP_LEFT_STRIP, 1))
        codes(3*(row1 max row2)+0)(3*col+0) = Seq(Code(BOTTOM_LEFT_STRIP, 1))

      case _ =>
    }

    // corners & T-shapes (including crosses) //////////////////////////////////
    for
      row <- 1 to size.row
      col <- 1 to size.col
    do
      for
        i <- 0 until 3
        j <- 0 until 3
        clue = codes(row*3+i)(col*3+j)
      do
        clue.size match
          case 0 =>
          case 1 =>
          case 2 =>
            var k = -1
            var l = -1
            if (clue(0).encoding eq HORIZONTAL_STRIP) && (clue(1).encoding eq VERTICAL_STRIP)
            then
              k = 0
              l = 1

            if (clue(0).encoding eq VERTICAL_STRIP) && (clue(1).encoding eq HORIZONTAL_STRIP)
            then
              k = 1
              l = 0

            if k < 0 || l < 0
            then
              ???
            else
              (clue(k).minmax._1.get, clue(l).minmax._2.get) match
                case (false, false) =>
                  codes(row*3+i)(col*3+j) = Seq(Code(BOTTOM_RIGHT_STRIP, 1))
                  if codes(row*3+i+1)(col*3+j+1).isEmpty
                  then
                    codes(row*3+i+1)(col*3+j+1) = Seq(Code(BOTTOM_RIGHT_STRIP, 1))
                case (false, true) =>
                  codes(row*3+i)(col*3+j) = Seq(Code(BOTTOM_LEFT_STRIP, 1))
                  if codes(row*3+i+1)(col*3+j-1).isEmpty
                  then
                    codes(row*3+i+1)(col*3+j-1) = Seq(Code(BOTTOM_LEFT_STRIP, 1))
                case (true, false) =>
                  codes(row*3+i)(col*3+j) = Seq(Code(TOP_RIGHT_STRIP, 1))
                  if codes(row*3+i-1)(col*3+j+1).isEmpty
                  then
                    codes(row*3+i-1)(col*3+j+1) = Seq(Code(TOP_RIGHT_STRIP, 1))
                case (true, true) =>
                  codes(row*3+i)(col*3+j) = Seq(Code(TOP_LEFT_STRIP, 1))
                  if codes(row*3+i-1)(col*3+j-1).isEmpty
                  then
                    codes(row*3+i-1)(col*3+j-1) = Seq(Code(TOP_LEFT_STRIP, 1))
          case _ => ???

    codes.map(_.map(_.headOption.getOrElse(null))).map(_.toList).toList

////////////////////////////////////////////////////////////////////////////////

  def apply(game: Game, clues: List[List[Code]], just: List[List[Code]]): List[List[Code]] =
    val size = game.size

    val codes: MutableList[MutableList[Code]] = MutableList.fill((size.row+1) * 3 + 1)
                                                                (MutableList.fill((size.col+1) * 3 + 1)
                                                                                 (null))

    // unused
    for
      k <- 0 to 1
    do
      codes((size.row+1)*3)(k) = Code(BLANK, 0)
      codes(k)((size.col+1)*3) = Code(BLANK, 0)

    // axes & border ///////////////////////////////////////////////////////////

    for
      row <- 1 to size.row
    do
      if game.showAxes
      then
        if row > 9
        then
          codes(row*3+1)(0) = Code(axes(row/10), 1)
        codes(row*3+1)(1) = Code(axes(row%10), 1)
      for
        i <- 0 until 3
      do
        codes(row*3+i)(2) = Code(VERTICAL_STRIP, 1)
        codes(row*3+i)((size.col+1)*3) = Code(VERTICAL_STRIP, 1)

    for
      col <- 1 to size.col
    do
      if game.showAxes
      then
        if col > 9
        then
          codes(1)(col*3) = Code(axes(col/10), 1)
        codes(1)(col*3+1) = Code(axes(col%10), 1)
      for
        j <- 0 until 3
      do
        codes(2)(col*3+j) = Code(HORIZONTAL_STRIP, 1)
        codes((size.row+1)*3)(col*3+j) = Code(HORIZONTAL_STRIP, 1)

    codes(2)(2) = Code(TOP_LEFT_STRIP, 1)
    codes((size.row+1)*3)(2) = Code(BOTTOM_LEFT_STRIP, 1)
    codes(2)((size.col+1)*3) = Code(TOP_RIGHT_STRIP, 1)
    codes((size.row+1)*3)((size.col+1)*3) = Code(BOTTOM_RIGHT_STRIP, 1)

    // strips at border ////////////////////////////////////////////////////////

    for
      row <- 1 until size.row
      row1 = row + 1
      max = size.col
    do
      game.clues_hints.foreach {
        case Strip((`row`, 1), (`row1`, 1)) | Strip((`row1`, 1), (`row`, 1)) =>
          codes(row*3+2)(2) = Code(LEFT_GRID, 0)
          codes(row*3+2)(3) = Code(HORIZONTAL_STRIP, 0)
          codes(row1*3)(2) = Code(LEFT_GRID, 0)
          codes(row1*3)(3) = Code(HORIZONTAL_STRIP, 0)
        case Strip((`row`, `max`), (`row1`, `max`)) | Strip((`row1`, `max`), (`row`, `max`)) =>
          codes(row*3+2)((max+1)*3) = Code(RIGHT_GRID, 0)
          codes(row*3+2)((max+1)*3-1) = Code(HORIZONTAL_STRIP, 0)
          codes(row1*3)((max+1)*3) = Code(RIGHT_GRID, 0)
          codes(row1*3)((max+1)*3-1) = Code(HORIZONTAL_STRIP, 0)
        case _ =>
      }
    for
      col <- 1 until size.col
      col1 = col + 1
      max = size.row
    do
      game.clues_hints.foreach {
        case Strip((1, `col`), (1, `col1`)) | Strip((1, `col1`), (1, `col`)) =>
          codes(2)(col*3+2) = Code(TOP_GRID, 0)
          codes(3)(col*3+2) = Code(VERTICAL_STRIP, 0)
          codes(2)(col1*3) = Code(TOP_GRID, 0)
          codes(3)(col1*3) = Code(VERTICAL_STRIP, 0)
        case Strip((`max`, `col`), (`max`, `col1`)) | Strip((`max`, `col1`), (`max`, `col`)) =>
          codes((max+1)*3)(col*3+2) = Code(BOTTOM_GRID, 0)
          codes((max+1)*3-1)(col*3+2) = Code(VERTICAL_STRIP, 0)
          codes((max+1)*3)(col1*3) = Code(BOTTOM_GRID, 0)
          codes((max+1)*3-1)(col1*3) = Code(VERTICAL_STRIP, 0)
        case _ =>
      }

    // lines ///////////////////////////////////////////////////////////////////

    def join(at: Point, by: Point, color: Int, horz: Boolean, vert: Boolean, dir: Boolean): Unit =
      if at.row == by.row // horizontal
      then
        if at.col < by.col
        then // right
          if horz
          then // collinear
            for j <- 1 to 2 do
              codes(at.row*3+1)(at.col*3+j) = Code(HORIZONTAL_LINE, color)
            codes(by.row*3+1)(by.col*3+0) = Code(HORIZONTAL_LINE, color)
          else
            //                                               down-right            up-right
            codes(at.row*3+1)(at.col*3+1) = Code(if dir then BOTTOM_LEFT_LINE else TOP_LEFT_LINE, color)
            codes(at.row*3+1)(at.col*3+2) = Code(HORIZONTAL_LINE, color)
            codes(by.row*3+1)(by.col*3+0) = Code(HORIZONTAL_LINE, color)
        else // left
          if horz
          then // collinear
            for j <- 0 to 1 do
              codes(at.row*3+1)(at.col*3+j) = Code(HORIZONTAL_LINE, color)
            codes(by.row*3+1)(by.col*3+2) = Code(HORIZONTAL_LINE, color)
          else
            //                                               down-left              up-left
            codes(at.row*3+1)(at.col*3+1) = Code(if dir then BOTTOM_RIGHT_LINE else TOP_RIGHT_LINE, color)
            codes(at.row*3+1)(at.col*3+0) = Code(HORIZONTAL_LINE, color)
            codes(by.row*3+1)(by.col*3+2) = Code(HORIZONTAL_LINE, color)

      else if at.col == by.col // vertical
      then
        if at.row < by.row
        then
          if vert
          then // collinear
            for i <- 1 to 2 do
              codes(at.row*3+i)(at.col*3+1) = Code(VERTICAL_LINE, color)
            codes(by.row*3+0)(by.col*3+1) = Code(VERTICAL_LINE, color)
          else
            //                                               right-down          left-down
            codes(at.row*3+1)(at.col*3+1) = Code(if dir then TOP_RIGHT_LINE else TOP_LEFT_LINE, color)
            codes(at.row*3+2)(at.col*3+1) = Code(VERTICAL_LINE, color)
            codes(by.row*3+0)(by.col*3+1) = Code(VERTICAL_LINE, color)
        else
          if vert
          then // collinear
            for i <- 0 to 1 do
              codes(at.row*3+i)(at.col*3+1) = Code(VERTICAL_LINE, color)
            codes(by.row*3+2)(by.col*3+1) = Code(VERTICAL_LINE, color)
          else
            //                                               right-up               left-up
            codes(at.row*3+1)(at.col*3+1) = Code(if dir then BOTTOM_RIGHT_LINE else BOTTOM_LEFT_LINE, color)
            codes(at.row*3+0)(at.col*3+1) = Code(VERTICAL_LINE, color)
            codes(by.row*3+2)(by.col*3+1) = Code(VERTICAL_LINE, color)

      else
        ???

    @tailrec
    def draw(at: Point, ls: Play, color: Int, horz: Boolean, vert: Boolean, dir: Boolean): Unit =
      if ls.isEmpty
      then
        codes(at.row*3+1)(at.col*3+1) = Code(MARK, color)
      else
        val by = ls.head
        join(at, by, color, horz, vert, dir)
        draw(by, ls.tail, color,
             horz = at.row == by.row, vert = at.col == by.col,
             dir = if at.row == by.row then at.col < by.col else at.row < by.row)

    val play = game.state.map(_.play)

    for
      (ls, i) <- play.zipWithIndex
      c = i / 2
      at = ls.head
    do
      ls.tail.headOption.foreach { by =>
        draw(at, ls.tail, -c-1,
             horz = at.row == by.row, vert = at.col == by.col,
             dir = if at.row == by.row then at.col < by.col else at.row < by.row)
      }

    for
      ((ls1, ls2), c) <- Seq.from(play.sliding(2, 2)).map { it => it.head -> it.last }.zipWithIndex
      if ls1.size > 1 && ls2.size > 1 && ls1.last == ls2.last
      it = ls1.last
      pt1 = ls1.init.last
      pt2 = ls2.init.last
    do
      join(it, pt2, -c-1,
           horz = pt1.row == it.row, vert = pt1.col == it.col,
           dir = if pt1.row == it.row then pt1.col < it.col else pt1.row < it.row)

    // clues ///////////////////////////////////////////////////////////////////

    for
      row <- 0 to size.row
      col <- 0 to size.col
    do
      for
        i <- 0 until 3
        j <- 0 until 3
        if clues(row*3+i)(col*3+j) ne null   // lines would overwrite letters (ls_+), so do not
        if (codes(row*3+i)(col*3+j) eq null) || ls_+.contains(clues(row*3+i)(col*3+j).encoding)
      do
        codes(row*3+i)(col*3+j) = clues(row*3+i)(col*3+j)

    for
      row <- 0 to size.row
      col <- 0 to size.col
    do
      for
        i <- 0 until 3
        j <- 0 until 3
        if codes(row*3+i)(col*3+j) eq null
      do
        codes(row*3+i)(col*3+j) = Code(BLANK, 0)

    val r = codes.map(_.toList).toList

    if game.showJust.isEmpty
    then
      r
    else
      r.map { it => it ++ it }

////////////////////////////////////////////////////////////////////////////////

  def apply(size: Point, codes: List[List[Code]]): List[Terminal => Unit] =

//    val (m, n) = (size.row+1)*3 -> (size.col+1)*3
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

          if true
          then

            var i = 0

            def last =
              if i < s.size
              then
                val s1 = s.substring(i, i+1).charAt(0)
                r += { it =>
                  it.setBackgroundColor(colors(c))
                  it.setForegroundColor(colors(1))
                  it.putCharacter(s1)
                  it.setBackgroundColor(colors(0))
                }

                i += 1

            if j > 0 && (codes(y)(j-1).encoding eq BLANK)
            then
              r += { it =>
                it.setCursorPosition(j-1, y)
                it.putCharacter(' ')
              }
            else
              r += { it =>
                it.setCursorPosition(j, y)
              }

            s.split("[A-ZⒶ-Ⓩ]").foreach {
              case ss if ss.nonEmpty =>

                r += { it =>
                  it.setForegroundColor(colors(c))
                  it.putString(ss)
                }

                i += ss.size

                last
 
              case _ =>

                last
            }

            last

          else

            r += { it =>
              it.setCursorPosition(j, y)
              it.setForegroundColor(colors(c))
              it.putString(s)
            }

    r.toList

////////////////////////////////////////////////////////////////////////////////

  val AXIS_0 = Value("0")
  val AXIS_1 = Value("1")
  val AXIS_2 = Value("2")
  val AXIS_3 = Value("3")
  val AXIS_4 = Value("4")
  val AXIS_5 = Value("5")
  val AXIS_6 = Value("6")
  val AXIS_7 = Value("7")
  val AXIS_8 = Value("8")
  val AXIS_9 = Value("9")

  val A_START = Value("A")
  val a_START = Value("Ⓐ")
  val B_START = Value("B")
  val b_START = Value("Ⓑ")
  val C_START = Value("C")
  val c_START = Value("Ⓒ")
  val D_START = Value("D")
  val d_START = Value("Ⓓ")
  val E_START = Value("E")
  val e_START = Value("Ⓔ")
  val F_START = Value("F")
  val f_START = Value("Ⓕ")
  val G_START = Value("G")
  val g_START = Value("Ⓖ")
  val H_START = Value("H")
  val h_START = Value("Ⓗ")
  val I_START = Value("I")
  val i_START = Value("Ⓘ")
  val J_START = Value("J")
  val j_START = Value("Ⓙ")
  val K_START = Value("K")
  val k_START = Value("Ⓚ")
  val L_START = Value("L")
  val l_START = Value("Ⓛ")
  val M_START = Value("M")
  val m_START = Value("Ⓜ")
  val N_START = Value("N")
  val n_START = Value("Ⓝ")

// Ⓞ Ⓟ  Ⓠ  Ⓡ  Ⓢ  Ⓣ  Ⓤ  Ⓥ  Ⓦ  Ⓧ  Ⓨ  Ⓩ

  val BLANK = Value(" ")

  val MARK = Value("●")

  val X_CROSS = Value("╬")

  val HORIZONTAL_STRIP = Value("─")
  val VERTICAL_STRIP = Value("│")

  val TOP_LEFT_STRIP = Value("┌")
  val TOP_RIGHT_STRIP = Value("┐")
  val BOTTOM_LEFT_STRIP = Value("└")
  val BOTTOM_RIGHT_STRIP = Value("┘")

  val CROSS_GRID = Value("┼")

  val TOP_GRID = Value("┬")
  val BOTTOM_GRID = Value("┴")
  val LEFT_GRID = Value("├")
  val RIGHT_GRID = Value("┤")

  val HORIZONTAL_LINE = Value("━")
  val VERTICAL_LINE = Value("┃")

  val TOP_LEFT_LINE = Value("┏")
  val TOP_RIGHT_LINE = Value("┓")
  val BOTTOM_LEFT_LINE = Value("┗")
  val BOTTOM_RIGHT_LINE = Value("┛")
