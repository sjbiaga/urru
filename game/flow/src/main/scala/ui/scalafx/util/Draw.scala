package urru
package game
package flow
package ui.scalafx
package util

import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import Color._
import javafx.scene.canvas.GraphicsContext


object Draw:

  lazy val colors = Map[Int, Color](
     2 -> Cyan,
     1 -> Grey,
     0 -> Black,
    -1 -> Indigo,
    -2 -> Chocolate,
    -3 -> DarkGreen,
    -4 -> Crimson,
    -5 -> Blue,
    -6 -> Yellow,
    -7 -> Magenta,
    -8 -> Olive,
    -9 -> Teal,
   -10 -> DarkViolet,
   -11 -> DarkRed,
   -13 -> Color.web("#666699"),
   -14 -> Brown,
   -15 -> LimeGreen,
  )

  import urru.common.grid.{ row, x, col, + }

  import Clue._

  // board /////////////////////////////////////////////////////////////////////

  extension(self: Canvas)

    def redraw(game: Game): Unit =
      val size = game.size

      val gc = self.getGraphicsContext2D()

      // grid //////////////////////////////////////////////////////////////////

      gc.setFill(colors(0))
      gc.setStroke(colors(1))
      gc.fillRect(0, 0, self.width(), self.height())

      gc.setLineWidth(dim.grid)
      if game.showAxes
      then
        for
          col <- 0 to size.col
        do
          gc.strokeLine(col * dim.cell, 0,
                        col * dim.cell, size.row * dim.cell)
        for
          row <- 0 to size.row
        do
          gc.strokeLine(0,                  row * dim.cell,
                        size.col * dim.cell, row * dim.cell)
      else
        for
          col <- List(0, size.col)
        do
          gc.strokeLine(col * dim.cell, 0,
                        col * dim.cell, size.row * dim.cell)
        for
          row <- List(0, size.row)
        do
          gc.strokeLine(0,                  row * dim.cell,
                        size.col * dim.cell, row * dim.cell)

      // lines /////////////////////////////////////////////////////////////////

      gc.setLineWidth(dim.line)
      for
        i <- 0 until game.state.size
        color = -i/2-1
        ls = game.state(i).play
      do
        gc.setStroke(colors(color))
        var from = ls(0)
        for
          j <- 1 until ls.size
        do
          val to = ls(j)
          draw(gc, from + (-1, -1), to + (-1, -1), color, j == 1, j == ls.size - 1, game.state(i).over)
          if j > 1
          then
            val to = ls(j-2)
            draw(gc, to + (-1, -1), from + (-1, -1), color, j == 2, false, false)
          from = to

      // clues /////////////////////////////////////////////////////////////////

      game.clues_hints.foreach {
        case Start(at, _, c) =>
          draw(gc, at + (-1, -1), c)

        case Cross((row, col)) =>
          gc.setStroke(colors(1))
          gc.setLineWidth(dim.cross)
          // top-left
          gc.strokeLine((col-1) * dim.cell + dim.mid - dim.line,            (row-1) * dim.cell + dim.mid - dim.line,
                        (col-1) * dim.cell + dim.mid - dim.line,            (row-1) * dim.cell + dim.mid - dim.line - dim.cross)
          gc.strokeLine((col-1) * dim.cell + dim.mid - dim.line,            (row-1) * dim.cell + dim.mid - dim.line,
                        (col-1) * dim.cell + dim.mid - dim.line - dim.cross, (row-1) * dim.cell + dim.mid - dim.line)
          // bottom-left
          gc.strokeLine((col-1) * dim.cell + dim.mid - dim.line,            (row-1) * dim.cell + dim.mid + dim.line,
                        (col-1) * dim.cell + dim.mid - dim.line,            (row-1) * dim.cell + dim.mid + dim.line + dim.cross)
          gc.strokeLine((col-1) * dim.cell + dim.mid - dim.line,            (row-1) * dim.cell + dim.mid + dim.line,
                        (col-1) * dim.cell + dim.mid - dim.line - dim.cross, (row-1) * dim.cell + dim.mid + dim.line)
          // bottom-right
          gc.strokeLine((col-1) * dim.cell + dim.mid + dim.line,            (row-1) * dim.cell + dim.mid + dim.line,
                        (col-1) * dim.cell + dim.mid + dim.line,            (row-1) * dim.cell + dim.mid + dim.line + dim.cross)
          gc.strokeLine((col-1) * dim.cell + dim.mid + dim.line,            (row-1) * dim.cell + dim.mid + dim.line,
                        (col-1) * dim.cell + dim.mid + dim.line + dim.cross, (row-1) * dim.cell + dim.mid + dim.line)
          // top-right
          gc.strokeLine((col-1) * dim.cell + dim.mid + dim.line,            (row-1) * dim.cell + dim.mid - dim.line,
                        (col-1) * dim.cell + dim.mid + dim.line,            (row-1) * dim.cell + dim.mid - dim.line - dim.cross)
          gc.strokeLine((col-1) * dim.cell + dim.mid + dim.line,            (row-1) * dim.cell + dim.mid - dim.line,
                        (col-1) * dim.cell + dim.mid + dim.line + dim.cross, (row-1) * dim.cell + dim.mid - dim.line)

        case Strip((row, col), _) if row < 1 || row > size.row || col < 1 || col > size.col =>
        case Strip(_, (row, col)) if row < 1 || row > size.row || col < 1 || col > size.col =>

        case Strip((row1, col1), (row, col2)) if row1 == row =>
          val col = col1 min col2
          gc.setStroke(colors(2))
          gc.setLineWidth(dim.strip)
          gc.strokeLine(col * dim.cell, (row-1) * dim.cell,
                        col * dim.cell, row * dim.cell)

        case Strip((row1, col1), (row2, col)) if col1 == col =>
          val row = row1 min row2
          gc.setStroke(colors(2))
          gc.setLineWidth(dim.strip)
          gc.strokeLine((col-1) * dim.cell, row * dim.cell,
                        col * dim.cell,     row * dim.cell)

        case _ =>
      }

    private def draw(gc: GraphicsContext, start: Point, color: Int) =
      val (row, col) = start
      val letter = ('A' + -1-color).toChar
      gc.setFill(colors(color))
      gc.setStroke(colors(color))
      gc.fillOval(col * dim.cell + dim.mid - dim.start / 2,
                  row * dim.cell + dim.mid - dim.start / 2,
                  dim.start, dim.start)
      gc.setFill(White)
      gc.fillText(String.valueOf(letter),
                  col * dim.cell + dim.mid - dim.start / 4,
                  row * dim.cell + dim.mid + dim.start / 4)

    def draw(from: Point, to: Point, color: Int,
             start: Boolean, tip: Boolean, over: Boolean): Unit =
      val gc = self.getGraphicsContext2D()
      gc.setStroke(colors(color))
      gc.setLineWidth(dim.line)
      draw(gc, from + (-1, -1), to + (-1, -1), color, start, tip, over)

    private def draw(gc: GraphicsContext,
                     from: Point, to: Point, color: Int,
                     start: Boolean, tip: Boolean, over: Boolean = false): Unit =
      if !start && tip
      then
        gc.setFill(colors(0))
        gc.fillOval(from.col * dim.cell + dim.mid - (dim.line + 3) / 2,
                    from.row * dim.cell + dim.mid - (dim.line + 3) / 2,
                    dim.line + 3, dim.line + 3)
      gc.strokeLine(from.col * dim.cell + dim.mid, from.row * dim.cell + dim.mid,
                      to.col * dim.cell + dim.mid,   to.row * dim.cell + dim.mid)
      if start
      then
        draw(gc, from, color)
      if tip && !over
      then
        gc.setFill(colors(color))
        gc.fillOval(to.col * dim.cell + dim.mid - (dim.line + 3) / 2,
                    to.row * dim.cell + dim.mid - (dim.line + 3) / 2,
                    dim.line + 3, dim.line + 3)

////////////////////////////////////////////////////////////////////////////////

  object dim:

    inline val pad = 15
    inline val cell = 45
    inline val grid = 1
    inline val line = 9
    inline val mid = grid + (cell - grid) / 2
    inline val start = 16
    inline val cross = 5
    inline val strip = 5
