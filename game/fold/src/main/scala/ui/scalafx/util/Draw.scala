package urru
package game
package fold
package ui.scalafx
package util

import scalafx.application.Platform.runLater
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.shape.ArcType
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

    def redraw(game: Game): Unit = runLater {

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

      val grid =
        for
          row <- 1 to size.row
          col <- 1 to size.col
        yield
          row x col

      gc.setFill(colors(1))
      gc.setStroke(colors(1))
      draw(gc, grid*)

      // blocks ////////////////////////////////////////////////////////////////

      game.state.map(_.play).foreach(draw(game, _, true))

      // clues /////////////////////////////////////////////////////////////////

      game.clues_hints.foreach {
        case Empty(at) =>
          draw(gc, 0, at)

        case _ =>
      }

      draw(game)()

    }

    def draw(game: Game, play: Play, redraw: Boolean = false): Unit = runLater {
      play.head match
        case Block(_, _, color, block*) =>
          val gc = self.getGraphicsContext2D()
          if play.size == 1 || redraw
          then
            draw(gc, color, block*)
          else
            draw(gc, color, block.drop(block.size / 2)*)
            draw(game)()
    }

    private def draw(gc: GraphicsContext, color: Int, block: Point*): Unit =
      gc.setFill(colors(color))
      gc.setStroke(colors(color))
      draw(gc, block*)

    private def draw(gc: GraphicsContext, block: Point*): Unit =
      block.foreach { (row, col) =>
        gc.fillRect((col-1) * dim.cell + dim.mid - dim.block / 2,
                    (row-1) * dim.cell + dim.mid - dim.block / 2,
                    dim.block, dim.block)
      }

    // multi ///////////////////////////////////////////////////////////////////

    def draw(game: Game)(): Unit = runLater {

      val gc = self.getGraphicsContext2D()

      game.wildcards.foreach {
        case Multi(at @ (row, col)) =>
          draw(gc, 0, at)

          val bs = game.state
            .map(_.play.head)

          val cs = bs
            .collect[Int] {
              case Block(_, _, c, ps*) if ps.contains(at) => c
            }

          if cs.nonEmpty
          then
            val angle = 360.0 / cs.size

            for
              i <- 0 until cs.size
            do
              gc.setFill(colors(cs(i)))
              gc.fillArc((col-1) * dim.cell + dim.mid - dim.block / 2,
                         (row-1) * dim.cell + dim.mid - dim.block / 2,
                         dim.block, dim.block,
                         angle * i, angle, ArcType.Round)

          else
            gc.setFill(colors(1))
            gc.fillOval((col-1) * dim.cell + dim.mid - dim.block / 2,
                        (row-1) * dim.cell + dim.mid - dim.block / 2,
                        dim.block, dim.block)

        case _ =>
      }

    }

////////////////////////////////////////////////////////////////////////////////

  object dim:

    inline val pad = 15
    inline val cell = 45
    inline val grid = 1
    inline val block = 33
    inline val mid = grid + (cell - grid) / 2
