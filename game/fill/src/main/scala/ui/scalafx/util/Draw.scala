package urru
package game
package fill
package ui.scalafx
package util

import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.shape.ArcType
import Color._
import javafx.scene.canvas.GraphicsContext


object Draw:

  lazy val colors = Map[Int, Color](
     2 -> LightGrey,
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

  import urru.common.grid.{ row, col, x }

  import Clue._

  // board /////////////////////////////////////////////////////////////////////

  extension(self: Canvas)

    def redraw(game: Game)(lock: Boolean = false,
                           x: Double = 0, y: Double = 0,
                           c: Int = 0, ps: Point*): Unit =
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
          gc.strokeLine(0,                   row * dim.cell,
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
          gc.strokeLine(0,                   row * dim.cell,
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

      if game.selectionMode < 0
      then
        val i = -game.nowPlay.color-1
        val ls = game.state.map(_.play.head.block)
        ls.remove(i)
        ls.foreach(draw(_))
      else if game.selectionMode > 0
      then
        draw(game.nowPlay.block)
      else
        game.state
          .map(_.play.head.block)
          .foreach(draw(_))

      // clues /////////////////////////////////////////////////////////////////

      game.clues_hints.foreach {
        case Empty(at) =>
          draw(gc, 0, at)

        case _ =>
      }

      if lock
      then
        draw(game)(ps*)
      else
        draw(game)()

      // drag'n'drop ///////////////////////////////////////////////////////////

      if ps.nonEmpty
      then
        gc.setFill(colors(c))
        gc.setStroke(colors(c))
        ps.foreach {
          case at @ (row, col)
              if !lock
              || !game.wildcards.exists {
                case Multi(`at`) => true
                case _ => false
              } =>
            gc.fillRect((x + (col-1)) * dim.cell + dim.mid - dim.block / 2,
                        (y + (row-1)) * dim.cell + dim.mid - dim.block / 2,
                        dim.block, dim.block)
          case _ =>
        }

    def draw(block: Block): Unit = block match
      case Block(_, _, _, _, _, _, _, color, ps*) =>
        val gc = self.getGraphicsContext2D()
        draw(gc, color, ps*)

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

    def draw(game: Game, i: Int): Unit =
      val gc = self.getGraphicsContext2D()
      val color = -i-1
      val play = game.state(i).play
      draw(gc, 1, (play(1).block.block diff play(0).block.block)*)
      draw(gc, color, (play(0).block.block diff play(1).block.block)*)
      draw(game)()

    // multi ///////////////////////////////////////////////////////////////////

    def draw(game: Game)(ps: Point*): Unit =
      val gc = self.getGraphicsContext2D()

      game.wildcards.foreach {
        case Multi(at @ (row, col)) =>
          draw(gc, 0, at)

          var f = false

          if game.selectionMode > 0
          then
            if game.nowPlay.block.block.contains(at)
            then
              f = true
              gc.setFill(colors(game.nowPlay.color))
              gc.fillOval((col-1) * dim.cell + dim.mid - dim.block / 2,
                          (row-1) * dim.cell + dim.mid - dim.block / 2,
                          dim.block, dim.block)
          else
            val bs = game.state
              .map(_.play.head.block)

            if ps.nonEmpty
            then
              val c = game.nowPlay.color
              val i = -c-1
              bs(i) = Block((0,0), (0,0), (0,0), Nil, Nil, Nil, Nil, c, ps*)

            val cs = bs
              .collect[Int] {
                case Block(_, _, _, _, _, _, _, c, ps*) if ps.contains(at) => c
              }

            if cs.nonEmpty
            then
              f = true
              val angle = 360.0 / cs.size

              for
                i <- 0 until cs.size
              do
                gc.setFill(colors(cs(i)))
                gc.fillArc((col-1) * dim.cell + dim.mid - dim.block / 2,
                           (row-1) * dim.cell + dim.mid - dim.block / 2,
                           dim.block, dim.block,
                           angle * i, angle, ArcType.Round)

          if !f
          then
            gc.setFill(colors(1))
            gc.fillOval((col-1) * dim.cell + dim.mid - dim.block / 2,
                        (row-1) * dim.cell + dim.mid - dim.block / 2,
                        dim.block, dim.block)

        case _ =>
      }

  // pad ///////////////////////////////////////////////////////////////////////

  extension(self: (List[Canvas], Int, Int))

    def redraw(game: Game): Unit =

      def draw(gc: GraphicsContext, head: Play, block: Block): Unit = block match
        case Block(_, _, _, _, _, _, _, c, ps*) =>
          val color = if game.selectionMode == 0 && head.pad then c else 2
          gc.setFill(colors(color))
          gc.setStroke(colors(color))

          ps.foreach { (row, col) =>
            gc.fillRect((col-1) * dim.cell / 2 + dim.cell / 4 - dim.block / 4,
                        (row-1) * dim.cell / 2 + dim.cell / 4 - dim.block / 4,
                        dim.block / 2, dim.block / 2)
          }

      val (pad, i, n) = self

      if game.selectionMode == 0
      then
        val play = game.state.map(_.play).drop(i).take(n)
        val layer = play.map { it => it.head -> it.last }

        layer.zipWithIndex.foreach {
          case ((head, last), j) =>
            val gc = pad(j).getGraphicsContext2D()
            draw(gc, head, last.block)
        }
      else
        val j = -game.nowPlay.color-1
        if i <= j && j < i + n
        then
          val it = game.state(j).play
          val gc = pad(j - i).getGraphicsContext2D()
          draw(gc, it.head, it.last.block)


////////////////////////////////////////////////////////////////////////////////

  object dim:

    inline val pad = 15
    inline val cell = 45
    inline val grid = 1
    inline val block = 33
    inline val mid = grid + (cell - grid) / 2
