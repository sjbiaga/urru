package urru
package game
package flow
package ui.scalafx
package util

import scala.math.{ abs, signum => sgn }

import cats.effect.{ Deferred, IO, Ref, Resource }
import cats.effect.std.Dispatcher

import fs2.Stream
import fs2.io.file.{ Files, Path }

import com.googlecode.lanterna.terminal.Terminal
import com.googlecode.lanterna.input.KeyType

import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import javafx.scene.input.{ KeyCode, KeyEvent, MouseEvent }
import scalafx.scene.paint.Color.White
import scalafx.scene.text.Text
import scalafx.geometry.Insets
import scalafx.scene.layout.{ HBox, VBox }
import scalafx.application.JFXApp3

import common.grid.{ row, x, col, +, - }

import Clue._

import Draw.{ colors, draw, redraw, dim }
import App.{ apply, Event }


class App(dispatcher: Dispatcher[IO],
          val name: String, game: Game,
          eventR: Ref[IO, Deferred[IO, Event]],
          loopR: Ref[IO, Deferred[IO, Unit]])
    extends JFXApp3:

  private def dispatch(event: Event): Unit =
    dispatcher.unsafeRunAndForget {
      for
        eventD <- eventR.get
        _ <- eventD.complete(event)
        loopD <- loopR.get
        _ <- loopD.get
      yield ()
    }

  val size = game.size

  val board = new Canvas:
    width = size.col * dim.cell
    height = size.row * dim.cell

    private val gc = this.getGraphicsContext2D()

    private var from: Point = 0 x 0
    private var item: Int = -1
    private var undo: Boolean = false

    this.addEventHandler(MouseEvent.MOUSE_PRESSED, { e =>
      from = ((e.getY() / dim.cell).toInt x (e.getX() / dim.cell).toInt) + (1, 1)
      item = game.state.map(_.play.last).indexOf(from)
      if item >= 0
      then
        if game.state(item).over && game.state(item).play.size == 1
        then
          item = 2 * (item / 2) + (1 - item % 2)
        val drag = item -> (0, 0)
        dispatch(Event(None, Some(undo -> drag)))
    })

    this.addEventHandler(MouseEvent.MOUSE_DRAGGED, { e =>
      if item >= 0
      then
        val to = ((e.getY() / dim.cell).toInt x (e.getX() / dim.cell).toInt) + (1, 1)
        if 1 <= to.row && to.row <= size.row && 1 <= to.col && to.col <= size.col
        then
          val horz = from.row == to.row
          val vert = from.col == to.col

          if from != to && (horz || vert)
          then
            val drag = item -> ((to - from).row x (to - from).col)
            val play = game.state(item).play
            undo = false
            if play.size > 1
            then
              val from = play(play.size-2)
              val to = play(play.size-1)
              if horz == (from.row == to.row) && vert == (from.col == to.col)
              then
                undo = sgn((to - from).row + (to - from).col) != sgn(drag._2.row + drag._2.col)
            from = to
            dispatch(Event(None, Some(undo -> drag)))
    })

  val pending = new HBox:
      padding = Insets(0, 0, 0, 0)
      children = Text("• PENDING") :: (0 until game.state.size)
        .foldLeft(List[Canvas]()) { (ls, i) =>
          val color = -(i/2)-1
          val start = new Canvas:
            width = 2*dim.grid + dim.start
            height = 2*dim.grid + dim.start
            visible = false
          val gc = start.getGraphicsContext2D()
          gc.setFill(colors(color))
          gc.setStroke(colors(color))
          gc.fillOval(dim.grid, dim.grid, dim.start, dim.start)
          gc.setFill(White)
          val letter = ('A' + -1-color).toChar
          gc.fillText(String.valueOf(letter), dim.grid + dim.start / 4, dim.grid + 3 * dim.start / 4)
          start :: ls
        }.reverse

  val current = new Canvas:
      width = 2*dim.grid + dim.start
      height = 2*dim.grid + dim.start

  val prompt = List(Text(""), Text(""), Text(""), Text("✓ JUST"))

  override def start(): Unit =

    stage = new JFXApp3.PrimaryStage:

      title = s"FLOW ($name) [${size.col}x${size.row}]"

      scene = new Scene:
        root = new VBox:
          padding = Insets(dim.pad, dim.pad, dim.pad, dim.pad)
          children = board :: pending ::
            ( new HBox:
                padding = Insets(0, 0, 0, 0)
                children = prompt(0) :: current :: Nil
            ) :: prompt.tail

      this.addEventHandler(KeyEvent.KEY_PRESSED, { e =>
        dispatch(Event(Some(e), None))
      })

    board.redraw(game)
    prompt(3).visible = false
    game(App.this, false, 0L)


object App:

  case class Event(key: Option[KeyEvent], drag: Option[(Boolean, (Int, (Int, Int)))])

  private val letters = List(
    KeyCode.A, KeyCode.B, KeyCode.C, KeyCode.D,
    KeyCode.E, KeyCode.F, KeyCode.G, KeyCode.H,
    KeyCode.I, KeyCode.J, KeyCode.K, KeyCode.L,
    KeyCode.M, KeyCode.N,
  )

  private val arrows = Map(
    KeyCode.UP -> (-1, 0),
    KeyCode.DOWN -> (1, 0),
    KeyCode.LEFT -> (0, -1),
    KeyCode.RIGHT -> (0, 1),
  )

  extension(game: Game)

    def apply(app: App,
              eventR: Ref[IO, Deferred[IO, Event]],
              loopR: Ref[IO, Deferred[IO, Unit]]): IO[Unit] =

      val board = app.board

      def loop(idleTimeR: Ref[IO, Long],
               startedR: Ref[IO, Long],
               exitR: Ref[IO, Boolean],
               pendingR: Ref[IO, Boolean],
               justD: Deferred[IO, Unit],
               pausedR: Ref[IO, Boolean]
      ): IO[Unit] =
        ( for
            _ <-  ( if game.showJust.isEmpty
                    then
                     Resource.unit[IO]
                    else
                      ( for
                          _ <- justD.complete(())
                        yield ()
                      ).background
/*
                      ( for
                          _ <- IO { app.prompt(3).visible = false }
                          grid = game.grid.toMap
                          over = game.state.map(_.over).toSeq
                          _ <- game.Just.travel.fold(()) { case (_, (color, path)) =>
                            path.headOption match {
                              case Some((intensity, urru, depth, nesting, degree)) =>
                                intensity.data.foreach {
                                  case it if it(grid, over*) =>
                                    // println(s"it=$it depth=$depth nesting=$nesting degree=$degree")
                                  case _ =>
                                    // assert(false)
                                }
                              case _ =>
                            }
                          }.compile.drain
                          _ <- IO { app.prompt(3).visible = true }
                          _ <- justD.complete(())
                        yield ()
                      ).background
*/
                  )
          yield ()
        ).use { _ =>
          for
            _ <-  ( if game.showJust.isEmpty
                    then
                      IO.unit
                    else
                      justD.get
                    // else if game.showHave.nonEmpty
                    // then
                    //   haveD.get
                  )
            idleTime <- idleTimeR.get
            paused <- pausedR.get

            started <- startedR.get
            ended = System.currentTimeMillis

            _ <- idleTimeR.update(_ + ended - started)

            _ <- startedR.set(ended)

            eventD <- eventR.get
            Event(key, drag) <- eventD.get

            started <- startedR.get
            elapsed = System.currentTimeMillis - started

            _ <- startedR.update(_ + elapsed)

            _ <-  ( if drag.nonEmpty
                    then
                       IO {
                        val (undo, (i, diff)) = drag.get
                        game(-i/2-1)
                        val (odd, _) = game.nowStart
                        if i%2 != odd then game.toggle
                        var count = abs(diff.row) + abs(diff.col)
                        if undo
                        then
                          while count > 0 && game.undo()(elapsed / count)
                          do
                            count -= 1
                          board.redraw(game)
                        else
                          val dir = (sgn(diff.row), sgn(diff.col))
                          while count > 0 && game.move(dir)(elapsed / count)
                          do
                            count -= 1
                            if game.pending.nonEmpty
                            then
                              board.redraw(game)
                            else
                              game(app)
                      }
                    else if key.get.getCode() eq KeyCode.ESCAPE
                    then
                      exitR.set(true) >> IO {
                        println("Use letters A-N to select a color, and TAB to toggle the pair.")
                        println("Use arrows <-, ->, ^, v to draw left, right, up, down.")
                        println("Use # to toggle axes, @ to restart game, | to pause.")
                        println("Use keys BACKSPACE and ENTER to undo or redo.")
                      }
                    else if paused
                    then
                      idleTimeR.update(_ + elapsed) >>
                      ( if key.get.getCode() eq KeyCode.BACK_SLASH
                        then
                         pausedR.set(false)
                        else
                          IO.unit
                      )

                    else if letters.contains(key.get.getCode())
                    then
                      val i = letters.indexOf(key.get.getCode())
                      if i < game.state.size / 2
                      then
                        IO {
                          val (_, start) = game.nowStart
                          if start.color == -i-1
                          then
                            game.toggle
                          else
                            game(-i-1)
                        }
                      else
                        IO.unit

                    else if arrows.keySet.contains(key.get.getCode())
                    then
                      IO {
                        val dir = arrows(key.get.getCode())

                        if game.move(dir)(elapsed)
                        then
                          if game.pending.nonEmpty
                          then
                            board.redraw(game)
                          else
                            game(app)
                      }

                    else if key.get.getCode() eq KeyCode.DIGIT3
                    then
                      IO {
                        game.showAxes = !game.showAxes
                        board.redraw(game)
                      }
                    else if key.get.getCode() eq KeyCode.DIGIT2
                    then
                      for
                        _ <- idleTimeR.set(0L)
                        _ <- startedR.set(System.currentTimeMillis)
                        started <- startedR.get
                        _ <- IO {
                          game.restart
                          game.startTime = started
                          board.redraw(game)
                        }
                      yield ()

                    else if key.get.getCode() eq KeyCode.BACK_SLASH
                    then
                      pausedR.set(true)

                    else if key.get.getCode() eq KeyCode.COMMA
                    then
                      for
                        cwd <- Files[IO].currentWorkingDirectory
                        s = Stream.emit[IO, String] {
                          import spray.json.enrichAny
                          import flow.util.JsonFormats.GameJsonProtocol._
                          game.toJson.prettyPrint
                        }
                        name = Pisc.uuid(app.name)
                        _ <- s.through(Files[IO].writeUtf8Lines(cwd / (name + ".json"))).compile.drain
                      yield ()

                    else if key.get.getCode() eq KeyCode.PERIOD // PiScala
                    then
                      for
                        _ <- Pisc(app.name, game)
                      yield ()

                    else if key.get.getCode() eq KeyCode.DIGIT7
                    then
                      IO {
                        game.showJust = None
                        app.prompt(3).visible = false
                      }

                    else if key.get.getCode() eq KeyCode.DIGIT8
                    then
                      IO { game.showJust = game.showJust.map(!_).orElse(Some(true)) }

                    else if key.get.getCode() eq KeyCode.BACK_SPACE
                    then
                      IO {
                        if game.undo()(elapsed)
                        then
                          board.redraw(game)
                      }

                    else if key.get.getCode() eq KeyCode.ENTER
                    then
                      IO {
                        if game.redo()(elapsed)
                        then
                          if game.pending.nonEmpty
                          then
                            board.redraw(game)
                          else
                            game(app)
                      }

                    else if key.get.getCode() eq KeyCode.TAB
                    then
                      IO { game.toggle }

                    else
                      IO.unit

                  )

            _ <- IO { game(app, paused, idleTime) } // prompt
          yield ()
        } >> {
          for
            eventD <- Deferred[IO, Event]
            _ <- eventR.set(eventD)
            loopD <- loopR.get
            _ <- loopD.complete(())
            loopD <- Deferred[IO, Unit]
            _ <- loopR.set(loopD)

            justD <- Deferred[IO, Unit]
            _ <-  ( if game.showJust.isEmpty
                    then
                      justD.complete(())
                    else
                      IO.unit
                  )
            exit <- exitR.get
            _ <-  ( if exit
                    then
                      IO.unit
                    else
                      loop(idleTimeR, startedR, exitR, pendingR, justD, pausedR)
                  )
          yield ()
        }
      for
        idleTimeR <- IO.ref(0L)
        idleTimeR <- IO.ref(0L)
        startedR <- IO.ref(System.currentTimeMillis)
        exitR <- IO.ref(false)
        pendingR <- IO.ref(game.pending.nonEmpty)
        justD <- Deferred[IO, Unit]
        _ <-  ( if game.showJust.isEmpty
                then
                  justD.complete(())
                else
                  IO.unit
              )
        pausedR <- IO.ref(false)
        started <- startedR.get
        _ <- IO { game.startTime = started }
        _ <- loop(idleTimeR, startedR, exitR, pendingR, justD, pausedR)
      yield ()

    /**
      * Draw the previous segment also to overwrite
      * the ugly trace of the (previous) cleared tip.
      */
    def apply(app: App): Unit =
      val (odd, start) = game.nowStart
      val color = start.color
      val i = -color-1
      if game.state(2*i+odd).over
      then
        app.board.redraw(game)
      else
        val play = game.state(2*i+odd).play
        val from = play(play.size-2)
        val to = play(play.size-1)
        app.board.draw(from, to, color, play.size == 2, true, false) // tip
        if play.size > 2
        then
          val to = play(play.size-3)
          app.board.draw(to, from, color, play.size == 3, false, false) // previous

    def apply(app: App, paused: Boolean, idleTime: Long): Unit =
      val pending = app.pending.getChildren()
      for
        i <- 0 until game.state.size
      do
        pending
          .get(i+1)
          .visibleProperty()
          .set {
            game.pending.exists {
              case (`i`, _) => true
              case _ => false
            }
          }

      val size = game.size
      val (odd, start) = game.nowStart
      val color = start.color
      val i = -color-1
      val point = game.tip

      val prompt = app.prompt

      prompt(0).text = s"• POINT AT ${point.row} x ${point.col}" +
        ( if game.state(2*i+odd).over
          then
            " (FLOW)"
          else
            if game.state(2*i+odd).play.size > 1 || game.state(2*i+1-odd).play.size > 1
            then
              " [OPEN]"
            else
              " <INIT>"
        )

      val gc = app.current.getGraphicsContext2D()
      gc.setFill(colors(color))
      gc.setStroke(colors(color))
      gc.fillOval(dim.grid, dim.grid, dim.start, dim.start)
      val letter = ('A' + -1-color).toChar
      gc.setFill(White)
      gc.fillText(String.valueOf(letter), dim.grid + dim.start / 4, dim.grid + 3 * dim.start / 4)

      val total = game.state.size / 2
      val count = game.state.count(_.over) / 2
      val percent = game.state.flatMap(_.play).toSet.size.toDouble / game.init._1.size.toDouble
      prompt(1).text = s"• PLAYING… CONNECTED: #$count/$total COVERAGE: ${(percent * 100.0).toInt}%"

      if game.gameOver
      then
        prompt(2).text = s"• GAME OVER${if game.status then ": YOU WIN!" else "!"}"
      else
        if paused
        then
          prompt(2).text = "• PAUSED"
        else
          val elapsed = 0L max ((System.currentTimeMillis - idleTime) - game.startTime - (0L max game.minusTime))
          val ms = elapsed % 1000
          val dd = (elapsed / 1000) / 86400
          val hh = ((elapsed / 1000) % 86400) / 3600
          val mm = (((elapsed / 1000) % 86400) % 3600) / 60
          val ss = ((((elapsed / 1000) % 86400) % 3600) % 60) / 1
          prompt(2).text = s"• ELAPSED:${if dd > 0 then s" $dd day${if dd > 1 then "s" else ""}" else ""} $hh:$mm:$ss.$ms"
