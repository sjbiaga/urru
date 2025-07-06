package urru
package game
package fold
package ui.scalafx
package util

import scala.concurrent.duration.*
import scala.math.{ signum => sgn }

import cats.effect.{ Clock, Deferred, IO, Ref, Resource }
import cats.effect.std.{ Dispatcher, CyclicBarrier }

import scalafx.application.Platform.{ exit, runLater }
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import javafx.scene.input.{ KeyCode, KeyEvent, MouseEvent }
import scalafx.scene.paint.Color.White
import scalafx.scene.text.Text
import scalafx.geometry.Insets
import scalafx.scene.layout.{ HBox, VBox }
import scalafx.application.JFXApp3

import common.grid.{ row, x, col, +, -, unary_! }
import common.{ Mongo, Mutable }
import Mutable.given

import Clue.*

import Draw.{ colors, draw, redraw, dim }
import App.{ apply, Event }


class App(dispatcher: Dispatcher[IO],
          val name: String, game: Mutable[Game],
          eventR: Ref[IO, Deferred[IO, Event]],
          loopCB: CyclicBarrier[IO])
    extends JFXApp3:

  private def dispatch(event: Event): Unit =
    dispatcher.unsafeRunAndForget {
      for
        eventD <- eventR.get
        _ <- eventD.complete(event)
        _ <- loopCB.await
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
      item = game.state.map(_.play.head.block).indexWhere(_.contains(from))
      if item >= 0
      then
        val drag = item -> (0, 0)
        dispatch(Event(None, Some(false -> drag)))
    })

    this.addEventHandler(MouseEvent.MOUSE_DRAGGED, { e =>
      if item >= 0
      then
        val to = ((e.getY() / dim.cell).toInt x (e.getX() / dim.cell).toInt) + (1, 1)
        if 1 <= to.row && to.row <= size.row && 1 <= to.col && to.col <= size.col
        then
          if from != to
          then
            val play = game.state(item).play
            val block = play.head.block
            val i = block.indexOf(from)
            val j = block.indexOf(to)
            if i >= 0
            then
              undo = play.size > 1
                  && block.size / 2 <= i
                  && 0 <= j && j < block.size / 2
              if undo || j < 0
              then
                val drag = item -> (sgn((to - from).row) x sgn((to - from).col))
                from = to
                dispatch(Event(None, Some(undo -> drag)))
              else
                from = to
    })

  val pending = new HBox:
      padding = Insets(0, 0, 0, 0)
      children = Text("• PENDING") :: (0 until game.state.size)
        .foldLeft(List[Canvas]()) { (ls, i) =>
          val color = -i-1
          ls :+ new Canvas:
            width = 2*dim.grid + dim.block / 2
            height = 2*dim.grid + dim.block / 2

            visible = false

            val gc = this.getGraphicsContext2D()
            gc.setFill(colors(color))
            gc.setStroke(colors(color))
            gc.fillRect(dim.grid, dim.grid, dim.block, dim.block)
        }

  val current = new Canvas:
      width = 2*dim.grid + dim.block / 2
      height = 2*dim.grid + dim.block / 2

  val prompt = List(Text("• COLOR IS"), Text(""), Text(""), Text("✓ JUST"))

  override def start(): Unit =

    stage = new JFXApp3.PrimaryStage:

      title = s"FOLD ($name) [${size.col}x${size.row}]"

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
    game(App.this, false, 0L, 0L)

  override def stopApp(): Unit =
    dispatch(Event(None, None))


object App:

  case class Event(key: Option[KeyEvent], drag: Option[(Boolean, (Int, (Int, Int)))])

  private val arrows = Map(
    KeyCode.UP -> (-1, 0),
    KeyCode.DOWN -> (1, 0),
    KeyCode.LEFT -> (0, -1),
    KeyCode.RIGHT -> (0, 1),
  )

  extension(game: Mutable[Game])

    private def time(app: App, idleTime: Long, now: Long): Unit =
      val elapsed = (now - idleTime) - game.startTime
      val ms = elapsed % 1000
      val dd = (elapsed / 1000) / 86400
      val hh = ((elapsed / 1000) % 86400) / 3600
      val mm = (((elapsed / 1000) % 86400) % 3600) / 60
      val ss = ((((elapsed / 1000) % 86400) % 3600) % 60) / 1
      app.prompt(2).text = s"• ELAPSED:${if dd > 0 then s" $dd day${if dd > 1 then "s" else ""}" else ""} $hh:$mm:$ss.$ms"

    def apply(app: App,
              eventR: Ref[IO, Deferred[IO, Event]],
              loopCB: CyclicBarrier[IO]): IO[Unit] =

      val mongo = Mongo(Config().urru.mongo)

      val board = app.board

      def load(savepoint: Option[String]): IO[Unit] =
        IO.blocking {
          savepoint
            .flatMap {
              mongo.load(_, "fold").headOption.map {
                import spray.json.enrichString
                import fold.util.JsonFormats.GameJsonProtocol.*
                _.toJson.parseJson.convertTo[Game]
              }
            }
            .tapEach { game ::= _ }
            .foreach { board.redraw(_) }
        }

      def usage: IO[Unit] = help >> IO { exit() }

      def help: IO[Unit] = IO {
        println("Use TAB to switch colors.")
        println("Use arrows ←, →, ↑, ↓ to (un)fold left, right, up, down.")
        println("Use # to toggle grid, twice @ to restart game, | to pause.")
        println("Use keys BACKSPACE and ENTER to undo or redo.")
      }

      def tick(idleTime: Long): IO[Unit] =
        for
          _ <- IO.sleep(1.second)
          now <- Clock[IO].monotonic.map(_.toMillis)
          _ <- IO { runLater { time(app, idleTime, now) } }
          _ <- tick(idleTime)
        yield ()

      def loop(idleTimeR: Ref[IO, Long],
               startedR: Ref[IO, Long],
               exitR: Ref[IO, Boolean],
               pendingR: Ref[IO, Boolean],
               justCB: CyclicBarrier[IO],
               pausedR: Ref[IO, Boolean]
      ): IO[Unit] =
        ( for
            _ <-  ( if true
                    then
                      Resource.unit[IO]
                    else
                      Resource.eval(justCB.await)
/*
                      ( for
                          _ <- IO { app.prompt(3).visible = false }
                          grid = game.grid.toMap
                          over = game.state.map(_.over).toSeq
                          _ <- game.Just.travel.fold(()) { case (_, (color, path)) =>
                            path.headOption match {
                              case Some((intensity, urru, depth, nesting, degree)) =>
                                intensity.data.foreach {
                                  // case it if it(grid, over*) =>
                                    // println(s"it=$it depth=$depth nesting=$nesting degree=$degree")
                                  case _ =>
                                    // assert(false)
                                }
                              case _ =>
                            }
                          }.compile.drain
                          _ <- IO { app.prompt(3).visible = true }
                          _ <- justCB.await
                        yield ()
                      ).background
*/
                  )
            paused <- Resource.eval(pausedR.get)
            idleTime <- Resource.eval(idleTimeR.get)
            _ <- (  if game.gameOver || paused
                    then
                      Resource.unit[IO]
                    else
                      tick(idleTime).background
                 )
          yield ()
        ).use { _ =>
          for
            _ <-  ( if true
                    then
                      IO.unit
                    else
                      justCB.await
                  )
            idleTime <- idleTimeR.get
            paused <- pausedR.get

            started <- startedR.get
            ended <- Clock[IO].monotonic.map(_.toMillis)

            _ <- idleTimeR.update(_ + ended - started)

            _ <- startedR.set(ended)

            eventD <- eventR.get
            Event(key, drag) <- eventD.get

            started <- startedR.get
            ended <- Clock[IO].monotonic.map(_.toMillis)
            elapsed = ended - started

            _ <- startedR.update(_ + elapsed)

            _ <-  ( if key.isEmpty && drag.isEmpty
                    then
                      exitR.set(true) >> usage
                    else if drag.nonEmpty
                    then
                      IO {
                        val (undo, (i, dir)) = drag.get
                        game.value(-i-1)
                        if undo
                        then
                          if game.undo()(elapsed)
                          then
                            board.redraw(game)
                        else
                          if !(!dir) && game.move(dir)(elapsed)
                          then
                            if game.pending.nonEmpty
                            then
                              board.redraw(game)
                            else
                              game(app)
                      }
                    else
                      val keyCode = key.get.getCode()

                      if keyCode eq KeyCode.ESCAPE
                      then
                        exitR.set(true) >> usage
                      else if paused
                      then
                        idleTimeR.update(_ + elapsed) >>
                        ( if keyCode eq KeyCode.BACK_SLASH
                          then
                           pausedR.set(false)
                          else
                            IO.unit
                        )

                      else if arrows.keySet.contains(keyCode)
                      then
                        IO {
                          val dir = arrows(keyCode)

                          if game.move(dir)(elapsed)
                          then
                            if game.pending.nonEmpty
                            then
                              board.redraw(game)
                            else
                              game(app)
                        }

                      else if keyCode eq KeyCode.DIGIT3
                      then
                        IO {
                          game.showAxes = !game.showAxes
                          board.redraw(game)
                        }
                      else if keyCode eq KeyCode.DIGIT2
                      then
                        for
                          _ <- idleTimeR.set(0L)
                          started <- Clock[IO].monotonic.map(_.toMillis)
                          _ <- startedR.set(started)
                          _ <- IO {
                                 game.restart
                                 game.startTime = started
                                 board.redraw(game)
                               }
                        yield ()

                      else if keyCode eq KeyCode.BACK_SLASH
                      then
                        pausedR.set(true)

                      else if keyCode eq KeyCode.F1
                      then
                        help

                      else if keyCode eq KeyCode.F2
                      then
                        import spray.json.enrichAny
                        import fold.util.JsonFormats.GameJsonProtocol.*
                        mongo.file(game.value.toJson, "fold", app.name)

                      else if keyCode eq KeyCode.F9
                      then
                        load(game.savepoint.previous)

                      else if keyCode eq KeyCode.COMMA
                      then
                        IO.blocking {
                          import spray.json.enrichAny
                          import fold.util.JsonFormats.GameJsonProtocol.*
                          val current = game.savepoint.current
                          game.savepoint.current = Some(mongo.save(game.value.toJson, "fold")._2)
                          game.savepoint.previous = current
                        }

                      else if keyCode eq KeyCode.BACK_SPACE
                      then
                        IO {
                          if game.undo()(elapsed)
                          then
                            board.redraw(game)
                        }

                      else if keyCode eq KeyCode.ENTER
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

                      else if keyCode eq KeyCode.TAB
                      then
                        IO { game.switch }

                      else
                        IO.unit

                  )

            now <- Clock[IO].monotonic.map(_.toMillis)
            _ <- IO { game(app, paused, idleTime, now) } // prompt
          yield ()
        } >> {
          for
            eventD <- Deferred[IO, Event]
            _ <- eventR.set(eventD)
            _ <- loopCB.await

            exit <- exitR.get
            _ <-  ( if exit
                    then
                      IO.unit
                    else
                      loop(idleTimeR, startedR, exitR, pendingR, justCB, pausedR)
                  )
          yield ()
        }
      for
        idleTimeR <- IO.ref(0L)
        started <- Clock[IO].monotonic.map(_.toMillis)
        startedR <- IO.ref(started)
        exitR <- IO.ref(false)
        pendingR <- IO.ref(game.pending.nonEmpty)
        justCB <- CyclicBarrier[IO](2)
        pausedR <- IO.ref(false)
        _ <- IO { game.startTime = started }
        _ <- loop(idleTimeR, startedR, exitR, pendingR, justCB, pausedR)
      yield ()

    def apply(app: App): Unit =
      val color = game.nowColor
      val i = -color-1
      runLater { app.board.draw(game, game.state(i).play) }

    def apply(app: App, paused: Boolean, idleTime: Long, now: Long): Unit = runLater {

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
      val color = game.nowColor
      val i = -color-1

      val prompt = app.prompt

      val gc = app.current.getGraphicsContext2D()
      gc.setFill(colors(color))
      gc.setStroke(colors(color))
      gc.fillRect(dim.grid, dim.grid, dim.block / 2, dim.block / 2)

      val total = game.state.count(_.over)
      val percent = game.state.flatMap(_.play.head.block).toSet.size.toDouble / game.init._1.size.toDouble
      prompt(1).text = s"• PLAYING… BLOCKED: #$total/${game.state.size} COVERAGE: ${(percent * 100.0).toInt}%"

      if game.gameOver
      then
        prompt(2).text = s"• GAME OVER${if game.status then ": YOU WIN!" else "!"}"
      else
        if paused
        then
          prompt(2).text = "• PAUSED"
        else if now > 0
        then
          time(app, idleTime, now)

    }
