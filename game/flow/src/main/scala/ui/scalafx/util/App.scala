package urru
package game
package flow
package ui.scalafx
package util

import scala.concurrent.duration.*
import scala.math.{ abs, signum => sgn }

import cats.effect.{ IO, Clock, Deferred, Ref, Resource }
import cats.effect.std.{ Dispatcher, CyclicBarrier }

import scalafx.application.Platform.{ exit => exitFX, runLater }
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import javafx.scene.input.{ KeyCode, KeyEvent, MouseEvent }
import scalafx.scene.paint.Color.White
import scalafx.scene.text.Text
import scalafx.geometry.Insets
import scalafx.scene.layout.{ HBox, VBox }
import scalafx.application.JFXApp3

import common.grid.{ row, x, col, +, - }
import common.{ Mongo, Mutable }
import Mutable.given

import Clue.*

import Draw.{ colors, draw, redraw, dim }
import App.{ apply, Event, prompt }


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
          ls :+ new Canvas:
            width = 2*dim.grid + dim.start
            height = 2*dim.grid + dim.start

            visible = false

            val gc = this.getGraphicsContext2D()
            gc.setFill(colors(color))
            gc.setStroke(colors(color))
            gc.fillOval(dim.grid, dim.grid, dim.start, dim.start)
            gc.setFill(White)
            val letter = ('A' + -1-color).toChar
            gc.fillText(String.valueOf(letter), dim.grid + dim.start / 4, dim.grid + 3 * dim.start / 4)
        }

  val current = new Canvas:
      width = 2*dim.grid + dim.start
      height = 2*dim.grid + dim.start

  val prompt = List(Text(""), Text(""), Text(""), Text("✓ BUSY"))

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
    game.prompt(App.this, false, 0L, 0L)

  override def stopApp(): Unit =
    dispatch(Event(None, None))


object App:

  import Busy.*

  enum Busy:
    case Traveling, Versus, Loosing

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

  private def usage: IO[Unit] = IO {
    println("Use letters A-N to select a color, and TAB to toggle the pair.")
    println("Use arrows ←, →, ↑, ↓ to move left, right, up, down.")
    println("Use # to toggle axes, twice @ to restart game, | to pause.")
    println("Use keys BACKSPACE and ENTER to undo or redo.")
  }

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
              mongo.load(_, "flow").headOption.map {
                import spray.json.enrichString
                import flow.util.JsonFormats.GameJsonProtocol.*
                _.toJson.parseJson.convertTo[Game]
              }
            }
            .tapEach { game ::= _ }
            .foreach { board.redraw(_) }
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
               pausedR: Ref[IO, Boolean],
               busyR: Ref[IO, Option[Busy]],
               busyCB: CyclicBarrier[IO]
      ): IO[Unit] =
        ( for
            busy <- Resource.eval(busyR.get)
            _ <-  ( if !busy.isDefined
                    then
                      Resource.unit[IO]
                    else (
                      busy.get match
                        case Traveling =>
                          busyCB.await
                          // for
                          //   _ <- IO { app.prompt(3).visible = true }
                          //   grid = game.grid.toMap
                          //   over = game.state.map(_.over).toSeq
                          //   _ <- game.Just.travel.fold(()) {
                          //     case (_, (_, Nil)) =>
                          //     case (_, (color, path)) =>
                          //       println()
                          //       path.foreach {
                          //         case (intensity, urru, depth, nesting, degree) =>
                          //           urru match
                          //             case Left(it) => print("undo"->nesting->it.move)
                          //             case Right(it) => print("redo"->nesting->it.undo.move)
                          //           // intensity.data.foreach {
                          //           //   case it if it(grid, over*) =>
                          //           //     // println(s"it=$it depth=$depth nesting=$nesting degree=$degree")
                          //           //   case _ =>
                          //           //     // assert(false)
                          //           // }
                          //         // case _ =>
                          //         //   IO.unit
                          //       }
                          //       println()
                          //   }.compile.drain
                          //   _ <- IO { app.prompt(3).visible = false }
                          //   _ <- busyCB.await
                          // yield ()
                        case Versus =>
                          for
                            r <- flow.util.sΠ.Versus(mongo, game, app.name, app.prompt(3), busyCB)
                            _ <- IO.println(r)
                          yield ()
                        case Loosing =>
                          import flow.util.sΠ.Loser
                          val (odd, start) = game.nowStart
                          val color = start.color
                          val k = -color-1
                          val i = 2*k+odd
                          for
                            _ <-  IO { app.prompt(3).visible = true }
                            _ <-  IO.blocking { Loser(mongo, app.name, game.savepoint.current.get, game, i) }
                            _ <-  IO { app.prompt(3).visible = false }
                            _ <-  busyCB.await
                          yield ()
                    ).background
                  )
            paused <- Resource.eval(pausedR.get)
            idleTime <- Resource.eval(idleTimeR.get)
            _ <-  ( if game.gameOver || paused
                    then
                      Resource.unit[IO]
                    else
                      tick(idleTime).background
                  )
          yield ()
        ).use { _ =>
          for
            busy <- busyR.get
            _ <-  ( if !busy.isDefined
                    then
                      IO.unit
                    else
                      busyCB.await
                  )
            _ <- busyR.set(None)

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
                        val (undo, (i, diff)) = drag.get
                        game.value.apply(-i/2-1)
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

                      else if letters.contains(keyCode)
                      then
                        val i = letters.indexOf(keyCode)
                        if i < game.state.size / 2
                        then
                          IO {
                            val (_, start) = game.nowStart
                            if start.color == -i-1
                            then
                              game.toggle
                            else
                              game.value.apply(-i-1)
                          }
                        else
                          IO.unit

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
                        usage

                      else if keyCode eq KeyCode.F2
                      then
                        import spray.json.enrichAny
                        import flow.util.JsonFormats.GameJsonProtocol.*
                        mongo.file(game.value.toJson, "flow", app.name)

                      else if keyCode eq KeyCode.F5
                      then
                        val savepoint = game.savepoint
                        load(game.savepoint.current) >>
                        IO {
                          game.savepoint.current = savepoint.current
                          game.savepoint.previous = savepoint.previous
                        }

                      else if keyCode eq KeyCode.F9
                      then
                        load(game.savepoint.previous)

                      else if keyCode eq KeyCode.COMMA
                      then
                        IO.blocking {
                          import spray.json.enrichAny
                          import flow.util.JsonFormats.GameJsonProtocol.*
                          game.savepoint.previous = game.savepoint.current
                          game.savepoint.current = Some(mongo.save(game.value.toJson, "flow")._2)
                        }

                      else if keyCode eq KeyCode.PERIOD
                      then
                        if game.savepoint.current.isDefined
                        then
                          busyR.set(Some(Loosing))
                        else
                          IO.unit

                      else if keyCode eq KeyCode.DIGIT7
                      then
                        if game.savepoint.current.isDefined
                        then
                          busyR.set(Some(Versus))
                        else
                          IO.unit

                      else if keyCode eq KeyCode.DIGIT8
                      then
                        busyR.set(Some(Traveling))

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
                        IO { game.toggle }

                      else
                        IO.unit

                  )

            now <- Clock[IO].monotonic.map(_.toMillis)
            _ <- IO { prompt(app, paused, idleTime, now) }
          yield ()
        } >> {
          for
            eventD <- Deferred[IO, Event]
            _ <- eventR.set(eventD)
            _ <- loopCB.await

            exit <- exitR.get
            _ <-  ( if exit
                    then
                      IO { exitFX() }
                    else
                      loop(idleTimeR, startedR, exitR, pendingR, pausedR, busyR, busyCB)
                  )
          yield ()
        }
      for
        idleTimeR <- IO.ref(0L)
        started <- Clock[IO].monotonic.map(_.toMillis)
        startedR <- IO.ref(started)
        exitR <- IO.ref(false)
        pendingR <- IO.ref(game.pending.nonEmpty)
        pausedR <- IO.ref(false)
        busyR <- IO.ref(None)
        busyCB <- CyclicBarrier[IO](2)
        _ <- IO { game.startTime = started }
        _ <- loop(idleTimeR, startedR, exitR, pendingR, pausedR, busyR, busyCB)
      yield ()

    /**
      * Draw the previous segment also to overwrite
      * the ugly trace of the (previous) cleared tip.
      */
    def apply(app: App): Unit =
      val (odd, start) = game.nowStart
      val color = start.color
      val k = -color-1
      val i = 2*k+odd
      val item = game.state(i)
      if item.over
      then
        app.board.redraw(game)
      else
        val play = item.play
        val from = play(play.size-2)
        val to = play(play.size-1)
        app.board.draw(from, to, color, play.size == 2, true, false) // tip
        if play.size > 2
        then
          val to = play(play.size-3)
          app.board.draw(to, from, color, play.size == 3, false, false) // previous

    def prompt(app: App, paused: Boolean, idleTime: Long, now: Long): Unit = runLater {

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
      val k = -color-1
      val i = 2*k+odd
      val j = 2*k+1-odd
      val item = game.state(i)
      val itemʹ = game.state(j)
      val point = game.tip

      val prompt = app.prompt

      prompt(0).text = s"• POINT AT ${point.row} x ${point.col}" +
        ( if item.over
          then
            " (FLOW)"
          else
            if item.play.size > 1 || itemʹ.play.size > 1
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
        else if now > 0
        then
          time(app, idleTime, now)

    }
