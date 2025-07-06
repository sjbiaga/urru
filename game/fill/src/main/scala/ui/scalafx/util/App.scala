package urru
package game
package fill
package ui.scalafx
package util

import scala.concurrent.duration.*

import cats.effect.{ Clock, Deferred, IO, Ref, Resource }
import cats.effect.std.{ Dispatcher, CyclicBarrier }

import scalafx.application.Platform.{ exit, runLater }
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import javafx.scene.input.{ KeyCode, KeyEvent, InputEvent, MouseEvent }
import scalafx.scene.layout.Background
import scalafx.scene.paint.Color.White
import scalafx.scene.text.Text
import scalafx.geometry.Insets
import scalafx.scene.layout.{ HBox, VBox }
import scalafx.application.JFXApp3

import common.grid.{ row, x, col, +, -, unary_! }
import common.{ Mongo, Mutable }
import Mutable.given

import Clue.*

import DnD.*

import Draw.{ colors, draw, redraw, dim }
import App.{ Event, apply, pads }


class App(dispatcher: Dispatcher[IO],
          val name: String, game: Mutable[Game],
          eventR: Ref[IO, Deferred[IO, Event]],
          loopCB: CyclicBarrier[IO])
    extends JFXApp3:

  val size = game.size

  private def dispatch(event: Event): Unit =
    dispatcher.unsafeRunAndForget {
      for
        eventD <- eventR.get
        _ <- eventD.complete(event)
        _ <- loopCB.await
      yield ()
    }

  private var item: Int = -1
  private var lock: Boolean = false

  private def dragged(X: Double, Y: Double, move: Boolean): Unit =
    if item >= 0
    then
      val pad = if move then 0 else dim.pad
      val x = (X - pad) / dim.cell
      val y = (Y - pad) / dim.cell
      val to = (y.toInt x x.toInt) + (1, 1)
      if 1 <= to.row && to.row <= size.row
      && 1 <= to.col && to.col <= size.col
      then
        if (x - (to.col-1)) * dim.cell < dim.mid
        || (y - (to.row-1)) * dim.cell < dim.mid
        then
          val drag = move -> (true -> (item -> to))
          lock = true
          dispatch(Event(None, Some(drag)))
        else
          val block = game.nowPlay.block.block
          board.redraw(game)(false, x, y, -item-1, block*)
          lock = false

  val board = new Canvas:
    width = size.col * dim.cell
    height = size.row * dim.cell

    private val gc = this.getGraphicsContext2D()

    this.addEventHandler(MouseEvent.MOUSE_PRESSED, { e =>
      val to = ((e.getY() / dim.cell).toInt x (e.getX() / dim.cell).toInt) + (1, 1)
      val i = game.state.map(_.play.head.block.block).indexWhere(_.contains(to))
      if i >= 0
      then
        item = i
        val drag = true -> (true -> (item -> (0, 0)))
        dispatch(Event(None, Some(drag)))
     })

    this.addEventHandler(MouseEvent.MOUSE_DRAGGED, { e =>
      dragged(e.getX(), e.getY(), true)
    })

    this.addEventHandler(MouseEvent.MOUSE_RELEASED, { e =>
      if item >= 0
      then
        item = -1
        if lock
        then
          val x = (e.getX() / dim.cell).toInt
          val y = (e.getY() / dim.cell).toInt
          val to = (y x x) + (1, 1)
          val drag = true -> (true -> (item -> to))
          dispatch(Event(None, Some(drag)))
        else
          val drag = true -> (true -> (item -> (-1 x -1)))
          dispatch(Event(None, Some(drag)))
    })

    this.addEventHandler(MouseEvent.MOUSE_CLICKED, { e =>
      val to = ((e.getY() / dim.cell).toInt x (e.getX() / dim.cell).toInt) + (1, 1)
      if e.getClickCount() > 1
      && 1 <= to.row && to.row <= size.row
      && 1 <= to.col && to.col <= size.col
      then
        val i = game.state.map(_.play.head.block.block).indexWhere(_.contains(to))
        if i >= 0
        then
          val drag = true -> (false -> (i -> (0, 0)))
          dispatch(Event(None, Some(drag)))
    })

  private val pad: ((Block, Int)) => Canvas = {
    case (Block(_, min, max, _, _, _, _, _, _*), i) =>
      val size = (max - min) + (1, 1)
      new Canvas:
        width = size.col * dim.cell / 2
        height = size.row * dim.cell / 2

        this.addEventHandler(MouseEvent.MOUSE_PRESSED, { _ =>
          if game.selectionMode == 0
          then
            if game.state(i).play.head.pad
            then
              item = i
              val drag = false -> (true -> (item -> (0, 0)))
              dispatch(Event(None, Some(drag)))
            else
              val drag = false -> (false -> (i -> (0, 0)))
              dispatch(Event(None, Some(drag)))
        })
  }

  val upper = game
    .state
    .map(_.play.last.block)
    .zipWithIndex
    .take(game.state.size / 3)
    .foldLeft(List[Canvas]())(_ :+ pad(_))

  private val upper_pad = new HBox:
    spacing = dim.pad
    children = upper

  val middle = game
    .state
    .map(_.play.last.block)
    .zipWithIndex
    .drop(game.state.size / 3)
    .take(game.state.size / 3)
    .foldLeft(List[Canvas]())(_ :+ pad(_))

  private val middle_pad = new HBox:
    spacing = dim.pad
    children = middle

  val lower = game
    .state
    .map(_.play.last.block)
    .zipWithIndex
    .drop(2 * game.state.size / 3)
    .foldLeft(List[Canvas]())(_ :+ pad(_))

  private val lower_pad = new HBox:
    spacing = dim.pad
    children = lower

  private val pads = new VBox:
    padding = Insets(0, 0, dim.pad, dim.pad)
    spacing = dim.pad
    children = upper_pad :: middle_pad :: lower_pad :: Nil
    background = Background.fill(colors(0))

  private val board_pad = new HBox:
    padding = Insets(0, 0, 0, 0)
    children = board :: pads :: Nil

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

  val prompt = List(Text(""), Text(""), Text(""), Text("✓ JUST"))

  override def start(): Unit =

    stage = new JFXApp3.PrimaryStage:

      title = s"FILL ($name) [${size.col}x${size.row}]"

      scene = new Scene:
        root = new VBox:
          padding = Insets(dim.pad, dim.pad, dim.pad, dim.pad)
          children = board_pad :: pending ::
            ( new HBox:
                padding = Insets(0, 0, 0, 0)
                children = prompt(0) :: current :: Nil
            ) :: prompt.tail

      this.addEventHandler(KeyEvent.KEY_PRESSED, { e =>
        dispatch(Event(Some(e), None))
      })

      this.addEventHandler(MouseEvent.MOUSE_DRAGGED, { e =>
        dragged(e.getX(), e.getY(), false)
      })

      this.addEventHandler(MouseEvent.MOUSE_RELEASED, { e =>
        if item >= 0
        then
          item = -1
          if lock
          then
            val x = ((e.getX() - dim.pad) / dim.cell).toInt
            val y = ((e.getY() - dim.pad) / dim.cell).toInt
            val to = (y x x) + (1, 1)
            val drag = false -> (true -> (item -> to))
            dispatch(Event(None, Some(drag)))
          else
            val drag = false -> (true -> (item -> (-1 x -1)))
            dispatch(Event(None, Some(drag)))
      })

    board.redraw(game)()
    game.pads(App.this)
    prompt(3).visible = false
    game(App.this, false, 0L, 0L)

  override def stopApp(): Unit =
    dispatch(Event(None, None))


object App:

  case class Event(key: Option[KeyEvent], drag: Option[(Boolean, (Boolean, (Int, Point)))])

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

      def reload(idleTimeR: Ref[IO, Long],
                 startedR: Ref[IO, Long]): IO[Unit] =
        for
          _ <- idleTimeR.set(0L)
          started <- Clock[IO].monotonic.map(_.toMillis)
          _ <- startedR.set(started)
          _ <- IO {
                 game.restart
                 game.startTime = started
                 board.redraw(game)()
                 game.pads(app)
               }
        yield ()

      def usage: IO[Unit] = help >> IO { exit() }

      def help: IO[Unit] = IO {
        println("Use TAB to switch colors, - and + to drag'n'drop, / for pad.")
        println("Use arrows ←, →, ↑, ↓ to move left, right, up, down.")
        println("Use # to toggle grid, twice @ to restart game, | to pause.")
        println("Use keys BACKSPACE and ENTER to undo or redo.")
      }

      def load(savepoint: Option[String]): IO[Unit] =
        IO.blocking {
          savepoint
            .flatMap {
              mongo.load(_, "fill").headOption.map {
                import spray.json.enrichString
                import fill.util.JsonFormats.GameJsonProtocol.*
                _.toJson.parseJson.convertTo[Game]
              }
            }
            .tapEach { game ::= _ }
            .foreach { _ =>
              board.redraw(game)()
              game.pads(app)
            }
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
            elapsed = System.currentTimeMillis - started

            _ <- startedR.update(_ + elapsed)

            _ <-  ( if key.isEmpty && drag.isEmpty
                    then
                      exitR.set(true) >> usage
                    else if drag.nonEmpty
                    then
                      IO {
                        val (move, (drop, (i, to))) = drag.get
                        if drop
                        then
                          if !to // pressed
                          then
                            if move
                            then
                              game.value(-i-1).dragOut
                            else
                              game.value(-i-1)
                            game.dropIn(-1)
                            board.redraw(game)()
                            game.pads(app, true)
                          else // drag'n'drop
                            val color = game.nowPlay.color
                            val block = game.nowPlay.block
                            lazy val ps = block.block.map { (row, col) =>
                              ((to.row-1 + row-1) x (to.col-1 + col-1)) + (1, 1)
                            }
                            if i >= 0
                            then // dragged
                              if game.dropOn(ps*)
                              then
                                board.redraw(game)(true, 0, 0, -i-1, ps*)
                            else // dropped
                              if 1 <= to.row && to.row <= game.size.row
                              && 1 <= to.col && to.col <= game.size.col
                              && game.dropOn(ps*)
                              then
                                game.nowPlay = game.nowPlay.copy(block = block(to - (1, 1)))
                                if game.dragOff(elapsed)
                                then
                                  game.pads(app, game.pending.isEmpty)
                                else
                                  game.dropOut
                                  game.pads(app, true)
                              else
                                game.dropOut
                                game.pads(app, true)
                              board.redraw(game)()
                        else if move
                        then
                          game.value(-i-1).dragOut
                          game(app)
                          game.pads(app, true)
                        else
                          game.value(-i-1)
                      }

                    else if game.selectionMode < 0
                    then
                      IO.unit

                    else if game.selectionMode > 0
                    then
                      val keyCode = key.get.getCode()

                      if keyCode eq KeyCode.ESCAPE
                      then
                        IO {
                          game.dropOut
                          board.redraw(game)()
                          game.pads(app, true)
                        }

                      else if keyCode eq KeyCode.BACK_SPACE
                      then
                        IO {
                          game.dragOn
                          board.redraw(game)()
                          if game.selectionMode == 0
                          then
                            game.pads(app, true)
                        }

                      else if keyCode eq KeyCode.ENTER
                      then
                        IO {
                          if !game.dragOff(elapsed)
                          then
                            game.dropOut
                            game.pads(app, true)
                          board.redraw(game)()
                        }

                      else if keyCode eq KeyCode.DIGIT3
                      then
                        IO {
                          game.showAxes = !game.showAxes
                          board.redraw(game)()
                        }

                      else if keyCode eq KeyCode.DIGIT2
                      then
                        reload(idleTimeR, startedR)

                      else
                        IO.unit

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
                          val i = -game.nowPlay.color-1
                          val m = game.state(i).play.size

                          if game.move(dir)(elapsed)
                          then
                            if game.pending.nonEmpty
                            then
                              board.redraw(game)()
                              game.pads(app)
                            else
                              val n = game.state(i).play.size
                              if n < m
                              then
                                board.redraw(game)()
                              else
                                game(app)
                        }

                      else if keyCode eq KeyCode.DIGIT3
                      then
                        IO {
                          game.showAxes = !game.showAxes
                          board.redraw(game)()
                        }

                      else if keyCode eq KeyCode.DIGIT2
                      then
                        reload(idleTimeR, startedR)

                      else if keyCode eq KeyCode.BACK_SLASH
                      then
                        pausedR.set(true)

                      else if keyCode eq KeyCode.F1
                      then
                        help

                      else if keyCode eq KeyCode.F2
                      then
                        import spray.json.enrichAny
                        import fill.util.JsonFormats.GameJsonProtocol.*
                        mongo.file(game.value.toJson, "fill", app.name)

                      else if keyCode eq KeyCode.F9
                      then
                        load(game.savepoint.previous)

                      else if keyCode eq KeyCode.COMMA
                      then
                        IO.blocking {
                          import spray.json.enrichAny
                          import fill.util.JsonFormats.GameJsonProtocol.*
                          val current = game.savepoint.current
                          game.savepoint.current = Some(mongo.save(game.value.toJson, "fill")._2)
                          game.savepoint.previous = current
                        }

                      else if keyCode eq KeyCode.MINUS
                      then
                        IO {
                          game.dragOut
                          game(app)
                          game.pads(app, true)
                        }

                      else if keyCode eq KeyCode.EQUALS
                      then
                        IO {
                          game.dropIn(1)
                          if game.selectionMode != 0
                          then
                            board.redraw(game)()
                            game.pads(app, true)
                        }

                      else if keyCode eq KeyCode.SLASH
                      then
                        IO {
                          game.showPad = !game.showPad
                          app.pads.visible = game.showPad
                        }

                      else if keyCode eq KeyCode.BACK_SPACE
                      then
                        IO {
                          if game.undo()(elapsed)
                          then
                            board.redraw(game)()
                            game.pads(app)
                        }

                      else if keyCode eq KeyCode.ENTER
                      then
                        IO {
                          if game.redo()(elapsed)
                          then
                            if game.pending.nonEmpty
                            then
                              board.redraw(game)()
                              game.pads(app)
                            else
                              game(app)
                              game.pads(app, true)
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

    def pads(app: App, now: Boolean = false): Unit =
      if now
      then
        val color = game.nowPlay.block.color
        val i = -color-1
        if i < game.state.size / 3
        then
          (app.upper.drop(i), i, 1).redraw(game)
        else if i < 2 * game.state.size / 3
        then
          (app.middle.drop(i % (game.state.size / 3)), i, 1).redraw(game)
        else
          (app.lower.drop(i % (2 * game.state.size / 3)), i, 1).redraw(game)
      else
        (app.upper, 0, game.state.size / 3).redraw(game)
        (app.middle, game.state.size / 3, game.state.size / 3).redraw(game)
        (app.lower, 2 * game.state.size / 3, game.state.size).redraw(game)

    def apply(app: App): Unit =
      val block = game.nowPlay.block
      if game.selectionMode == 0
      then
        val color = block.color
        val i = -color-1
        app.board.draw(game, i)
      else
        runLater { app.board.draw(block) }

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
      val block = game.nowPlay.block
      val color = block.color
      val i = -color-1

      val prompt = app.prompt

      prompt(0).text = s"• COLOR IS" +
        ( if game.state(i).play.head.pad
          then
            " <INIT>"
          else if game.state(i).over
          then
            " (FILL)"
          else
            " [OPEN]"
        )

      val gc = app.current.getGraphicsContext2D()
      gc.setFill(colors(color))
      gc.setStroke(colors(color))
      gc.fillRect(dim.grid, dim.grid, dim.block / 2, dim.block / 2)

      val play = game.state
        .map(_.play.head)
        .filterNot(_.pad)
        .map(_.block.block)

      val total = game.state.count(_.over)
      val percent = play.flatten.toSet.size.toDouble / game.init._1.size.toDouble
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
