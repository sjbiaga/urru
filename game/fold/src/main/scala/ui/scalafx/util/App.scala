package urru
package game
package fold
package ui.scalafx
package util

import scala.math.{ signum => sgn }

import cats.effect.{ Deferred, IO, Ref, Resource }
import cats.effect.std.Dispatcher

import fs2.Stream
import fs2.io.file.{ Files, Path }

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
    game(App.this, false, 0L)

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

  extension(game: Game)

    def apply(app: App,
              eventR: Ref[IO, Deferred[IO, Event]],
              loopR: Ref[IO, Deferred[IO, Unit]]): IO[Unit] =

      val board = app.board

      def usage: IO[Unit] = IO {
        println("Use TAB to switch colors.")
        println("Use arrows ←, →, ↑, ↓ to (un)fold left, right, up, down.")
        println("Use # to toggle grid, @ to restart game, | to pause.")
        println("Use keys BACKSPACE and ENTER to undo or redo.")
        exit()
      }

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
                                  // case it if it(grid, over*) =>
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
            _ <-  ( if game.showJust.nonEmpty
                    then
                      justD.get
                    // else if game.showHave.nonEmpty
                    // then
                    //   haveD.get
                    else
                      IO.unit
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

            _ <-  ( if key.isEmpty && drag.isEmpty
                    then
                      exitR.set(true) >> usage
                    else if drag.nonEmpty
                    then
                      IO {
                        val (undo, (i, dir)) = drag.get
                        game(-i-1)
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
                    else if key.get.getCode() eq KeyCode.ESCAPE
                    then
                      exitR.set(true) >> usage
                    else if paused
                    then
                      idleTimeR.update(_ + elapsed) >>
                      ( if key.get.getCode() eq KeyCode.BACK_SLASH
                        then
                         pausedR.set(false)
                        else
                          IO.unit
                      )

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
                          import scala.concurrent.Await
                          import scala.concurrent.duration._
                          import spray.json.enrichAny
                          import spray.json.JsString
                          import fold.util.JsonFormats.GameJsonProtocol._
                          import org.bson.types.ObjectId
                          import org.mongodb.scala._
                          val mongoClient = MongoClient("mongodb://127.0.0.1:27017")
                          val database = mongoClient.getDatabase("urru")
                          val collection = database.getCollection("fold")
                          var json = game.toJson
                          var jsonObj = json.asJsObject
                          val jsonId = JsString(ObjectId().toString)
                          jsonObj = jsonObj.copy(fields = jsonObj.fields.updated("_id", jsonId))
                          json = jsonObj.toJson
                          val observable = collection.insertOne(Document(json.toString))
                          Await.result(observable.toFuture(), 10.seconds)
                          json.prettyPrint
                        }
                        name = Pisc.uuid(app.name)
                        _ <- s.through(Files[IO].writeUtf8Lines(cwd / (name + ".json"))).compile.drain
                      yield ()

                    else if key.get.getCode() eq KeyCode.PERIOD // PiScala
                    then
                      Pisc(app.name, game)

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
                      IO { game.switch }

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

    def apply(app: App): Unit =
      val color = game.nowColor
      val i = -color-1
      runLater { app.board.draw(game, game.state(i).play) }

    def apply(app: App, paused: Boolean, idleTime: Long): Unit = runLater {

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
        else
          val elapsed = 0L max ((System.currentTimeMillis - idleTime) - game.startTime - (0L max game.minusTime))
          val ms = elapsed % 1000
          val dd = (elapsed / 1000) / 86400
          val hh = ((elapsed / 1000) % 86400) / 3600
          val mm = (((elapsed / 1000) % 86400) % 3600) / 60
          val ss = ((((elapsed / 1000) % 86400) % 3600) % 60) / 1
          prompt(2).text = s"• ELAPSED:${if dd > 0 then s" $dd day${if dd > 1 then "s" else ""}" else ""} $hh:$mm:$ss.$ms"

    }
