package urru
package game
package flow
package ui.scalafx
package util

import scala.math.{ abs, signum => sgn }

import cats.effect.{ Deferred, IO, Ref, Resource }
import cats.effect.std.{ Dispatcher, CyclicBarrier }

import fs2.Stream
import fs2.io.file.{ Files, Path }

import org.http4s.Request
import org.http4s.Method.{ GET, POST }
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.client.{ Client => HttpClient }
import org.http4s.client.dsl.io.*

import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

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
import common.Mutable
import Mutable.given

import grid.Grid.Id
import grid.Grid.http4s.given

import Clue.*

import flow.kafka.*
import http.GameApi.*
import http.GameApi.http4s.given

import Draw.{ colors, draw, redraw, dim }
import Cli.{ apply, Event }


class Cli(dispatcher: Dispatcher[IO],
          val name: String, game: Game,
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
    game(Cli.this, false, 0L)

  override def stopApp(): Unit =
    dispatch(Event(None, None))


object Cli:

  import io.circe.generic.auto.*

  import io.circe.Json
  import io.circe.parser.decode
  import org.typelevel.jawn.fs2.*
  import org.typelevel.jawn.Facade

  import tense.intensional.Data.Doubt
  import UndoRedo.*
  import Data.http4s.given
  import UndoRedo.http4s.given
  import Game.http4s.given

  given Facade[Json] = new io.circe.jawn.CirceSupportParser(None, false).facade

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  import org.http4s.implicits.*

  val flowPlayUri = uri"http://localhost:7103/flow/play"

  private val flowJustTravelUri = uri"http://localhost:7103/flow/just/travel"

  def consume(cli: Mutable[Cli], game: Mutable[Game],
              kafkaCB: CyclicBarrier[IO],
              exitR: Ref[IO, Boolean],
              pausedR: Ref[IO, Boolean]): KafkaEvent => IO[Unit] = {
    case Made(id, size, clues, feats*) =>
      IO.pure(game ::= Game(id, size, clues, feats*)) >> kafkaCB.await
    case Picked(color) =>
      IO { game.value(color) }.void >> kafkaCB.await
    case Moved(dir, count, elapsed) =>
      IO {
        var i = count
        while i > 0 && game.move(dir)(elapsed / count)
        do
          i -= 1
          if game.pending.nonEmpty
          then
            cli.board.redraw(game)
          else
            game(cli)
      } >> kafkaCB.await
    case Undone(count, elapsed) =>
      IO {
        var i = count
        while i > 0 && game.undo()(elapsed / count)
        do
          i -= 1
        cli.board.redraw(game)
      } >> kafkaCB.await
    case Redone(elapsed) =>
      IO {
        if game.redo()(elapsed)
        then
          if game.pending.nonEmpty
          then
            cli.board.redraw(game)
          else
            game(cli)
      } >> kafkaCB.await
    case Toggled =>
      IO.pure(game.toggle) >> kafkaCB.await
    case Exited =>
      IO.println(0) >> exitR.set(true) >> usage >> kafkaCB.await
    case Paused(value) =>
      pausedR.set(value) >> kafkaCB.await
  }


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
    println("Use # to toggle axes, @ to restart game, | to pause.")
    println("Use keys BACKSPACE and ENTER to undo or redo.")
    exitFX()
  }

  extension(game: Game)

    def apply(cli: Cli, id: Id, httpClient: HttpClient[IO],
              eventR: Ref[IO, Deferred[IO, Event]],
              loopCB: CyclicBarrier[IO],
              kafkaCB: CyclicBarrier[IO],
              exitR: Ref[IO, Boolean],
              pausedR: Ref[IO, Boolean]): IO[Unit] =

      val board = cli.board

      def loop(idleTimeR: Ref[IO, Long],
               startedR: Ref[IO, Long],
               pendingR: Ref[IO, Boolean],
               justCB: CyclicBarrier[IO]
      ): IO[Unit] =
        ( for
            _ <-  ( if true // game.showJust.isEmpty
                    then
                      Resource.unit[IO]
                    else
                      // justCB.await.background
                      ( for
                          _ <- IO { cli.prompt(3).visible = false }
                          grid = game.grid.toMap
                          over = game.state.map(_.over).toSeq
                          _ <-  ( for
                                    client <- Stream.resource(EmberClientBuilder.default[IO].build)
                                    req     = GET(game, flowJustTravelUri)
                                    sr     <- Stream.eval(IO.pure(req))
                                    res    <- client.stream(sr).flatMap(_.body.chunks.parseJsonStream)
                                  yield
                                    res
                                )
                                .map(_.toString)
                                .map(decode[(Int, Seq[(Doubt, Undo Either Redo, Int, Int, Int)])](_))
                                .map(_.right.get)
                                .fold(()) {
                                  case (_, (_, Nil)) =>
                                  case (_, (color, path)) =>
                                    println()
                                    path.foreach {
                                      case (intensity, urru, depth, nesting, degree) =>
                                        urru match
                                          case Left(it) => print("undo"->nesting->it.move)
                                          case Right(it) => print("redo"->nesting->it.undo.move)
                                        // intensity.data.foreach {
                                        //   case it if it(grid, over*) =>
                                        //     // println(s"it=$it depth=$depth nesting=$nesting degree=$degree")
                                        //   case _ =>
                                        //     // assert(false)
                                        // }
                                      // case _ =>
                                      //   IO.unit
                                    }
                                    println()
                                }.compile.drain
                          _ <- IO { cli.prompt(3).visible = true }
                          _ <- justCB.await
                        yield ()
                      ).background
                  )
          yield ()
        ).use { _ =>
          for
            _ <-  ( if true // game.showJust.isEmpty
                    then
                      IO.unit
                    else
                      justCB.await
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
                      httpClient.expect[Id](POST(exit(id), flowPlayUri)).void >> kafkaCB.await
                    else if drag.nonEmpty
                    then
                      val (undo, (i, diff)) = drag.get
                      httpClient.expect[Id](POST(pick(id, -i/2-1), flowPlayUri)).void >> kafkaCB.await >> {
                        val (odd, _) = game.nowStart
                        val count = abs(diff.row) + abs(diff.col)
                        ( if i%2 != odd
                          then
                            httpClient.expect[Id](POST(toggle(id), flowPlayUri)).void >> kafkaCB.await
                          else
                            IO.unit
                        ) >>
                        ( if undo
                          then
                            httpClient.expect[Id](POST(Command.undo(id, count, elapsed), flowPlayUri)).void >> kafkaCB.await
                          else
                            val dir = (sgn(diff.row), sgn(diff.col))
                            httpClient.expect[Id](POST(move(id, dir, count, elapsed), flowPlayUri)).void >> kafkaCB.await
                        )
                      }
                    else if key.get.getCode() eq KeyCode.ESCAPE
                    then
                      httpClient.expect[Id](POST(exit(id), flowPlayUri)).void >> kafkaCB.await
                    else if paused
                    then
                      idleTimeR.update(_ + elapsed) >>
                      ( if key.get.getCode() eq KeyCode.BACK_SLASH
                        then
                         httpClient.expect[Id](POST(pause(id, false), flowPlayUri)).void >> kafkaCB.await
                        else
                          IO.unit
                      )

                    else if letters.contains(key.get.getCode())
                    then
                      val i = letters.indexOf(key.get.getCode())
                      if i < game.state.size / 2
                      then
                        val (_, start) = game.nowStart
                        if start.color == -i-1
                        then
                          httpClient.expect[Id](POST(toggle(id), flowPlayUri)).void >> kafkaCB.await
                        else
                          httpClient.expect[Id](POST(pick(id, -i-1), flowPlayUri)).void >> kafkaCB.await
                      else
                        IO.unit

                    else if arrows.keySet.contains(key.get.getCode())
                    then
                      val dir = arrows(key.get.getCode())
                      httpClient.expect[Id](POST(Command.move(id, dir, 1, elapsed), flowPlayUri)).void >> kafkaCB.await

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
                      httpClient.expect[Id](POST(pause(id, true), flowPlayUri)).void >> kafkaCB.await

                    else if key.get.getCode() eq KeyCode.COMMA
                    then
                      for
                        cwd <- Files[IO].currentWorkingDirectory
                        s = Stream.emit[IO, String] {
                          import scala.concurrent.Await
                          import scala.concurrent.duration.*
                          import spray.json.enrichAny
                          import spray.json.JsString
                          import flow.util.JsonFormats.GameJsonProtocol.*
                          import org.bson.types.ObjectId
                          import org.mongodb.scala.*
                          val mongoClient = MongoClient("mongodb://10.192.168.184:27017")
                          val database = mongoClient.getDatabase("urru")
                          val collection = database.getCollection("flow")
                          var json = game.toJson
                          var jsonObj = json.asJsObject
                          val jsonId = JsString(ObjectId().toString)
                          jsonObj = jsonObj.copy(fields = jsonObj.fields.updated("_id", jsonId))
                          json = jsonObj.toJson
                          val observable = collection.insertOne(Document(json.toString))
                          Await.result(observable.toFuture(), 10.seconds)
                          json.prettyPrint
                        }
                        name = common.Mongo.uuid(cli.name)
                        _ <- s.through(Files[IO].writeUtf8Lines(cwd / (name + ".json"))).compile.drain
                      yield ()

                    else if key.get.getCode() eq KeyCode.BACK_SPACE
                    then
                      httpClient.expect[Id](POST(Command.undo(id, 1, elapsed), flowPlayUri)).void >> kafkaCB.await

                    else if key.get.getCode() eq KeyCode.ENTER
                    then
                      httpClient.expect[Id](POST(Command.redo(id, elapsed), flowPlayUri)).void >> kafkaCB.await

                    else if key.get.getCode() eq KeyCode.TAB
                    then
                      httpClient.expect[Id](POST(toggle(id), flowPlayUri)).void >> kafkaCB.await

                    else
                      IO.unit

                  )

            _ <- IO { game(cli, paused, idleTime) } // prompt
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
                      loop(idleTimeR, startedR, pendingR, justCB)
                  )
          yield ()
        }
      for
        idleTimeR <- IO.ref(0L)
        idleTimeR <- IO.ref(0L)
        startedR <- IO.ref(System.currentTimeMillis)
        pendingR <- IO.ref(game.pending.nonEmpty)
        justCB <- CyclicBarrier[IO](2)
        started <- startedR.get
        _ <- IO { game.startTime = started }
        _ <- loop(idleTimeR, startedR, pendingR, justCB)
      yield ()

    /**
      * Draw the previous segment also to overwrite
      * the ugly trace of the (previous) cleared tip.
      */
    def apply(cli: Cli): Unit =
      val (odd, start) = game.nowStart
      val color = start.color
      val i = -color-1
      if game.state(2*i+odd).over
      then
        cli.board.redraw(game)
      else
        val play = game.state(2*i+odd).play
        val from = play(play.size-2)
        val to = play(play.size-1)
        cli.board.draw(from, to, color, play.size == 2, true, false) // tip
        if play.size > 2
        then
          val to = play(play.size-3)
          cli.board.draw(to, from, color, play.size == 3, false, false) // previous

    def apply(cli: Cli, paused: Boolean, idleTime: Long): Unit = runLater {

      val pending = cli.pending.getChildren()
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

      val prompt = cli.prompt

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

      val gc = cli.current.getGraphicsContext2D()
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
          val elapsed = (System.currentTimeMillis - idleTime) - game.startTime
          val ms = elapsed % 1000
          val dd = (elapsed / 1000) / 86400
          val hh = ((elapsed / 1000) % 86400) / 3600
          val mm = (((elapsed / 1000) % 86400) % 3600) / 60
          val ss = ((((elapsed / 1000) % 86400) % 3600) % 60) / 1
          prompt(2).text = s"• ELAPSED:${if dd > 0 then s" $dd day${if dd > 1 then "s" else ""}" else ""} $hh:$mm:$ss.$ms"

    }
