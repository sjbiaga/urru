package urru
package game
package flow
package ui.scalafx

import cats.effect.{ Deferred, ExitCode, IO, IOApp, Ref, Resource }
import cats.effect.std.Dispatcher

import javafx.scene.input.KeyEvent

import grid.Game.Feature._

import flow.util.Read
import util.App
import App.{ Event, apply }


object Main extends IOApp:

  def app(name: String, game: Game,
          eventR: Ref[IO, Deferred[IO, Event]],
          loopR: Ref[IO, Deferred[IO, Unit]]): Resource[IO, App] =
    for
      dispatcher <- Dispatcher.sequential[IO]
    yield
      new App(dispatcher, name, game, eventR, loopR)

  override def run(args: List[String]): IO[ExitCode] =

    def loop(id: Ref[IO, Long], ls: Ref[IO, List[Int]], t: String): IO[ExitCode] =
      for
        l <- ls.get
        i = l.head
        r <- Read(s"flow-$t-$i.txt")
        (size, clues) = r
        n <- id.get
        game = Game(n, size, clues) //, Just, Have, Pisc
        eventD <- Deferred[IO, Event]
        eventR <- IO.ref(eventD)
        loopD <- Deferred[IO, Unit]
        loopR <- IO.ref(loopD)
        _ <- app(s"flow-$t-$i", game, eventR, loopR).use { app =>
          game(app, eventR, loopR).background.use { _ =>
            IO.interruptible { app.main(args.toArray) }
          }
        }
        _ <- id.update(_ + 1)
        _ <- ls.update(_.tail)
        l <- ls.get
        ec <- if l.isEmpty then IO(ExitCode.Success) else loop(id, ls, t)
      yield
        ec
    for
      id <- IO.ref(1L)
      // i <- List(2, 3)
      // t = "br"
      ls <- IO.ref(List(2, 3))
      t = "cl-wc"
      // i <- List(2, 3, 4, 5)
      // i <- List(3)
      // t = "cl"
      //ls <- IO.ref(List(1))
      //t = "weekly-htb"
      // ls <- IO.ref(List(11))
      // t = "weekly-p"
      ec <- loop(id, ls, t)
    yield
      ec
