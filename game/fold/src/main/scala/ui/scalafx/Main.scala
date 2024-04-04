package urru
package game
package fold
package ui.scalafx

import cats.effect.{ Deferred, ExitCode, IO, IOApp, Ref, Resource }
import cats.effect.std.Dispatcher

import javafx.scene.input.KeyEvent

import grid.Game.Feature._

import fold.util.Read
import util.App
import App.{ Event, apply }


object Main extends IOApp:

  def app(args: List[String],
          name: String, game: Game,
          eventR: Ref[IO, Deferred[IO, Event]],
          loopR: Ref[IO, Deferred[IO, Unit]]): Resource[IO, App] =
    for
      dispatcher <- Dispatcher[IO]
      app = new App(dispatcher, name, game, eventR, loopR)
      _ <- IO.interruptible { app.main(args.toArray) }.background
    yield app

  override def run(args: List[String]): IO[ExitCode] =

    def loop(id: Ref[IO, Long], ls: Ref[IO, List[Int]]): IO[ExitCode] =
      for
        l <- ls.get
        i = l.head
        r <- Read(s"fold-lv-$i.txt")
        (size, clues) = r
        n <- id.get
        game = Game(n, size, clues) //, Just, Have, Pisc
        eventD <- Deferred[IO, Event]
        eventR <- IO.ref(eventD)
        loopD <- Deferred[IO, Unit]
        loopR <- IO.ref(loopD)
        _ <- app(args, s"fold-lv-$i", game, eventR, loopR).use(game(_, eventR, loopR))
        _ <- id.update(_ + 1)
        _ <- ls.update(_.tail)
        l <- ls.get
        _ <- if l.isEmpty then IO(ExitCode.Success)
             else loop(id, ls)
      yield
        ExitCode.Error
    for
      id <- IO.ref(1L)
      //ls <- IO.ref(List(1170, 1171))
      ls <- IO.ref(List(1279))
      ec <- loop(id, ls)
    yield
      ec
