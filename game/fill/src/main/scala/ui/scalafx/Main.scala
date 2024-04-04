package urru
package game
package fill
package ui.scalafx

import cats.effect.{ Deferred, ExitCode, IO, IOApp, Ref, Resource }
import cats.effect.std.Dispatcher

import javafx.scene.input.KeyEvent

import grid.Game.Feature._

import fill.util.Read
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

      def loop(id: Ref[IO, Long], mp: Ref[IO, Map[Int, List[Int]]], k: Int, ls: Ref[IO, List[Int]], t: String): IO[ExitCode] =
        for
          l <- ls.get
          i = l.head
          r <- Read(s"fill-$t$k-$i.txt")
          (size, clues) = r
          n <- id.get
          game = Game(n, size, clues) //, Just, Have, Pisc
          eventD <- Deferred[IO, Event]
          eventR <- IO.ref(eventD)
          loopD <- Deferred[IO, Unit]
          loopR <- IO.ref(loopD)
          _ <- app(args, s"fill-$t$k-$i", game, eventR, loopR).use(game(_, eventR, loopR))
          _ <- id.update(_ + 1)
          _ <- if l.tail.isEmpty then mp.update(_.tail) else ls.update(_.tail)
          m <- mp.get
          _ <- if m.isEmpty then IO(ExitCode.Success)
               else if l.tail.isEmpty then
                 for
                   m <- mp.get
                   (k, l) = m.head
                   ls <- IO.ref(l)
                   ec <- loop(id, mp, k, ls, t)
                 yield
                   ec
               else
                 loop(id, mp, k, ls, t)
        yield
          ExitCode.Error
      for
        id <- IO.ref(1L)
        // mp <- IO.ref(Map(0 -> List(0), 2 -> List(5, 168)))
        // mp <- IO.ref(Map(7 -> List(19)))
        mp <- IO.ref(Map(3 -> List(125)))
        // mp <- IO.ref(Map(4 -> List(14)))
        m <- mp.get
        (k, l) = m.head
        ls <- IO.ref(l)
        // t = "cl-wc"
        t = "jp-pb-"
        // t = ""
        ec <- loop(id, mp, k, ls, t)
      yield
        ec
