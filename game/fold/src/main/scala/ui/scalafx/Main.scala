package urru
package game
package fold
package ui.scalafx

import cats.effect.{ Deferred, ExitCode, IO, IOApp, Ref, Resource }
import cats.effect.std.{ Dispatcher, CyclicBarrier }

import common.{ Mongo, Mutable }

import grid.Game.Feature.*

import fold.util.Read
import util.App
import App.{ Event, apply }


object Main extends IOApp:

  def app(name: String, game: Mutable[Game],
          eventR: Ref[IO, Deferred[IO, Event]],
          loopCB: CyclicBarrier[IO]): Resource[IO, App] =
    for
      dispatcher <- Dispatcher.sequential[IO]
    yield
      new App(dispatcher, name, game, eventR, loopCB)

  override def run(args: List[String]): IO[ExitCode] =

    if args.isEmpty then file else mongo(args.head, args.tail)

  def loop(id: Ref[IO, Long], ls: Ref[IO, List[Int]]): IO[ExitCode] =
    for
      l <- ls.get
      i = l.head
      r <- Read(s"fold-lv-$i.txt")
      (size, clues) = r
      n <- id.get
      game = Mutable(Game(n, size, clues, Pending)) //, Just, Have, Pending
      eventD <- Deferred[IO, Event]
      eventR <- IO.ref(eventD)
      loopCB <- CyclicBarrier[IO](2)
      _ <- app(s"fold-lv-$i", game, eventR, loopCB).use { app =>
        game(app, eventR, loopCB).background.use { _ =>
          IO.interruptible { app.main(Array.empty[String]) }
        }
      }
      _ <- id.update(_ + 1)
      _ <- ls.update(_.tail)
      l <- ls.get
      ec <- if l.isEmpty
            then IO(ExitCode.Success)
            else loop(id, ls)
    yield
      ec

  def file: IO[ExitCode] =
    for
      id <- IO.ref(1L)
      //ls <- IO.ref(List(1170, 1171))
      ls <- IO.ref(List(1279))
      ec <- loop(id, ls)
    yield
      ec

  def mongo(id: String, rest: List[String]): IO[ExitCode] =
    val mongo = Mongo(Config().urru.mongo)
    for
      gameOpt <- IO.blocking {
        import spray.json.enrichString
        import fold.util.JsonFormats.GameJsonProtocol.*
        mongo.load(id, "fold").headOption.map(_.toJson.parseJson.convertTo[Game])
      }
      _ <-  gameOpt match
              case Some(_game) =>
                for
                  _ <- IO.unit
                  game = Mutable(_game)
                  eventD <- Deferred[IO, Event]
                  eventR <- IO.ref(eventD)
                  loopCB <- CyclicBarrier[IO](2)
                  _ <- app(id, game, eventR, loopCB).use { app =>
                    game(app, eventR, loopCB).background.use { _ =>
                      IO.interruptible { app.main(Array.empty[String]) }
                    }
                  }
                yield
                  ()
              case _ => IO.unit
      ec <- if rest.isEmpty
            then IO(ExitCode.Success)
            else this.mongo(rest.head, rest.tail)
    yield
      ec
