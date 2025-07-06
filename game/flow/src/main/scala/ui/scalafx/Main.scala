package urru
package game
package flow
package ui.scalafx

import cats.effect.{ Deferred, ExitCode, IO, IOApp, Ref, Resource }
import cats.effect.std.{ Dispatcher, CyclicBarrier }

import common.{ Mongo, Mutable }

import grid.Game.Feature.*

import flow.util.Read
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

  def loop(id: Ref[IO, Long], ls: Ref[IO, List[Int]], t: String): IO[ExitCode] =
    for
      l <- ls.get
      i = l.head
      r <- Read(s"flow-$t-$i.txt")
      (size, clues) = r
      n <- id.get
      game = Mutable(Game(n, size, clues, Just)) // , Have
      eventD <- Deferred[IO, Event]
      eventR <- IO.ref(eventD)
      loopCB <- CyclicBarrier[IO](2)
      _ <- app(s"flow-$t-$i", game, eventR, loopCB).use { app =>
        game(app, eventR, loopCB).background.use { _ =>
          IO.interruptible { app.main(Array.empty[String]) }
        }
      }
      _ <- id.update(_ + 1)
      _ <- ls.update(_.tail)
      l <- ls.get
      ec <- if l.isEmpty
            then IO(ExitCode.Success)
            else loop(id, ls, t)
    yield
      ec

  def file: IO[ExitCode] =
    for
      id <- IO.ref(1L)
      // i <- List(2, 3)
      // t = "br"
      ls <- IO.ref(List(2)) // 3
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

  def mongo(id: String, rest: List[String]): IO[ExitCode] =
    val mongo = Mongo(Config().urru.mongo)
    for
      gameOpt <- IO.blocking {
        import spray.json.enrichString
        import flow.util.JsonFormats.GameJsonProtocol.*
        mongo.load(id, "flow").headOption.map(_.toJson.parseJson.convertTo[Game])
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
