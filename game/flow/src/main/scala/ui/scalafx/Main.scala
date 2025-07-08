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

  def loop(id: Long, ls: List[Int], t: String): IO[ExitCode] =
    if ls.isEmpty
    then IO(ExitCode.Success)
    else
      val i = ls.head
      for
        (size, clues) <- Read(s"flow-$t-$i.txt")
        game = Mutable(Game(id, size, clues, Just, Just /*Pending*/)) // , Have, Pending
        eventD <- Deferred[IO, Event]
        eventR <- IO.ref(eventD)
        loopCB <- CyclicBarrier[IO](2)
        _ <- app(s"flow-$t-$i", game, eventR, loopCB).use { app =>
          game(app, eventR, loopCB).background.use { _ =>
            IO.interruptible { app.main(Array.empty[String]) }
          }
        }
        _ <- loop(id + 1, ls.tail, t)
      yield
        ExitCode.Error

  def file: IO[ExitCode] =
    loop(1L, List(2), "cl-wc")

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
