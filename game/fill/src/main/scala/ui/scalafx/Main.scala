package urru
package game
package fill
package ui.scalafx

import cats.effect.{ Deferred, ExitCode, IO, IOApp, Ref, Resource }
import cats.effect.std.{ Dispatcher, CyclicBarrier }

import common.{ Mongo, Mutable }

import grid.Game.Feature.*

import fill.util.Read
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

  def loop(id: Long, mp: Map[Int, List[Int]], k: Int, ls: List[Int], t: String): IO[ExitCode] =
    if mp.isEmpty
    then
      IO(ExitCode.Success)
    else if ls.isEmpty
    then
      val (k,ls) = mp.head
      loop(id, mp.tail, k, ls, t)
    else
      val i = ls.head
      for
        (size, clues) <- Read(s"fill-$t$k-$i.txt")
        game = Mutable(Game(id, size, clues, Pending)) //, Just, Have, Pending
        eventD <- Deferred[IO, Event]
        eventR <- IO.ref(eventD)
        loopCB <- CyclicBarrier[IO](2)
        _ <- app(s"fill-$t$k-$i", game, eventR, loopCB).use { app =>
          game(app, eventR, loopCB).background.use { _ =>
            IO.interruptible { app.main(Array.empty[String]) }
          }
        }
        _ <- loop(id + 1, mp, k, ls.tail, t)
      yield
        ExitCode.Error

  def file: IO[ExitCode] =
    // val mp = Map(0 -> List(0), 2 -> List(5, 168))
    // val mp = Map(7 -> List(19))
    // val mp = Map(4 -> List(14))
    val mp = Map(20240409 -> List(3)) // 160
    val (k, ls) = mp.head
    loop(1L, mp, k, ls, "btp-d-") // "cl-wc" "jp-pb-"

  def mongo(id: String, rest: List[String]): IO[ExitCode] =
    val mongo = Mongo(Config().urru.mongo)
    for
      gameOpt <- IO.blocking {
        import spray.json.enrichString
        import fill.util.JsonFormats.GameJsonProtocol.*
        mongo.load(id, "fill").headOption.map(_.toJson.parseJson.convertTo[Game])
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
