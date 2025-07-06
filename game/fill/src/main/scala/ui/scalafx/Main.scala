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

  def loop(id: Ref[IO, Long], mp: Ref[IO, Map[Int, List[Int]]], k: Int, ls: Ref[IO, List[Int]], t: String): IO[ExitCode] =
    for
      l <- ls.get
      i = l.head
      r <- Read(s"fill-$t$k-$i.txt")
      (size, clues) = r
      n <- id.get
      game = Mutable(Game(n, size, clues)) //, Just, Have
      eventD <- Deferred[IO, Event]
      eventR <- IO.ref(eventD)
      loopCB <- CyclicBarrier[IO](2)
      _ <- app(s"fill-$t$k-$i", game, eventR, loopCB).use { app =>
        game(app, eventR, loopCB).background.use { _ =>
          IO.interruptible { app.main(Array.empty[String]) }
        }
      }
      _ <- id.update(_ + 1)
      _ <- if l.tail.isEmpty then mp.update(_.tail) else ls.update(_.tail)
      m <- mp.get
      ec <- if m.isEmpty then IO(ExitCode.Success)
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
      ec

  def file: IO[ExitCode] =
    for
      id <- IO.ref(1L)
      // mp <- IO.ref(Map(0 -> List(0), 2 -> List(5, 168)))
      // mp <- IO.ref(Map(7 -> List(19)))
      mp <- IO.ref(Map(20240409 -> List(3))) // 160
      // mp <- IO.ref(Map(4 -> List(14)))
      m <- mp.get
      (k, l) = m.head
      ls <- IO.ref(l)
      // t = "cl-wc"
      // t = "jp-pb-"
      t = "btp-d-"
      // t = ""
      ec <- loop(id, mp, k, ls, t)
    yield
      ec

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
