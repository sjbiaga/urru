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

  def app(name: String, game: Game,
          eventR: Ref[IO, Deferred[IO, Event]],
          loopR: Ref[IO, Deferred[IO, Unit]]): Resource[IO, App] =
    for
      dispatcher <- Dispatcher.sequential[IO]
    yield
      new App(dispatcher, name, game, eventR, loopR)

  override def run(args: List[String]): IO[ExitCode] =

    if args.isEmpty then file else mongo(args.head, args.tail)

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
      _ <- app(s"fill-$t$k-$i", game, eventR, loopR).use { app =>
        game(app, eventR, loopR).background.use { _ =>
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
    for
      gameOpt <- IO {
        import scala.concurrent.Await
        import scala.concurrent.duration._
        import spray.json.enrichString
        import fill.util.JsonFormats.GameJsonProtocol._
        import org.mongodb.scala._
        import org.mongodb.scala.model.Filters._
        val mongoClient = MongoClient("mongodb://127.0.0.1:27017")
        val database = mongoClient.getDatabase("urru")
        val collection = database.getCollection("fill")
        val observable = collection.find(equal("_id", id))
        val doc = Await.result(observable.toFuture(), 10.seconds)
        doc.headOption.map(_.toJson.parseJson.convertTo[Game])
      }
      _ <-  gameOpt match
              case Some(game) =>
                for
                  eventD <- Deferred[IO, Event]
                  eventR <- IO.ref(eventD)
                  loopD <- Deferred[IO, Unit]
                  loopR <- IO.ref(loopD)
                  _ <- app(id, game, eventR, loopR).use { app =>
                    game(app, eventR, loopR).background.use { _ =>
                      IO.interruptible { app.main(Array.empty[String]) }
                    }
                  }
                yield
                  ()
              case _ => IO.unit
      ec <- if rest.isEmpty
            then IO(ExitCode.Success)
            else mongo(rest.head, rest.tail)
    yield
      ec
