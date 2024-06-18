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

  def app(name: String, game: Game,
          eventR: Ref[IO, Deferred[IO, Event]],
          loopR: Ref[IO, Deferred[IO, Unit]]): Resource[IO, App] =
    for
      dispatcher <- Dispatcher.sequential[IO]
    yield
      new App(dispatcher, name, game, eventR, loopR)

  override def run(args: List[String]): IO[ExitCode] =

    if args.isEmpty then file else mongo(args.head, args.tail)

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
      _ <- app(s"fold-lv-$i", game, eventR, loopR).use { app =>
        game(app, eventR, loopR).background.use { _ =>
          IO.interruptible { app.main(Array.empty[String]) }
        }
      }
      _ <- id.update(_ + 1)
      _ <- ls.update(_.tail)
      l <- ls.get
      ec <- if l.isEmpty then IO(ExitCode.Success)
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
    for
      gameOpt <- IO {
        import scala.concurrent.Await
        import scala.concurrent.duration._
        import spray.json.enrichString
        import fold.util.JsonFormats.GameJsonProtocol._
        import org.mongodb.scala._
        import org.mongodb.scala.model.Filters._
        val mongoClient = MongoClient("mongodb://127.0.0.1:27017")
        val database = mongoClient.getDatabase("urru")
        val collection = database.getCollection("fold")
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
