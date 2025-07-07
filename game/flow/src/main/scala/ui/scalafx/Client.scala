package urru
package game
package flow
package ui.scalafx

import scala.concurrent.ExecutionContext.Implicits.*

import cats.effect.{ Deferred, ExitCode, IO, IOApp, Ref, Resource }
import cats.effect.std.{ Dispatcher, CountDownLatch, CyclicBarrier }

import fs2.kafka.*

import javafx.scene.input.KeyEvent

import io.circe.generic.auto.*

import org.http4s.Method.POST
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.client.dsl.io.*

import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import common.{ Mongo, Mutable }
import grid.Game.Feature.*

import grid.Grid.Id
import grid.Grid.http4s.given
import Clue.http4s.given

import http.GameApi.Command.make
import http.GameApi.http4s.given

import flow.kafka.KafkaEvent

import flow.util.Read
import util.Cli
import Cli.{ Event, apply, flowPlayUri }


object Client extends IOApp:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  import urru.kafka.Serdes.*

  val consumerSettings =
    ConsumerSettings[IO, Id, KafkaEvent](deserializer[Id], deserializer[KafkaEvent])
      .withAutoOffsetReset(AutoOffsetReset.Earliest)
      .withBootstrapServers("localhost:9092")
      .withGroupId("gameflow")

  def cli(name: String, game: Mutable[Game],
          eventR: Ref[IO, Deferred[IO, Event]],
          loopCB: CyclicBarrier[IO]): Resource[IO, Cli] =
    for
      dispatcher <- Dispatcher.sequential[IO]
    yield
      new Cli(dispatcher, name, game, eventR, loopCB)

  override def run(args: List[String]): IO[ExitCode] = file

  def loop(id: Ref[IO, Long], ls: Ref[IO, List[Int]], t: String): IO[ExitCode] =
    for
      l <- ls.get
      i = l.head
      r <- Read(s"flow-$t-$i.txt")
      (size, clues) = r
      n <- id.get
      eventD <- Deferred[IO, Event]
      eventR <- IO.ref(eventD)
      loopCB <- CyclicBarrier[IO](2)
      kafkaCB <- CyclicBarrier[IO](2)
      exitR <- IO.ref(false)
      pausedR <- IO.ref(false)
      _ <- EmberClientBuilder.default[IO].build.use { httpClient =>
        IO.println(1) >>
        httpClient
          .expect[Id](POST(make(n, size, clues, Just, Pending), flowPlayUri))
          .flatMap { id =>
            IO.println(2) >>
            KafkaConsumer.resource(consumerSettings).use { consumer =>
              val mongo = Mongo(Config().urru.mongo)
              val game: Mutable[Game] = Mutable(null)
              val client: Mutable[Cli] = Mutable(null)
              IO.println(3) >>
              consumer.subscribeTo("flow") >>
              consumer
                .stream
                .evalMap { it => Cli.consume(mongo, client, game, exitR, pausedR)(it.record.value) <* kafkaCB.await }
                .compile.drain
                .background.use { _ =>
                  kafkaCB.await *>
                  IO.println(4) >>
                  cli(s"flow-$t-$i", game, eventR, loopCB).use { cli =>
                    IO { client ::= cli } >>
                    game(mongo, cli, id, httpClient, eventR, loopCB, kafkaCB, exitR, pausedR)
                      .background.use { _ =>
                        IO.interruptible { cli.main(Array.empty[String]) }
                      }
                  }
                }
            }
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
