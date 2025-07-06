package urru
package game
package flow

import cats.effect.{ ExitCode, IO, IOApp }

import io.circe.generic.auto.*

import com.comcast.ip4s.*

import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.implicits.*

import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import urru.kafka.KafkaPublisher
import urru.kafka.KafkaProperties.default

import grid.Grid.Id
import grid.Game.Feature

import kafka.KafkaEvent

import http.GameApi
import GameApi.Command
import Clue.http4s.given
import Feature.http4s.given
import GameApi.http4s.given


object Server extends IOApp:

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  override def run(args: List[String]): IO[ExitCode] =
    for
      _ <- IO.unit
      kafkaPublisher = KafkaPublisher[Id, KafkaEvent](default)
      gameApi = new GameApi(kafkaPublisher)
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"7103")
        .withHttpApp(Logger.httpApp(true, true)(gameApi.routes.orNotFound))
        .build
        .use(_ => IO.never)
    yield
      ExitCode.Success
