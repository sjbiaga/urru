package urru
package game
package flow
package http

import cats.effect.IO

import fs2.Stream

import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

import urru.kafka.KafkaPublisher

import common.grid.x

import grid.Grid.Id
import grid.Grid.http4s.given
import Game.http4s.given

import grid.Game.Feature

import flow.kafka.*

import GameApi.*
import GameApi.http4s.given


class GameApi(kafkaPublisher: KafkaPublisher[Id, KafkaEvent]) extends Http4sDsl[IO]:

  def routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "flow" / "play" =>
      req.decode[IO, Command] {
        case make(number, size, clues, feats*) =>
          val io =
            for
              _    <- IO.unit
              id    = Id(number)
              _    <- kafkaPublisher.send("flow", id, Made(id, size, clues, feats*))
              res  <- Ok(id)
            yield
              res
          IO.println(0) >>
          io.handleErrorWith(err => BadRequest(err.getMessage))
        case pick(id, color) =>
          for
            _    <- kafkaPublisher.send("flow", id, Picked(color))
            res  <- Ok(id)
          yield
            res
        case move(id, dir, count, elapsed) =>
          for
            _    <- kafkaPublisher.send("flow", id, Moved(dir, count, elapsed))
            res  <- Ok(id)
          yield
            res
        case undo(id, count, elapsed) =>
          for
            _    <- kafkaPublisher.send("flow", id, Undone(count, elapsed))
            res  <- Ok(id)
          yield
            res
        case redo(id, elapsed) =>
          for
            _    <- kafkaPublisher.send("flow", id, Redone(elapsed))
            res  <- Ok(id)
          yield
            res
        case toggle(id) =>
          for
            _    <- kafkaPublisher.send("flow", id, Toggled)
            res  <- Ok(id)
          yield
            res
        case pause(id, value) =>
          for
            _    <- kafkaPublisher.send("flow", id, Paused(value))
            res  <- Ok(id)
          yield
            res
        case exit(id) =>
          for
            _    <- kafkaPublisher.send("flow", id, Exited)
            res  <- Ok(id)
          yield
            res
        case  save(id) =>
          for
            _    <- kafkaPublisher.send("flow", id, Saved)
            res  <- Ok(id)
          yield
            res
        case  refresh(id) =>
          for
            _    <- kafkaPublisher.send("flow", id, Refreshed)
            res  <- Ok(id)
          yield
            res
        case  load(id) =>
          for
            _    <- kafkaPublisher.send("flow", id, Loaded)
            res  <- Ok(id)
          yield
            res
        case  comma(id) =>
          for
            _    <- kafkaPublisher.send("flow", id, Comma)
            res  <- Ok(id)
          yield
            res
      }
    case req @ GET -> Root / "flow" / "just" / "travel" =>
      req.decode[IO, Game] { game =>
        grid.Game.duals.put(game.id, game)
        val io = Ok(game.just.travel)
        IO.println(-1) >> io
      }
  }


object GameApi:

  export Command.*

  enum Command:
    case make(number: Long, size: Point, clues: Set[Clue], feats: Feature*)
    case pick(id: Id, color: Int)
    case move(id: Id, dir: (Int, Int), count: Int, elapsed: Long)
    case undo(id: Id, count: Int, elapsed: Long)
    case redo(id: Id, elapsed: Long)
    case toggle(id: Id)
    case pause(id: Id, value: Boolean)
    case exit(id: Id)
    case save(id: Id)
    case refresh(id: Id)
    case load(id: Id)
    case comma(id: Id)

  object http4s:

    import tense.intensional.Data.Doubt

    import io.circe.generic.auto.*

    import org.http4s.circe.{ jsonEncoderOf, jsonOf }
    import org.http4s.{ EntityDecoder, EntityEncoder }

    import UndoRedo.{ Undo, Redo }

    import Clue.http4s.given
    import Data.http4s.given
    import UndoRedo.http4s.given

    given EntityDecoder[IO, (Int, Seq[(Doubt, Undo Either Redo, Int, Int, Int)])] = jsonOf
    given EntityEncoder[IO, (Int, Seq[(Doubt, Undo Either Redo, Int, Int, Int)])] = jsonEncoderOf

    given EntityDecoder[IO, Command] = jsonOf
    given EntityEncoder[IO, Command] = jsonEncoderOf
