package urru
package game
package flow

import scala.collection.mutable.{ ListBuffer => MutableList }


case class Cell(override val colors: MutableList[Int],
                clue: Option[Clue] = None)
    extends urru.grid.Game.Cell:
  override val group: Seq[Cell] = Seq(this)


object Cell:

  object http4s:

    import cats.effect.IO

    import io.circe.generic.auto.*

    import org.http4s.circe.{ jsonEncoderOf, jsonOf }
    import org.http4s.{ EntityDecoder, EntityEncoder }

    import common.grid.http4s.given
    import Clue.http4s.given

    given EntityDecoder[IO, Cell] = jsonOf
    given EntityEncoder[IO, Cell] = jsonEncoderOf
