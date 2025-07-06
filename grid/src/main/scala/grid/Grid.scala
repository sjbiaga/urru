package urru
package grid


abstract trait Grid[C, K, V, F[_], W[_, _]]:
  val id: Grid.Id
  def grid: W[K, F[V]]
  def clues: Set[C]


object Grid:

  import java.time.ZonedDateTime

  case class Id(number: Long, timestamp: ZonedDateTime = ZonedDateTime.now)

////////////////////////////////////////////////////////////////////////////////

  abstract trait GridOps[M, R]:
    def move(m: M): R

  abstract trait PathOps[P]:
    def undo(p: P): Unit
    def redo(p: P): Unit

  object http4s:

    import cats.effect.IO

    import io.circe.generic.auto.*

    import org.http4s.circe.{ jsonEncoderOf, jsonOf }
    import org.http4s.{ EntityDecoder, EntityEncoder }

    given EntityDecoder[IO, Grid.Id] = jsonOf
    given EntityEncoder[IO, Grid.Id] = jsonEncoderOf
