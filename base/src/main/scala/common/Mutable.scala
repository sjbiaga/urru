package urru
package common


final case class Mutable[T](var value: T):

  inline def :=(that: Mutable[T]) =
    this.value = that.value

  inline def ::=(other: T) =
    this.value = other

  inline def ===(other: T) =
    this.value == other


object Mutable:

  import io.github.greenleafoss.mongo.GreenLeafJsonProtocol
  import spray.json.JsonFormat

  trait MutableJsonProtocol extends GreenLeafJsonProtocol:
    implicit def MutableFormat[T : JsonFormat]: JsonFormat[Mutable[T]] = jsonFormat1[T, Mutable[T]](Mutable.apply)

  object MutableJsonProtocol extends MutableJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  given [T]: Conversion[Mutable[T], T] = _.value

  object http4s:

    import cats.effect.IO

    import io.circe.{ Decoder, Encoder }
    import io.circe.generic.auto.*

    import org.http4s.circe.{ jsonEncoderOf, jsonOf }
    import org.http4s.{ EntityDecoder, EntityEncoder }

    given [T: Decoder]: EntityDecoder[IO, Mutable[T]] = jsonOf
    given [T: Encoder]: EntityEncoder[IO, Mutable[T]] = jsonEncoderOf
