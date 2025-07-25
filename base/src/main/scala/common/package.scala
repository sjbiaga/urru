package urru


package common:

  object Implicits:

    @inline implicit def it2it[T](self: Iterable[Option[T]]): Iterable[T] = self.filter(_.nonEmpty).map(_.get)

////////////////////////////////////////////////////////////////////////////////

    import io.github.greenleafoss.mongo.GreenLeafJsonProtocol
    import spray.json.JsonFormat

    trait MutableJsonProtocol extends GreenLeafJsonProtocol:

      import scala.collection.mutable.{ HashMap, HashSet }
      import scala.collection.mutable.{ ListBuffer => MutableList, StringBuilder }

      import spray.json.{ JsArray, JsObject, JsString, JsValue }
      import spray.json.enrichAny

      //https://stackoverflow.com/questions/33943345/spray-json-marshalling-mutable-objects
      implicit def ListBufferFormat[T : JsonFormat]: JsonFormat[MutableList[T]] =
        new JsonFormat[MutableList[T]]:
          def write(self: MutableList[T]) = JsArray(self.map(_.toJson).toVector)

          def read(value: JsValue): MutableList[T] = value match
            case JsArray(elements) => MutableList.from(elements.map(_.convertTo[T]))
            case _ => ???

      implicit def HashMapFormat[K : JsonFormat, V : JsonFormat]: JsonFormat[HashMap[K, V]] =
        new JsonFormat[HashMap[K, V]]:
          def write(self: HashMap[K, V]) = JsArray {
            self.map { (k, v) =>
              JsArray(
                JsObject("key" -> k.toJson),
                JsObject("value" -> v.toJson)
              )
            }.toVector
          }

          def read(value: JsValue): HashMap[K, V] = value match
            case JsArray(elements) =>
              HashMap.from {
                elements
                  .map {
                    case JsArray(Vector(JsObject(key), JsObject(value))) =>
                      key("key").convertTo[K] -> value("value").convertTo[V]
                    case _ => ???
                  }
              }
            case _ => ???

      implicit def HashSetFormat[T : JsonFormat]: JsonFormat[HashSet[T]] =
        new JsonFormat[HashSet[T]]:
          def write(self: HashSet[T]) = JsArray(self.map(_.toJson).toVector)

          def read(value: JsValue): HashSet[T] = value match
            case JsArray(elements) => HashSet.from(elements.map(_.convertTo[T]))
            case _ => ???

      implicit def StringBuilderFormat: JsonFormat[StringBuilder] =
        new JsonFormat[StringBuilder]:
          def write(self: StringBuilder) = JsString(self.result())

          def read(value: JsValue): StringBuilder = value match
            case it: JsString => StringBuilder(it.value)
            case _ => ???

    object MutableJsonProtocol extends MutableJsonProtocol

////////////////////////////////////////////////////////////////////////////////

    trait MapJsonProtocol extends GreenLeafJsonProtocol:

      import spray.json.{ JsArray, JsObject, JsString, JsValue }
      import spray.json.enrichAny

      implicit def MapFormat[K : JsonFormat, V : JsonFormat]: JsonFormat[Map[K, V]] =
        new JsonFormat[Map[K, V]]:
          def write(self: Map[K, V]) = JsArray {
            self.map { (k, v) =>
              JsArray(
                JsObject("key" -> k.toJson),
                JsObject("value" -> v.toJson)
              )
            }.toVector
          }

          def read(value: JsValue): Map[K, V] = value match
            case JsArray(elements) =>
              Map.from {
                elements
                  .map {
                    case JsArray(Vector(JsObject(key), JsObject(value))) =>
                      key("key").convertTo[K] -> value("value").convertTo[V]
                    case _ => ???
                  }
              }
            case _ => ???

    object MapJsonProtocol extends MapJsonProtocol

  object http4s:

    import scala.collection.MapFactory
    import scala.collection.mutable.{ HashMap, HashSet }
    import scala.collection.mutable.{ ListBuffer => MutableList, StringBuilder }

    import cats.effect.IO

    import io.circe.{ Decoder, Encoder, KeyDecoder, KeyEncoder }

    import org.http4s.circe.{ jsonEncoderOf, jsonOf }
    import org.http4s.{ EntityDecoder, EntityEncoder }

    given [K: KeyDecoder, V: Decoder]: Decoder[HashMap[K, V]] =
      Decoder.decodeMapLike[K, V, HashMap](summon[KeyDecoder[K]],
                                           summon[Decoder[V]],
                                           MapFactory.toFactory[K, V, HashMap](HashMap))
    given [K: KeyEncoder, V: Encoder]: Encoder[HashMap[K, V]] =
      Encoder.encodeMapLike[K, V, HashMap](summon[KeyEncoder[K]],
                                           summon[Encoder[V]],
                                           Iterable.from)

    given [T: Decoder]: Decoder[HashSet[T]] =
      Decoder.decodeSet.map(HashSet.from(_))
    given [T: Encoder]: Encoder[HashSet[T]] =
      Encoder.encodeSet.contramap(Set.from(_))

    given [T: Decoder]: Decoder[MutableList[T]] =
      Decoder.decodeList.map(MutableList.from(_))
    given [T: Encoder]: Encoder[MutableList[T]] =
      Encoder.encodeList.contramap(List.from(_))

    given Decoder[StringBuilder] = Decoder.decodeString.map(StringBuilder(_))
    given Encoder[StringBuilder] = Encoder.encodeString.contramap(_.toString)

    given [K: KeyDecoder, V: Decoder]: EntityDecoder[IO, HashMap[K, V]] = jsonOf
    given [K: KeyEncoder, V: Encoder]: EntityEncoder[IO, HashMap[K, V]] = jsonEncoderOf

    given [T: Decoder]: EntityDecoder[IO, HashSet[T]] = jsonOf
    given [T: Encoder]: EntityEncoder[IO, HashSet[T]] = jsonEncoderOf

    given [T: Decoder]: EntityDecoder[IO, MutableList[T]] = jsonOf
    given [T: Encoder]: EntityEncoder[IO, MutableList[T]] = jsonEncoderOf

    given EntityDecoder[IO, StringBuilder] = jsonOf
    given EntityEncoder[IO, StringBuilder] = jsonEncoderOf


package object common:

  import scala.Option.when

  extension(self: Boolean)
    inline def :-[R](expr: => R): Option[R] = when(self)(expr)

  extension[T](self: Seq[T])
    inline def pairwise: Seq[(T, T)] = self zip (self.tail :+ self.head)

  import scala.collection.mutable.{ HashMap, HashSet }

  import scala.collection.{ Map => AnyMap, Set => AnySet }

  extension[K, V](self: HashMap[K, HashSet[V]])
    def +++(that: AnyMap[K, AnySet[V]]): HashMap[K, HashSet[V]] =
      (self.keySet & that.keySet).map { it => self(it) ++= that(it) }
      (that.keySet -- self.keySet).map { it => self(it) = HashSet.from(that(it)) }
      self

  extension[K, V](self: AnyMap[K, AnySet[V]])
    def ---(that: AnyMap[K, AnySet[V]]): AnyMap[K, AnySet[V]] =
      (self.keySet & that.keySet).map { it => it -> (self(it) -- that(it)) }.toMap ++
      (self.keySet -- that.keySet).map { it => it -> self(it) }.toMap
