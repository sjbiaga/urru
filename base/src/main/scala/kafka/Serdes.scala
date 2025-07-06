package urru
package kafka

import cats.effect.IO

//https://github.com/alanjphillips/AccountServiceIO/blob/master/kafkaserdes/src/main/scala/com/alaphi/accountservice/kafka/serdes/circe/Serdes.scala

import java.nio.charset.StandardCharsets
import java.util

import io.circe.parser.*
import io.circe.syntax.*
import io.circe.{ Decoder, Encoder }

import fs2.kafka.GenericDeserializer

import org.apache.kafka.common.serialization.{ Deserializer, Serializer }


object Serdes:

  class SerdesException(t: Throwable) extends base.BaseException("serdes", t)

  def serializer[T: Encoder] =
    new Serializer[T]:
      override def configure(configs: util.Map[String, ?], isKey: Boolean): Unit = ()

      override def serialize(topic: String, data: T): Array[Byte] =
        data.asJson.noSpaces.getBytes(StandardCharsets.UTF_8)

      override def close(): Unit = ()

  def deserializer[T: Decoder] =
    GenericDeserializer.delegate[IO, T] {
      new Deserializer[T]:

        override def configure(configs: util.Map[String, ?], isKey: Boolean): Unit = ()

        override def deserialize(topic: String, data: Array[Byte]): T =
          Option(data).fold(null.asInstanceOf[T]) { bytes =>
            decode[T](new String(bytes))
              .fold(error => throw SerdesException(error), identity)
          }

        override def close(): Unit = ()
    }
