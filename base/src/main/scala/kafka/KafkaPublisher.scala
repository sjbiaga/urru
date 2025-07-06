package urru
package kafka

//https://github.com/alanjphillips/AccountServiceIO/blob/master/kafkaclient/src/main/scala/com/alaphi/accountservice/kafka/client/fs2/KafkaPublisher.scala

import java.util.Properties

import cats.effect.IO

import io.circe.Encoder

import org.apache.kafka.clients.producer.{ ProducerRecord, RecordMetadata, KafkaProducer => ApacheKafkaProducer }

import Serdes.serializer


trait KafkaPublisher[K, V]:
  def send(topic: String, key: K, value: V): IO[Option[RecordMetadata]]
  def send(record: ProducerRecord[K, V]): IO[Option[RecordMetadata]]


object KafkaPublisher:

  class PublishException extends base.BaseException("publish")

  def apply[K: Encoder, V: Encoder](props: Properties): KafkaPublisher[K, V] =
    new KafkaPublisher[K, V]:
      val underlyingProducer = new ApacheKafkaProducer[K, V](props, serializer[K], serializer[V])

      def send(topic: String, key: K, value: V): IO[Option[RecordMetadata]] =
        send(new ProducerRecord[K, V](topic, key, value))

      def send(record: ProducerRecord[K, V]): IO[Option[RecordMetadata]] =
        IO.async_ { cb =>
          underlyingProducer.send(record,
                                  { (metadata: RecordMetadata, exception: Exception) =>
                                    cb(
                                      Option(metadata)
                                        .toRight(Option(exception).getOrElse(new PublishException))
                                        .map(meta => Some(meta))
                                    )
                                  }
          )
        }

