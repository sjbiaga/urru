package urru
package kafka

//https://github.com/alanjphillips/AccountServiceIO/blob/master/kafkaclient/src/main/scala/com/alaphi/accountservice/kafka/client/fs2/KafkaProperties.scala

import java.util.Properties

import org.apache.kafka.clients.consumer.ConsumerConfig


object KafkaProperties:

  val default = {
    val props = new Properties()
    props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092")
    props.put(ConsumerConfig.GROUP_ID_CONFIG, "gameflow")
    props.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "true")
    props.put(ConsumerConfig.AUTO_COMMIT_INTERVAL_MS_CONFIG, "1000")
    props.put(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, "300000")
    props.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")
    props
  }
