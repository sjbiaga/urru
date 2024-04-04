package urru
package base

import scala.concurrent.ExecutionContext

import org.mongodb.scala.{ MongoClient, MongoDatabase }

import io.github.greenleafoss.mongo.GreenLeafMongoDao


abstract class BaseMongoDao[Id, E](
  name: String
)(using
  override protected val ec: ExecutionContext
) extends GreenLeafMongoDao[Id, E]:

  override protected val db: MongoDatabase =
    MongoClient("mongodb://bro:27017").getDatabase(s"urru-game-$name")
