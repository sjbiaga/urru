package urru
package common

import java.util.UUID

import scala.concurrent.Await
import scala.concurrent.duration.*

import cats.effect.IO

import org.mongodb.scala.*
import org.mongodb.scala.model.Filters.*

import fs2.Stream
import fs2.io.file.{ Files, Path }

import spray.json.*


final class Mongo(mongoClient: MongoClient, database: MongoDatabase):

  object load:

    def apply(id: String, coll: String): Seq[Document] =
      val collection = database.getCollection(coll)
      val observable = collection.find(equal("_id", id))
      Await.result(observable.toFuture(), 10.seconds)

  object save:

    def apply(json: JsValue, coll: String)(using JsonFormat[JsObject]): (JsValue, String) =
      import org.bson.types.ObjectId
      val collection = database.getCollection(coll)
      val jsonId = JsString(ObjectId().toString)
      var jsonObj = json.asJsObject
      jsonObj = jsonObj.copy(fields = jsonObj.fields.updated("_id", jsonId))
      val jsonValue = jsonObj.toJson
      val observable = collection.insertOne(Document(jsonValue.toString))
      val result = Await.result(observable.toFuture(), 10.seconds)
      jsonValue -> result.getInsertedId().asString().getValue()

  object file:

    def apply(json: JsValue, coll: String, name: String)(using JsonFormat[JsObject]): IO[Unit] =
      for
        cwd <- Files[IO].currentWorkingDirectory
        s = Stream.emit[IO, String] { save(json, coll)._1.prettyPrint }
        filename = Mongo.uuid(name)
        _ <- s.through(Files[IO].writeUtf8Lines(cwd / (filename + ".json"))).compile.drain
      yield
        ()

  object item:

    def apply(coll: String, sp: String, item: Int): Seq[Document] =
      val collection = database.getCollection(coll)
      val observable = collection.find(and(equal("savepoint", sp),
                                           equal("tree.parameter.item", item)))
      Await.result(observable.toFuture(), 10.seconds)


object Mongo:

  def uuid(pre: String): String =
    pre + "_" + UUID.randomUUID.toString.replaceAll("-", "_")

  def apply(config: Config.MongoConfig): Mongo =
    val mongoClient = MongoClient(s"mongodb://${config.host}:${config.port}")
    new Mongo(mongoClient, mongoClient.getDatabase(config.database))
