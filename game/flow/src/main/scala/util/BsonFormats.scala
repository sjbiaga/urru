package urru
package game
package flow
package util

import java.time.ZonedDateTime

import scala.concurrent.{ ExecutionContext, Future }

import org.mongodb.scala.MongoCollection
import org.mongodb.scala.bson.collection.immutable.Document

import io.github.greenleafoss.mongo.GreenLeafBsonProtocol
import io.github.greenleafoss.mongo.GreenLeafMongoDao.DaoBsonProtocol
import io.github.greenleafoss.mongo.ZonedDateTimeOps._
import spray.json.{ JsonFormat, RootJsonFormat }
import spray.json._

import common.Mutable

import Clue._
import UndoRedo._

import urru.grid.Grid.Id
import tense.intensional.Data._

import JsonFormats._


object BsonFormats:

  // class CellBsonProtocol
  //     extends CellJsonProtocol
  //     with GreenLeafBsonProtocol

  class UndoBsonProtocol extends UndoJsonProtocol
      with GreenLeafBsonProtocol
      with DaoBsonProtocol[Id, Undo]:

    import urru.grid.Implicits.IdJsonProtocol._
    import MoveJsonProtocol._
    import Mutable.MutableJsonProtocol._
    import DoubtJsonProtocol._
    import PathJsonProtocol._

    override implicit lazy val UndoFormat: RootJsonFormat[Undo] =
      rootFormat(lazyFormat(jsonFormat(Undo.apply,
                                       "_id",
                                       "move", "intensity",
                                       "next", "path",
                                       "number", "identifier",
                                       "elapsed")))

    override implicit val idFormat: JsonFormat[Id] = IdFormat

    override implicit val entityFormat: JsonFormat[Undo] = UndoFormat

////////////////////////////////////////////////////////////////////////////////

  class GameBsonProtocol extends GameJsonProtocol
      with GreenLeafBsonProtocol
      with DaoBsonProtocol[Id, Game]:

    import common.Implicits.MapJsonProtocol._
    import common.Implicits.MutableJsonProtocol._
    import urru.grid.Implicits.IdJsonProtocol._
    import urru.grid.Implicits.CountersJsonProtocol._
    import urru.grid.Implicits.FeatureJsonProtocol._
    import Mutable.MutableJsonProtocol._
    import CellJsonProtocol._
    import ClueJsonProtocol._
    import StartJsonProtocol._
    import FlowJsonProtocol._

    override implicit lazy val GameFormat: RootJsonFormat[Game] =
      jsonFormat(Game.apply,
                 "_id",
                 "size", "grid", "clues", "features",
                 "state", "hints", "counters",
                 "pending", "batch",
                 "nowStart", "showAxes", "showJust", "gameOver",
                 "startTime", "minusTime")

    override implicit val idFormat: JsonFormat[Id] = IdFormat

    override implicit val entityFormat: JsonFormat[Game] = GameFormat

  class GameDao(collectionName: String)(using ExecutionContext)
      extends base.BaseMongoDao[Id, Game]("flow"):

    override protected val collection: MongoCollection[Document] =
      db.getCollection(collectionName)

    override protected val protocol: GameBsonProtocol =
      GameBsonProtocol()

    import protocol._

    def findByDate(date: ZonedDateTime): Future[Seq[Game]] =
      find("_id.date" $eq date)
