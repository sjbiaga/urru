package urru
package game
package flow
package util

import io.github.greenleafoss.mongo.GreenLeafJsonProtocol
import spray.json.{ JsonFormat, RootJsonFormat }

import common.Mutable

import Clue._
import UndoRedo._

import tense.intensional.Data._


object JsonFormats:

  trait CellJsonProtocol extends GreenLeafJsonProtocol:
    import common.Implicits.MutableJsonProtocol._
    import ClueJsonProtocol._

    implicit lazy val CellFormat: JsonFormat[Cell] =
      jsonFormat(Cell.apply, "colors", "clue")

  object CellJsonProtocol extends CellJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait ClueJsonProtocol extends GreenLeafJsonProtocol:
    import spray.json.{ JsObject, JsValue }
    import StartJsonProtocol._
    import EmptyJsonProtocol._
    import CrossJsonProtocol._
    import StripJsonProtocol._
    import FrameJsonProtocol._
    import TrackJsonProtocol._

    implicit def ClueFormat: JsonFormat[Clue] =
      new JsonFormat[Clue]:
        def write(self: Clue) = self match
          case it: Start => JsObject("Start" -> StartFormat.write(it))
          case it: Empty => JsObject("Empty" -> EmptyFormat.write(it))
          case it: Cross => JsObject("Cross" -> CrossFormat.write(it))
          case it: Strip => JsObject("Strip" -> StripFormat.write(it))
          case it: Frame => JsObject("Frame" -> FrameFormat.write(it))
          case it: Track => JsObject("Track" -> TrackFormat.write(it))

        def read(value: JsValue): Clue = value match
          case JsObject(fields) if fields.size == 1 => fields.head match
            case ("Start", it) => StartFormat.read(it)
            case ("Empty", it) => EmptyFormat.read(it)
            case ("Cross", it) => CrossFormat.read(it)
            case ("Strip", it) => StripFormat.read(it)
            case ("Frame", it) => FrameFormat.read(it)
            case ("Track", it) => TrackFormat.read(it)
            case _ => ???
          case _ => ???

  object ClueJsonProtocol extends ClueJsonProtocol

  trait StartJsonProtocol extends GreenLeafJsonProtocol:
    implicit lazy val StartFormat: JsonFormat[Start] = jsonFormat3(Start.apply)

  object StartJsonProtocol extends StartJsonProtocol

  trait EmptyJsonProtocol extends GreenLeafJsonProtocol:
    implicit lazy val EmptyFormat: JsonFormat[Empty] = jsonFormat1(Empty.apply)

  object EmptyJsonProtocol extends EmptyJsonProtocol

  trait CrossJsonProtocol extends GreenLeafJsonProtocol:
    implicit lazy val CrossFormat: JsonFormat[Cross] = jsonFormat1(Cross.apply)

  object CrossJsonProtocol extends CrossJsonProtocol

  trait StripJsonProtocol extends GreenLeafJsonProtocol:
    implicit lazy val StripFormat: JsonFormat[Strip] = jsonFormat2(Strip.apply)

  object StripJsonProtocol extends StripJsonProtocol

  trait FrameJsonProtocol extends GreenLeafJsonProtocol:
    import StripJsonProtocol._

    private val apply = Frame.apply(_*)

    implicit lazy val FrameFormat: JsonFormat[Frame] = jsonFormat(apply, "border")

  object FrameJsonProtocol extends FrameJsonProtocol

  trait TrackJsonProtocol extends GreenLeafJsonProtocol:
    import common.Implicits.MapJsonProtocol._
    import StripJsonProtocol._

    private val apply = Track.apply(_, _*)

    implicit lazy val TrackFormat: JsonFormat[Track] = jsonFormat(apply, "sides", "path")

  object TrackJsonProtocol extends TrackJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait DoubtJsonProtocol extends GreenLeafJsonProtocol:
    import DataJsonProtocol._

    implicit lazy val DoubtFormat: JsonFormat[Doubt] = jsonFormat1(Doubt.apply)

  object DoubtJsonProtocol extends DoubtJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait DataJsonProtocol extends GreenLeafJsonProtocol:
    import spray.json.{ JsObject, JsValue }
    import BacktrackJsonProtocol._
    import HalfCrossJsonProtocol._
    import FullCrossJsonProtocol._
    import BacklashJsonProtocol._
    import PulloutJsonProtocol._

    import tense.intensional.Data

    implicit def DataFormat: JsonFormat[Data[?]] =
      new JsonFormat[Data[?]]:
        def write(self: Data[?]) = self match
          case it: Backtrack => JsObject("Backtrack" -> BacktrackFormat.write(it))
          case it: HalfCross => JsObject("HalfCross" -> HalfCrossFormat.write(it))
          case it: FullCross => JsObject("FullCross" -> FullCrossFormat.write(it))
          case it: Backlash => JsObject("Backlash" -> BacklashFormat.write(it))
          case it: Pullout => JsObject("Pullout" -> PulloutFormat.write(it))

        def read(value: JsValue): Data[?] = value match
          case JsObject(fields) if fields.size == 1 => fields.head match
            case ("Backtrack", it) => BacktrackFormat.read(it)
            case ("HalfCross", it) => HalfCrossFormat.read(it)
            case ("FullCross", it) => FullCrossFormat.read(it)
            case ("Backlash", it) => BacklashFormat.read(it)
            case ("Pullout", it) => PulloutFormat.read(it)
            case _ => ???
          case _ => ???

  object DataJsonProtocol extends DataJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait BacktrackJsonProtocol extends GreenLeafJsonProtocol:
    import MoveJsonProtocol._

    implicit lazy val BacktrackFormat: JsonFormat[Backtrack] = jsonFormat(Backtrack.apply, "move", "at", "over")

  object BacktrackJsonProtocol extends BacktrackJsonProtocol

  trait HalfCrossJsonProtocol extends GreenLeafJsonProtocol:
    import MoveJsonProtocol._
    import CrossJsonProtocol._

    implicit lazy val HalfCrossFormat: JsonFormat[HalfCross] = jsonFormat(HalfCross.apply, "move", "clue")

  object HalfCrossJsonProtocol extends HalfCrossJsonProtocol

  trait FullCrossJsonProtocol extends GreenLeafJsonProtocol:
    import MoveJsonProtocol._
    import CrossJsonProtocol._

    implicit lazy val FullCrossFormat: JsonFormat[FullCross] = jsonFormat(FullCross.apply, "move", "clue", "pair")

  object FullCrossJsonProtocol extends FullCrossJsonProtocol

  trait BacklashJsonProtocol extends GreenLeafJsonProtocol:
    import MoveJsonProtocol._

    implicit lazy val BacklashFormat: JsonFormat[Backlash] = jsonFormat(Backlash.apply, "move", "at", "pair", "over")

  object BacklashJsonProtocol extends BacklashJsonProtocol

  trait PulloutJsonProtocol extends GreenLeafJsonProtocol:
    import MoveJsonProtocol._
    import TrackJsonProtocol._

    implicit lazy val PulloutFormat: JsonFormat[Pullout] = jsonFormat4(Pullout.apply)

  object PulloutJsonProtocol extends PulloutJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait FlowJsonProtocol extends GreenLeafJsonProtocol:
    import scala.collection.mutable.{ ListBuffer => MutableList }
    import common.Implicits.MutableJsonProtocol._
    import common.Mutable.MutableJsonProtocol._
    import PathJsonProtocol._

    implicit lazy val FlowFormat: JsonFormat[Flow] =
      jsonFormat3[Play, Boolean, MutableList[game.flow.Path], Flow](grid.Item.apply)

  object FlowJsonProtocol extends FlowJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait GameJsonProtocol extends GreenLeafJsonProtocol:
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

    implicit lazy val GameFormat: RootJsonFormat[Game] =
      jsonFormat(Game.apply,
                 "id",
                 "size", "grid", "clues", "features",
                 "state", "hints", "counters",
                 "pending", "batch",
                 "nowStart",
                 "showAxes", "showJust", "gameOver",
                 "startTime", "minusTime")

  object GameJsonProtocol extends GameJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait MoveJsonProtocol extends GreenLeafJsonProtocol:
    implicit lazy val MoveFormat: JsonFormat[Move] =
      jsonFormat(Move.apply,
                 "odd",
                 "at", "by", "color",
                 "elapsed")

  object MoveJsonProtocol extends MoveJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait PathJsonProtocol extends GreenLeafJsonProtocol:
    import common.Implicits.MutableJsonProtocol._
    import urru.grid.Implicits.IdJsonProtocol._
    import UndoJsonProtocol._
    import RedoJsonProtocol._
    import JustJsonProtocol._
    import HaveJsonProtocol._

    implicit lazy val PathFormat: JsonFormat[Path] =
      lazyFormat(jsonFormat(Path.apply,
                            "dual", "number",
                            "depth", "nesting",
                            "replica", "parent",
                            "undo", "redo",
                            "just", "have",
                            "pisc"))

  object PathJsonProtocol extends PathJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  import grid.Tense.Have
  import flow.Have.Board

  trait HaveJsonProtocol extends GreenLeafJsonProtocol:
    import spray.json.{ JsObject, JsValue }
    import BoardJsonProtocol._

    implicit def HaveFormat: RootJsonFormat[Have[Clue, Cell, Move]] =
      new RootJsonFormat[Have[Clue, Cell, Move]]:
        def write(self: Have[Clue, Cell, Move]) = self match
          case it: Board => JsObject("Board" -> BoardFormat.write(it))
          case _ => ???

        def read(value: JsValue): Have[Clue, Cell, Move] = value match
          case JsObject(fields) if fields.size == 1 => fields.head match
            case ("Board", it) => BoardFormat.read(it)
            case _ => ???
          case _ => ???

  object HaveJsonProtocol extends HaveJsonProtocol

  trait BoardJsonProtocol extends GreenLeafJsonProtocol:
    import common.Implicits.MutableJsonProtocol._
    import urru.grid.Implicits.IdJsonProtocol._
    import CellJsonProtocol._
    import ClueJsonProtocol._

    implicit lazy val BoardFormat: JsonFormat[Board] = jsonFormat4(Board.apply)

  object BoardJsonProtocol extends BoardJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  import grid.Tense.Just

  trait JustJsonProtocol extends GreenLeafJsonProtocol:
    import spray.json.{ JsObject, JsValue }
    import UndoJsonProtocol._
    import RedoJsonProtocol._

    implicit def JustFormat: RootJsonFormat[Just[Doubt, Clue, Cell, Move]] =
      new RootJsonFormat[Just[Doubt, Clue, Cell, Move]]:
        def write(self: Just[Doubt, Clue, Cell, Move]) = self match
          case it: Undo => JsObject("Undo" -> UndoFormat.write(it))
          case it: Redo => JsObject("Redo" -> RedoFormat.write(it))

        def read(value: JsValue): Just[Doubt, Clue, Cell, Move] = value match
          case JsObject(fields) if fields.size == 1 => fields.head match
            case ("Undo", it) => UndoFormat.read(it)
            case ("Redo", it) => RedoFormat.read(it)
            case _ => ???
          case _ => ???

  object JustJsonProtocol extends JustJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait UndoJsonProtocol extends GreenLeafJsonProtocol:
    import urru.grid.Implicits.IdJsonProtocol._
    import MoveJsonProtocol._
    import Mutable.MutableJsonProtocol._
    import DoubtJsonProtocol._
    import PathJsonProtocol._

    implicit lazy val UndoFormat: RootJsonFormat[Undo] =
      rootFormat(lazyFormat(jsonFormat(Undo.apply,
                                       "id",
                                       "move", "intensity",
                                       "next", "path",
                                       "number", "identifier",
                                       "elapsed")))

  object UndoJsonProtocol extends UndoJsonProtocol

  trait RedoJsonProtocol extends GreenLeafJsonProtocol:
    import urru.grid.Implicits.IdJsonProtocol._
    import UndoJsonProtocol._
    import Mutable.MutableJsonProtocol._
    import PathJsonProtocol._

    implicit lazy val RedoFormat: RootJsonFormat[Redo] =
      rootFormat(lazyFormat(jsonFormat(Redo.apply,
                                       "id",
                                       "undo",
                                       "next", "path",
                                       "identifier")))

  object RedoJsonProtocol extends RedoJsonProtocol
