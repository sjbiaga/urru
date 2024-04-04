package urru
package game
package fold
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
    import BlockJsonProtocol._
    import EmptyJsonProtocol._
    import MultiJsonProtocol._

    implicit def ClueFormat: JsonFormat[Clue] =
      new JsonFormat[Clue]:
        def write(self: Clue) = self match
          case it: Block => JsObject("Block" -> BlockFormat.write(it))
          case it: Empty => JsObject("Empty" -> EmptyFormat.write(it))
          case it: Multi => JsObject("Multi" -> MultiFormat.write(it))

        def read(value: JsValue): Clue = value match
          case JsObject(fields) if fields.size == 1 => fields.head match
            case ("Block", it) => BlockFormat.read(it)
            case ("Empty", it) => EmptyFormat.read(it)
            case ("Multi", it) => MultiFormat.read(it)
            case _ => ???
          case _ => ???

  object ClueJsonProtocol extends ClueJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait BlockJsonProtocol extends GreenLeafJsonProtocol:
    private val apply = Block.apply(_, _, _, _*)

    implicit lazy val BlockFormat: JsonFormat[Block] = jsonFormat(apply, "min", "max", "color", "block")

  object BlockJsonProtocol extends BlockJsonProtocol

  trait EmptyJsonProtocol extends GreenLeafJsonProtocol:
    implicit lazy val EmptyFormat: JsonFormat[Empty] = jsonFormat1(Empty.apply)

  object EmptyJsonProtocol extends EmptyJsonProtocol

  trait MultiJsonProtocol extends GreenLeafJsonProtocol:
    implicit lazy val MultiFormat: JsonFormat[Multi] = jsonFormat1(Multi.apply)

  object MultiJsonProtocol extends MultiJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait DoubtJsonProtocol extends GreenLeafJsonProtocol:
    import DataJsonProtocol._

    implicit lazy val DoubtFormat: JsonFormat[Doubt] = jsonFormat1(Doubt.apply)

  object DoubtJsonProtocol extends DoubtJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait DataJsonProtocol extends GreenLeafJsonProtocol:
    import spray.json.{ JsObject, JsValue }
    import BacktrackJsonProtocol._
    import BacklashJsonProtocol._

    import tense.intensional.Data

    implicit def DataFormat: JsonFormat[Data[?]] =
      new JsonFormat[Data[?]]:
        def write(self: Data[?]) = self match
          case it: Backtrack => JsObject("Backtrack" -> BacktrackFormat.write(it))
          case it: Backlash => JsObject("Backlash" -> BacklashFormat.write(it))

        def read(value: JsValue): Data[?] = value match
          case JsObject(fields) if fields.size == 1 => fields.head match
            case ("Backtrack", it) => BacktrackFormat.read(it)
            case ("Backlash", it) => BacklashFormat.read(it)
            case _ => ???
          case _ => ???

  object DataJsonProtocol extends DataJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait BacktrackJsonProtocol extends GreenLeafJsonProtocol:
    import MoveJsonProtocol._

    implicit lazy val BacktrackFormat: JsonFormat[Backtrack] = jsonFormat(Backtrack.apply, "move")

  object BacktrackJsonProtocol extends BacktrackJsonProtocol

  trait BacklashJsonProtocol extends GreenLeafJsonProtocol:
    import common.Implicits.MapJsonProtocol._
    import MoveJsonProtocol._

    implicit lazy val BacklashFormat: JsonFormat[Backlash] = jsonFormat(Backlash.apply, "move", "pair")

  object BacklashJsonProtocol extends BacklashJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait FoldJsonProtocol extends GreenLeafJsonProtocol:
    import scala.collection.mutable.{ ListBuffer => MutableList }
    import common.Implicits.MutableJsonProtocol._
    import common.Mutable.MutableJsonProtocol._
    import PathJsonProtocol._
    import BlockJsonProtocol._

    implicit lazy val FoldFormat: JsonFormat[Fold] =
      jsonFormat3[Play, Boolean, MutableList[game.fold.Path], Fold](grid.Item.apply)

  object FoldJsonProtocol extends FoldJsonProtocol

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
    import FoldJsonProtocol._

    implicit lazy val GameFormat: RootJsonFormat[Game] =
      jsonFormat(Game.apply,
                 "id",
                 "size", "grid", "clues",
                 "state", "hints", "counters", "features",
                 "pending", "batch",
                 "nowColor",
                 "showAxes", "showJust", "gameOver",
                 "startTime", "minusTime")

  object GameJsonProtocol extends GameJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait MoveJsonProtocol extends GreenLeafJsonProtocol:
    import BlockJsonProtocol._

    implicit lazy val MoveFormat: JsonFormat[Move] = jsonFormat4(Move.apply)

  object MoveJsonProtocol extends MoveJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait PathJsonProtocol extends GreenLeafJsonProtocol:
    import common.Implicits.MutableJsonProtocol._
    import UndoJsonProtocol._
    import RedoJsonProtocol._
    import JustJsonProtocol._
    import HaveJsonProtocol._
    import urru.grid.Implicits.IdJsonProtocol._

    implicit lazy val PathFormat: JsonFormat[Path] =
      lazyFormat(jsonFormat(Path.apply,
                            "dual", "id",
                            "depth", "nesting",
                            "replica", "parent",
                            "undo", "redo",
                            "just", "have",
                            "pisc"))

  object PathJsonProtocol extends PathJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  import grid.Tense.Have
  import fold.Have.Board

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

  trait BoardJsonProtocol extends GreenLeafJsonProtocol:
    import common.Implicits.MutableJsonProtocol._
    import urru.grid.Implicits.IdJsonProtocol._
    import CellJsonProtocol._
    import ClueJsonProtocol._

    implicit lazy val BoardFormat: JsonFormat[Board] = jsonFormat4(Board.apply)

  object BoardJsonProtocol extends BoardJsonProtocol

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
