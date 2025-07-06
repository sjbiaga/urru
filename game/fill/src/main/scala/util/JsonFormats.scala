package urru
package game
package fill
package util

import io.github.greenleafoss.mongo.GreenLeafJsonProtocol
import spray.json.{ JsonFormat, RootJsonFormat }

import common.Mutable

import Clue.*
import UndoRedo.*

import tense.intensional.Data.*


object JsonFormats:

  trait CellJsonProtocol extends GreenLeafJsonProtocol:
    import common.Implicits.MutableJsonProtocol.*
    import ClueJsonProtocol.*

    implicit lazy val CellFormat: JsonFormat[Cell] =
      jsonFormat(Cell.apply, "colors", "clue")

  object CellJsonProtocol extends CellJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait ClueJsonProtocol extends GreenLeafJsonProtocol:
    import spray.json.{ JsObject, JsValue }
    import BlockJsonProtocol.*
    import EmptyJsonProtocol.*
    import MultiJsonProtocol.*

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
    private val apply = Block.apply(_, _, _, _, _, _, _, _, _*)

    implicit lazy val BlockFormat: JsonFormat[Block] =
      jsonFormat(apply, "delta", "min", "max", "up", "down", "left", "right", "color", "block")

  object BlockJsonProtocol extends BlockJsonProtocol

  trait EmptyJsonProtocol extends GreenLeafJsonProtocol:
    implicit lazy val EmptyFormat: JsonFormat[Empty] = jsonFormat1(Empty.apply)

  object EmptyJsonProtocol extends EmptyJsonProtocol

  trait MultiJsonProtocol extends GreenLeafJsonProtocol:
    implicit lazy val MultiFormat: JsonFormat[Multi] = jsonFormat1(Multi.apply)

  object MultiJsonProtocol extends MultiJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait DoubtJsonProtocol extends GreenLeafJsonProtocol:
    import DataJsonProtocol.*

    implicit lazy val DoubtFormat: JsonFormat[Doubt] = jsonFormat1(Doubt.apply)

  object DoubtJsonProtocol extends DoubtJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait DataJsonProtocol extends GreenLeafJsonProtocol:
    import spray.json.{ JsObject, JsValue }
    import BacktrackJsonProtocol.*
    import BacklashJsonProtocol.*

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
    import MoveJsonProtocol.*

    implicit lazy val BacktrackFormat: JsonFormat[Backtrack] = jsonFormat(Backtrack.apply, "move")

  object BacktrackJsonProtocol extends BacktrackJsonProtocol

  trait BacklashJsonProtocol extends GreenLeafJsonProtocol:
    import common.Implicits.MapJsonProtocol.*
    import MoveJsonProtocol.*

    implicit lazy val BacklashFormat: JsonFormat[Backlash] = jsonFormat(Backlash.apply, "move", "pair")

  object BacklashJsonProtocol extends BacklashJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait FillJsonProtocol extends GreenLeafJsonProtocol:
    import scala.collection.mutable.{ ListBuffer => MutableList }
    import common.Implicits.MutableJsonProtocol.*
    import common.Mutable.MutableJsonProtocol.*
    import PathJsonProtocol.*
    import MoveJsonProtocol.*

    implicit lazy val FillFormat: JsonFormat[Fill] =
      jsonFormat3[Seq[Play], Boolean, MutableList[game.fill.Path], Fill](grid.Item.apply)

  object FillJsonProtocol extends FillJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait GameJsonProtocol extends GreenLeafJsonProtocol:
    import common.Implicits.MapJsonProtocol.*
    import common.Implicits.MutableJsonProtocol.*
    import urru.grid.Implicits.IdJsonProtocol.*
    import urru.grid.Implicits.CountersJsonProtocol.*
    import urru.grid.Implicits.FeatureJsonProtocol.*
    import urru.grid.Implicits.SavepointJsonProtocol.*
    import Mutable.MutableJsonProtocol.*
    import CellJsonProtocol.*
    import ClueJsonProtocol.*
    import FillJsonProtocol.*
    import MoveJsonProtocol.*

    implicit lazy val GameFormat: RootJsonFormat[Game] =
      jsonFormat(Game.apply,
                 "id",
                 "size", "grid", "clues", "features",
                 "state", "init", "hints", "counters", "savepoint",
                 "pending", "batch",
                 "selectionMode", "nowPlay",
                 "showAxes", "showPad", "gameOver",
                 "startTime")

  object GameJsonProtocol extends GameJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait MoveJsonProtocol extends GreenLeafJsonProtocol:
    import BlockJsonProtocol.*

    implicit lazy val MoveFormat: JsonFormat[Move] =
      jsonFormat(Move.apply, "dir", "block", "color", "elapsed")

  object MoveJsonProtocol extends MoveJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait PathJsonProtocol extends GreenLeafJsonProtocol:
    import common.Implicits.MutableJsonProtocol.*
    import urru.grid.Implicits.IdJsonProtocol.*
    import UndoJsonProtocol.*
    import RedoJsonProtocol.*
    import JustJsonProtocol.*
    import HaveJsonProtocol.*

    implicit lazy val PathFormat: JsonFormat[Path] =
      lazyFormat(jsonFormat(Path.apply,
                            "dual", "number",
                            "depth", "nesting",
                            "replica", "parent",
                            "undo", "redo",
                            "just", "have"))

  object PathJsonProtocol extends PathJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  import grid.Tense.Have
  import fill.Have.Board

  trait HaveJsonProtocol extends GreenLeafJsonProtocol:
    import spray.json.{ JsObject, JsValue }
    import BoardJsonProtocol.*

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
    import UndoJsonProtocol.*
    import RedoJsonProtocol.*

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
    import common.Implicits.MutableJsonProtocol.*
    import urru.grid.Implicits.IdJsonProtocol.*
    import CellJsonProtocol.*
    import ClueJsonProtocol.*

    implicit lazy val BoardFormat: JsonFormat[Board] = jsonFormat4(Board.apply)

  object BoardJsonProtocol extends BoardJsonProtocol

////////////////////////////////////////////////////////////////////////////////

  trait UndoJsonProtocol extends GreenLeafJsonProtocol:
    import urru.grid.Implicits.IdJsonProtocol.*
    import MoveJsonProtocol.*
    import Mutable.MutableJsonProtocol.*
    import DoubtJsonProtocol.*
    import PathJsonProtocol.*

    implicit lazy val UndoFormat: RootJsonFormat[Undo] =
      rootFormat(lazyFormat(jsonFormat(Undo.apply,
                                       "id",
                                       "move", "intensity",
                                       "next", "path",
                                       "number", "identifier",
                                       "elapsed")))

  object UndoJsonProtocol extends UndoJsonProtocol

  trait RedoJsonProtocol extends GreenLeafJsonProtocol:
    import urru.grid.Implicits.IdJsonProtocol.*
    import UndoJsonProtocol.*
    import Mutable.MutableJsonProtocol.*
    import PathJsonProtocol.*

    implicit lazy val RedoFormat: RootJsonFormat[Redo] =
      rootFormat(lazyFormat(jsonFormat(Redo.apply,
                                       "id",
                                       "undo",
                                       "next", "path",
                                       "identifier",
                                       "elapsed")))

  object RedoJsonProtocol extends RedoJsonProtocol
