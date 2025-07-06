package urru
package game
package flow
package util

import io.github.greenleafoss.mongo.GreenLeafJsonProtocol
import spray.json.{ JsonFormat, RootJsonFormat }

import UndoRedo.*


object Versus:

  enum Dir:
    case UP, DOWN, LEFT, RIGHT

  object Dir:
    def apply(dir: (Int, Int)): Dir =
      dir match
        case (-1, 0) => UP
        case (1, 0) => DOWN
        case (0, -1) => LEFT
        case (0, 1) => RIGHT

    trait DirJsonProtocol extends GreenLeafJsonProtocol:
      import spray.json.{ JsString, JsValue }

      implicit def DirFormat: JsonFormat[Dir] =
        new JsonFormat[Dir]:
          def write(self: Dir) = JsString(self.toString)

          def read(value: JsValue): Dir = value match
            case it: JsString => Dir.valueOf(it.value)
            case _ => ???

    object DirJsonProtocol extends DirJsonProtocol

  case class Key(at: Point, dir: Dir)

  object Key:
    def apply(move: Move): Key =
      Key(move.at, Dir(move.dir))

    trait KeyJsonProtocol extends GreenLeafJsonProtocol:
      import Dir.DirJsonProtocol.*

      implicit lazy val KeyFormat: JsonFormat[Key] = jsonFormat2(Key.apply)

    object KeyJsonProtocol extends KeyJsonProtocol

  case class UR(ur: Boolean, key: Key, elapsed: Long)

  object UR:
    def apply(undo: Undo): UR =
      UR(true, Key(undo.move), undo.elapsed)
    def apply(redo: Redo): UR =
      UR(false, Key(redo.move), redo.elapsed)

    trait URJsonProtocol extends GreenLeafJsonProtocol:
      import Key.KeyJsonProtocol.*

      implicit lazy val URFormat: JsonFormat[UR] = jsonFormat3(UR.apply)

    object URJsonProtocol extends URJsonProtocol

  case class Parameter(item: Option[Int], ur: Option[UR])

  object Parameter:

    trait ParameterJsonProtocol extends GreenLeafJsonProtocol:
      import UR.URJsonProtocol.*

      implicit lazy val ParameterFormat: JsonFormat[Parameter] = jsonFormat2(Parameter.apply)

    object ParameterJsonProtocol extends ParameterJsonProtocol

  case class Data(override val depth: Int, parameter: Parameter) extends common.Tree.Validate.HasDepth

  object Data:

    trait DataJsonProtocol extends GreenLeafJsonProtocol:
      import Parameter.ParameterJsonProtocol.*

      implicit lazy val DataFormat: JsonFormat[Data] = jsonFormat2(Data.apply)

    object DataJsonProtocol extends DataJsonProtocol
