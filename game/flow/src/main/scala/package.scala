package object urru:

  type Point = common.grid.Point


  type Play = Seq[Point]

  type Flow = grid.Item[Play, game.flow.Path]


  import game.flow.{ Game, Path, UndoRedo, Move }
  import UndoRedo.*

  type Visitor[F[_], A >: Null] = base.Visitor[Game, Path, Undo, Redo, F, A]
