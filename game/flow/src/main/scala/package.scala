package object urru:

  type Point = common.grid.Point


  type Play = Seq[Point]

  type Flow = grid.Item[Play, game.flow.Path]
