package object urru:

  type Point = common.grid.Point


  import urru.game.fill.Clue.Block

  type Play = List[Block]

  type Fall = grid.Item[Play, game.fill.Path]
