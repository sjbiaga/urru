package object urru:

  type Point = common.grid.Point


  import urru.game.fold.Clue.Block

  type Play = List[Block]

  type Fold = grid.Item[Play, game.fold.Path]
