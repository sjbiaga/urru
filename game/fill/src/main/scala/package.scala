package object urru:

  type Point = common.grid.Point


  import urru.game.fill.Move

  type Play = Move

  extension(self: Play)
    inline def pad = self.block.block.isEmpty
    inline def block = self.block
    inline def color = self.color

  type Fill = grid.Item[Seq[Play], game.fill.Path]
