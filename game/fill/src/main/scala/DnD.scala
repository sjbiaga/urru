package urru
package game
package fill

import scala.annotation.tailrec

import common.grid.{ row, col }

import Clue.*

import tense.intensional.Data.Doubt

import tense.intensional.Data.DragAndDrop

import urru.grid.Game.Feature

import Game.*


object DnD:

  // drag'n'drop /////////////////////////////////////////////////////////////////

  extension (self: Game)

    def dragOut: self.type =
      if !self.nowPlay.pad
      then
        val i = -self.nowPlay.color-1

        val it = Move(self.nowPlay.block, false, empty = true)

        if !self.features(Feature.Just)
        then
          self.move(it)(Doubt(Set.empty))
        else
          self.move(it)(Doubt(Set(DragAndDrop(it))))

        if self.pending.exists { (j, m) => j == i || m.contains(i) }
        then
          self.pending.clear

        self(-i-1)

      else
        self

    def dragOff(elapsed: Long): Boolean =
      val it = self.nowPlay
      val i = -it.color-1
      val in = self()(it)

      if self(it, in)(elapsed)
      then
        self(-i-1)

        self.selectionMode = 0

        true
      else
        false

    def dragOn: self.type = dragOn(true, false)

    @tailrec
    private def dragOn(continue: Boolean, matching: Boolean): self.type =
      if self.nowPlay.block.max.row > self.size.row
      then
        return dropOut

      else if !continue
      then
        if self.nowPlay.block(self.size, (1, 1), self.clues, self.grid)
        then
          return self
        else if !matching
        then
          return dragOn(true, true)

      if self.selectionMode % 2 == 0
      then

        if self.nowPlay.block.min.col < 1
        then
          val right = self.nowPlay.block(self.size, (0, 1), force = true).get
          self.nowPlay = Move(right)
          val down = self.nowPlay.block(self.size, (1, 0), force = true).get
          self.nowPlay = Move(down)

          self.selectionMode += 1
          dragOn(false, true)

        else if !matching && self.nowPlay.block(self.size, (0, -1), self.clues, self.grid)
        then
          val left = self.nowPlay.block(self.size, (0, -1)).get
          self.nowPlay = Move(left)
          self

        else
          val left = self.nowPlay.block(self.size, (0, -1), force = true).get
          self.nowPlay = Move(left)

          dragOn(false, matching)

      else // self.selectionMode % 2 == 1

        if self.nowPlay.block.max.col > self.size.col
        then
          val left = self.nowPlay.block(self.size, (0, -1), force = true).get
          self.nowPlay = Move(left)
          val down = self.nowPlay.block(self.size, (1, 0), force = true).get
          self.nowPlay = Move(down)

          self.selectionMode += 1
          dragOn(false, true)

        else if !matching && self.nowPlay.block(self.size, (0, 1), self.clues, self.grid)
        then
          val right = self.nowPlay.block(self.size, (0, 1)).get
          self.nowPlay = Move(right)
          self

        else
          val right = self.nowPlay.block(self.size, (0, 1), force = true).get
          self.nowPlay = Move(right)

          dragOn(false, matching)

    def dropOut: self.type =
      val i = -self.nowPlay.color-1

      self.selectionMode = 0

      self(-i-1)

    def dropIn(mode: Int): self.type =
      if self.nowPlay.pad
      then
        self.selectionMode = mode

        val i = -self.nowPlay.color-1
        self.nowPlay = self.state(i).play.last

        if mode > 0
        then
          dragOn(false, false)

      self

    // droppable w/o grid
    def dropOn(ps: Point*): Boolean =
      ps.forall { case pt @ (row, col) =>
        1 <= row && row <= self.size.row &&
        1 <= col && col <= self.size.col &&
        !self.clues.exists {
          case Empty(`pt`) => true
          case _ => false
        }
      }
