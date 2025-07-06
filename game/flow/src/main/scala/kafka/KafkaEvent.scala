package urru
package game
package flow
package kafka

import grid.Grid.Id
import grid.Game.Feature


export KafkaEvent.*

enum KafkaEvent:
  case Made(id: Id, size: Point, clues: Set[Clue], feats: Feature*)
  case Picked(color: Int)
  case Moved(dir: (Int, Int), count: Int, elapsed: Long)
  case Undone(count: Int, elapsed: Long)
  case Redone(elapsed: Long)
  case Toggled
  case Paused(value: Boolean)
  case Exited
