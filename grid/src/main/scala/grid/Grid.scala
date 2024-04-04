package urru
package grid


abstract trait Grid[C, K, V, F[_], W[_, _]]:
  val id: Grid.Id
  def grid: W[K, F[V]]
  def clues: Set[C]


object Grid:

  import java.time.ZonedDateTime

  case class Id(number: Long, timestamp: ZonedDateTime = ZonedDateTime.now)

////////////////////////////////////////////////////////////////////////////////

  abstract trait GridOps[M, R]:
    def move(it: M): R

  abstract trait PathOps[P]:
    def undo(item: P): Unit
    def redo(item: P): Unit
