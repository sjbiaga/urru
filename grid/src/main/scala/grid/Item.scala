package urru
package grid

import scala.collection.mutable.{ ListBuffer => MutableList }


case class Item[
  T,
  B <: Path[?, ?, ?, ?, ?, ?, ?]
](
  var play: T,
  var over: Boolean,
  val path: MutableList[B] = MutableList[B]()
)
