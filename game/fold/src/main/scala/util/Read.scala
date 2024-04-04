package urru
package game
package fold
package util

import java.io.{ FileReader, BufferedReader }

import scala.collection.mutable.HashSet

import cats.effect.IO
import fs2.io.file.{ Files, Path }

import common.grid.{ row, col }
import common.pairwise
import common.Mutable
import Mutable.given

import Clue._


object Read:

  def apply(filename: String): IO[(Point, Set[Clue])] =
    //          size                          c[olor]      r[esult]
    val init = (Mutable(Option.empty[Point]), Mutable(-1), HashSet[Clue]())

    Files[IO]
      .readUtf8Lines(Path(filename))
      .map { line =>
        val List(clue, ls*) = (if line.isBlank then "#" else line)
          .split(" ").toList
          .filterNot(_.isBlank)
          .toList
        clue -> ls
      }
      .filterNot { _._1 match
        case "#" | "//" | "--" => true
        case _ => false
      }
      .compile
      .fold(init) { case ((size, c, r), (clue, ls)) =>
        def valid(it: Point) = 1 <= it.row && it.row <= size.get.row && 1 <= it.col && it.col <= size.get.col

        val cs = ls.map(_.toInt).toSeq
        val ps = Seq.from(cs.pairwise.map(_ -> _).sliding(1, 2)).map(_.head)
        val ps2 = ps.toSet

        // println(clue)
        // println(ps)

        def unique = !r.exists {
          case Block(_, _, _, block*) => (ps2 & block.toSet).nonEmpty
          case Empty(at) => ps.contains(at)
          case Multi(at) => ps.contains(at)
        }

        clue.toUpperCase match

          case "BOARD" => if size.nonEmpty then ???

          case _ => if size.isEmpty then ???

        clue.toUpperCase match

          case "BOARD"
              if ps.size == 1 =>

            require(1 <= ps(0).row && 1 <= ps(0).col)

            size ::= Some(ps(0))


          case "BLOCK"
              if ps.nonEmpty
              && r.forall {
                case Block(_, _, _, block*) =>
                  (ps2 & block.toSet).forall { pt =>
                    r.exists {
                      case Multi(`pt`) => true
                      case _ => false
                    }
                  }
                case Empty(at) => !ps.contains(at)
                case _ => true
              } =>

            require(ps.forall(valid(_)))

            r.add(new Block(c, ps*))
            c ::= c - 1


          case "EMPTY"
              if ps.nonEmpty
              && unique =>

            require(ps.forall(valid(_)))

            ps.foreach { at => r.add(Empty(at)) }


          case "MULTI"
              if ps.nonEmpty
              && !r.exists {
                case Empty(at) => ps.contains(at)
                case Multi(at) => ps.contains(at)
                case _ => false
              } =>

            require(ps.forall(valid(_)))

            ps.foreach { at => r.add(Multi(at)) }


          case _ => ???

        (size, c, r)
      }
      .map { (size, _, r) => size.get -> r.toSet }
      // .map { (size, _, r) =>

      //   r.foreach {
      //     case Block(min, max, color, block*) =>
      //       println(s"Block($block, $min, $max, $color)")

      //     case Empty(p, _*) =>
      //       println(s"Empty($p)")

      //     case Multi(at) =>
      //       println(s"Multi($at)")
      //   }

      //   size.get -> r.toSet

      // }
