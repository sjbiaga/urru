package urru
package game
package flow
package util

import scala.collection.mutable.{ HashMap, HashSet, ListBuffer => MutableList }
import scala.Function.tupled

import cats.effect.IO
import fs2.io.file.{ Files, Path }

import common.grid.{ adj, row, x, col }
import common.pairwise
import common.Mutable
import Mutable.given

import Clue.*


object Read:

  def apply(filename: String): IO[(Point, Set[Clue])] =
    //          size                          c[olor]      r[esult]         str[ips]
    val init = (Mutable(Option.empty[Point]), Mutable(-1), HashSet[Clue](), HashSet[Strip]())

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
      .fold(init) { case ((size, c, r, str), (clue, ls)) =>

        def strip(at: Point, by: Point): Strip =
          str.find {
            case Strip(`at`, `by`) | Strip(`by`, `at`) => true
            case _ => false
          } match
            case Some(it) => it
            case _ => ???

        def valid(it: Point) = 1 <= it.row && it.row <= size.get.row && 1 <= it.col && it.col <= size.get.col

        val cs = ls.map(_.toInt).toSeq
        val ps = Seq.from(cs.pairwise.map(_ -> _).sliding(1, 2)).map(_.head)
        val ps2 = ps.toSet

        // println(clue)
        // println(ps)

        def unique = !r.exists {
          case Start(at, _, _) => ps.contains(at)
          case Cross(at) => ps.contains(at)
          case Empty(at) => ps.contains(at)
          case _ => false
        }

        clue.toUpperCase match

          case "BOARD" => if size.nonEmpty then ???

          case _ => if size.isEmpty then ???


        clue.toUpperCase match

          case "BOARD"
              if ps.size == 1 =>

            require(2 <= ps(0).row && 2 <= ps(0).col)

            size ::= Some(ps(0))

            str.addAll {
              ( for
                  row <- 1 to size.get.row
                yield
                  Set(Strip(row x 1-1, row x 1), Strip(row x size.get.col, row x size.get.col + 1))
              ).reduce(_ ++ _)
              ++
              ( for
                  col <- 1 to size.get.col
                yield
                  Set(Strip(1-1 x col, 1 x col), Strip(size.get.row x col, size.get.row + 1 x col))
              ).reduce(_ ++ _)
            }


          case "START"
              if ps2.size == 2
              && unique =>

            require(ps.forall(valid(_)))

            r.add(Start(ps(0), ps(1), c))
            r.add(Start(ps(1), ps(0), c))
            c ::= c - 1


          case "EMPTY"
              if ps.nonEmpty
              && unique =>

            require(ps.forall(valid(_)))

            ps.foreach { at => r.add(Empty(at)) }


          case "CROSS"
              if ps.size == 1
              && unique =>

            require(ps.forall(valid(_)))

            r.add(Cross(ps(0)))


          case "STRIP"
              if ps.size == 2
              && !str.exists { case Strip(at, by) => Set(at, by) == ps2 } =>

            require(ps.forall(valid(_)) && ps(0).adj(ps(1)))

            r.add(Strip(ps(0), ps(1)))

            str.add(Strip(ps(0), ps(1)))


          case "FRAME"
            if ps.size >= 8 =>

            val ls = List.from(ps.sliding(2,2))
            val ts = ls.map(_.head) zip ls.map(_.last)

            r.add(Frame(ts.map(tupled(strip(_, _)))*))


          case "TRACK"
            if ps.size % 5 == 0
            && Set.from(ps.sliding(1,5)).map(_.head).size == ps.size / 5
            && r.forall {
              case Track(_, path*) =>
                (path.toSet & ps2).forall { pt =>
                  r.exists {
                    case Cross(`pt`) => true
                    case _ => false
                  }
                }
              case _ => true
            } =>

            val path = MutableList[Point]()
            val tr = HashMap[Point, Option[(Strip, Strip)]]()

            ps.sliding(5, 5)
              .foreach {
                case it @ Seq(p, at1, by1, at2, by2, _*) =>
                  tr(p) =
                    if it.tail.forall(_ == (0 x 0))
                    then
                      None
                    else if Set(at1, by1) == Set(at2, by2)
                    then
                      ???
                    else
                      Some(strip(at1, by1) -> strip(at2, by2))
                  path += p
              }

            r.add(Track(tr.toMap, path.toSeq*))


          case _ => ???

        (size, c, r, str)
      }
      .map { (size, _, r, _) => size.get -> r.toSet }
      // .map { (size, _, r, _) =>

      //   r.foreach {
      //     case it @ Start(p, _, color) =>
      //       println(s"Start($p, _, $color)")

      //     case it @  Cross(p) =>
      //       println(s"Cross($p) #${it.##}")

      //     case it @ Strip(at, by) =>
      //       println(s"Strip($at, $by) #${it.##}")

      //     case it @ Frame(border*) =>
      //       println(s"Frame{${border.map(_.toString)}} #${it.##}")

      //     case it @ Track(sides, path*) =>
      //       println(s"Track{${path.map(_.toString)}} #${it.##}")

      //     case _ =>
      //   }

      //   size.get -> r.toSet

      // }
