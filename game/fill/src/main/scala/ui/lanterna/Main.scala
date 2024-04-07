package urru
package game
package fill
package ui.lanterna

import cats.effect.{ ExitCode, IO, IOApp, Ref, Resource }

import com.googlecode.lanterna.terminal.{ DefaultTerminalFactory, Terminal }

import grid.Game.Feature._

import fill.util.Read
import util.Term.apply


object Main extends IOApp:

  def term(): Resource[IO, Terminal] =
    Resource.make {
      IO {
        val terminal = new DefaultTerminalFactory().setForceTextTerminal(true).createTerminal()
        terminal.setCursorVisible(false)
        terminal
      }
    } { terminal =>
      IO {
        terminal.setCursorVisible(true)
        terminal.close
      }
    }

  override def run(args: List[String]): IO[ExitCode] =

    ( for
        terminal <- term()
      yield
        terminal
    ).use { terminal =>
      def loop(id: Ref[IO, Long], mp: Ref[IO, Map[Int, List[Int]]], k: Int, ls: Ref[IO, List[Int]], t: String): IO[ExitCode] =
        for
          l <- ls.get
          i = l.head
          r <- Read(s"fill-$t$k-$i.txt")
          (size, clues) = r
          n <- id.get
          game = Game(n, size, clues) //, Just, Have, Pisc
          _ <- terminal(s"fill-$t$k-$i", game)
          _ <- id.update(_ + 1)
          _ <- if l.tail.isEmpty then mp.update(_.tail) else ls.update(_.tail)
          m <- mp.get
          ec <- if m.isEmpty then IO(ExitCode.Success)
               else if l.tail.isEmpty then
                 for
                   m <- mp.get
                   (k, l) = m.head
                   ls <- IO.ref(l)
                   ec <- loop(id, mp, k, ls, t)
                 yield
                   ec
               else
                 loop(id, mp, k, ls, t)
        yield
          ec
      for
        id <- IO.ref(1L)
        // mp <- IO.ref(Map(0 -> List(0), 2 -> List(5, 168)))
        // mp <- IO.ref(Map(7 -> List(19)))
        mp <- IO.ref(Map(3 -> List(125)))
        // mp <- IO.ref(Map(4 -> List(14)))
        m <- mp.get
        (k, l) = m.head
        ls <- IO.ref(l)
        // t = "cl-wc"
        t = "jp-pb-"
        // t = ""
        ec <- loop(id, mp, k, ls, t)
      yield
        ec
    }

      // import spray.json.enrichAny
      // import util.JsonFormats.GameJsonProtocol._
      // //println(game.toJson.prettyPrint)
      // println(game.toJson.convertTo[Game].toJson.prettyPrint)
