package urru
package game
package flow
package ui.lanterna

import cats.effect.{ ExitCode, IO, IOApp, Ref, Resource }

import com.googlecode.lanterna.terminal.{ DefaultTerminalFactory, Terminal }

import grid.Game.Feature._

import flow.util.Read
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

  import cats.effect.FiberIO

  override def run(args: List[String]): IO[ExitCode] =

    ( for
        terminal <- term()
      yield
        terminal
    ).use { terminal =>
      def loop(id: Ref[IO, Long], ls: Ref[IO, List[Int]], t: String): IO[ExitCode] =
        for
          l <- ls.get
          i = l.head
          r <- Read(s"flow-$t-$i.txt")
          (size, clues) = r
          n <- id.get
          game = Game(n, size, clues) //, Just, Have, Pisc
          _ <- terminal(s"flow-$t-$i", game)
          _ <- id.update(_ + 1)
          _ <- ls.update(_.tail)
          l <- ls.get
          ec <- if l.isEmpty then IO(ExitCode.Success) else loop(id, ls, t)
        yield
          ec
      for
        id <- IO.ref(1L)
        // i <- List(2, 3)
        // t = "br"
        ls <- IO.ref(List(2, 3))
        t = "cl-wc"
        // i <- List(2, 3, 4, 5)
        // i <- List(3)
        // t = "cl"
        ls <- IO.ref(List(1))
        t = "weekly-htb"
        // ls <- IO.ref(List(11))
        // t = "weekly-p"
        ec <- loop(id, ls, t)
      yield
        ec
    }

      // for
      //   file <- args
      // do
      //   n += 1
      //   val (size, clues) = Read(file)
      //   val game = Game(n, size, clues)
      //   terminal(game)

        // import spray.json.enrichAny
        // import util.JsonFormats.GameJsonProtocol._
        // //println(game.toJson.prettyPrint)
        // println(game.toJson.convertTo[Game].toJson.prettyPrint)
