package urru
package game
package fold
package ui.lanterna

import cats.effect.{ ExitCode, IO, IOApp, Ref, Resource }

import com.googlecode.lanterna.terminal.{ DefaultTerminalFactory, Terminal }

import grid.Game.Feature._

import fold.util.Read
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
      def loop(id: Ref[IO, Long], ls: Ref[IO, List[Int]]): IO[ExitCode] =
        for
          l <- ls.get
          i = l.head
          r <- Read(s"fold-lv-$i.txt")
          (size, clues) = r
          n <- id.get
          game = Game(n, size, clues) //, Just, Have, Pisc
          _ <- terminal(s"fold-lv-$i", game)
          _ <- id.update(_ + 1)
          _ <- ls.update(_.tail)
          l <- ls.get
          _ <- if l.isEmpty then IO(ExitCode.Success)
               else loop(id, ls)
        yield
          ExitCode.Error
      for
        id <- IO.ref(1L)
        //ls <- IO.ref(List(1170, 1171))
        ls <- IO.ref(List(1279))
        ec <- loop(id, ls)
      yield
        ec
    }

      // import spray.json.enrichAny
      // import util.JsonFormats.GameJsonProtocol._
      // println(game.toJson.prettyPrint)
      // println(game.toJson.convertTo[Game].toJson.prettyPrint)
      // terminal(game.toJson.convertTo[Game])
