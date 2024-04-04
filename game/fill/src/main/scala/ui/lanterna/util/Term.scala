package urru
package game
package fill
package ui.lanterna
package util

import cats.effect.{ Deferred, IO, Ref, Resource }

import fs2.Stream
import fs2.io.file.{ Files, Path }

import com.googlecode.lanterna.terminal.Terminal
import com.googlecode.lanterna.input.KeyType

import common.grid.{ row, x, col }

import Clue._

import DnD._


object Term:

  extension(self: Terminal)

    def prompt(game: Game, paused: Boolean, idleTime: Long): Unit =
      val size = game.size
      val color = game.nowPlay.block.color
      val i = -color-1

      self.setCursorPosition(0, 2*(size.row+1)+5)
      self.setForegroundColor(Code.colors(1))
      self.putString("• COLOR IS ")
      self.setForegroundColor(Code.colors(color))
      self.putString(Code.BLOCK.toString)
      self.setForegroundColor(Code.colors(1))
      self.putString {
        if game.state(i).play.head.pad
        then
          " <INIT>"
        else if game.state(i).over
        then
          " (FILL)"
        else
          " [OPEN]"
      }

      val play = game.state
        .map(_.play.head)
        .filterNot(_.pad)
        .map(_.block.block)

      val total = game.state.count(_.over)
      val percent = play.flatten.toSet.size.toDouble / game.init._1.size.toDouble
      self.setCursorPosition(0, 2*(size.row+1)+6)
      self.setForegroundColor(Code.colors(1))
      self.putString(s"• PLAYING… BLOCKED: #$total/${game.state.size} COVERAGE: ${(percent * 100.0).toInt}%")

      if game.pending.nonEmpty
      then
        self.putString(" PENDING:")
        for
          (i, _) <- game.pending
          c = -i-1
        do
          self.setForegroundColor(Code.colors(c))
          self.putString(" " + Code.BLOCK.toString)

      self.setCursorPosition(0, 2*(size.row+1)+7)
      self.setForegroundColor(Code.colors(1))
      self.putString("• MODE: ")
      self.setForegroundColor(Code.colors(color))
      self.putString(if game.selectionMode == 0 then "DRAG" else "DROP")

      if false && game.status
      then
        for
          c <- List.from(0 until game.state.size) :+ -2
        do
          self.setCursorPosition(0, 2*(size.row+1)+8)
          self.setForegroundColor(Code.colors(1))
          self.putString("• GAME OVER: ")
          self.setForegroundColor(Code.colors(-c-1))
          self.putString("YOU WIN!")
          Thread.sleep(500)

      else if game.gameOver
      then

        self.setCursorPosition(0, 2*(size.row+1)+8)
        self.setForegroundColor(Code.colors(1))
        self.putString(s"• GAME OVER${if game.status then ": YOU WIN!" else "!"}")

      if !game.gameOver
      then
        self.setCursorPosition(0, 2*(size.row+1)+8)
        self.setForegroundColor(Code.colors(1))
        if paused
        then
          self.putString(s"• PAUSED")
        else
          val elapsed = 0L max ((System.currentTimeMillis - idleTime) - game.startTime - (0L max game.minusTime))
          val ms = elapsed % 1000
          val dd = (elapsed / 1000) / 86400
          val hh = ((elapsed / 1000) % 86400) / 3600
          val mm = (((elapsed / 1000) % 86400) % 3600) / 60
          val ss = ((((elapsed / 1000) % 86400) % 3600) % 60) / 1
          self.putString(s"• ELAPSED:${if dd > 0 then s" $dd day${if dd > 1 then "s" else ""}" else ""} $hh:$mm:$ss.$ms")

      //self.setBackgroundColor(Code.colors(0))
      self.setForegroundColor(Code.colors(0))


    def apply(name: String, game: Game): IO[Unit] =

      def render(clues: List[List[Code]],
                 justR: Ref[IO, List[List[Code]]],
                 linesR: Ref[IO, List[Terminal => Unit]]
      ): IO[Unit] =
        for
          just <- justR.get
          _ <- linesR.set(Code(game, Code(game, clues, just)))
        yield ()

      def restart(idleTimeR: Ref[IO, Long],
                  startedR: Ref[IO, Long],
                  clues: List[List[Code]],
                  justR: Ref[IO, List[List[Code]]],
                  linesR: Ref[IO, List[Terminal => Unit]]
      ): IO[Unit] =
        for
          _ <- idleTimeR.set(0L)
          _ <- startedR.set(System.currentTimeMillis)
          started <- startedR.get
          _ <- IO {
            game.restart
            game.startTime = started
          } >> render(clues, justR, linesR)
        yield ()

      def toggle(clues: List[List[Code]],
                 justR: Ref[IO, List[List[Code]]],
                 linesR: Ref[IO, List[Terminal => Unit]]
      ): IO[Unit] =
        IO { game.showAxes = !game.showAxes } >> render(clues, justR, linesR)

      def loop(doneD: Deferred[IO, Unit],
               idleTimeR: Ref[IO, Long],
               startedR: Ref[IO, Long],
               exitR: Ref[IO, Boolean],
               clues: List[List[Code]],
               justR: Ref[IO, List[List[Code]]],
               justD: Deferred[IO, Unit],
               linesR: Ref[IO, List[Terminal => Unit]],
               pausedR: Ref[IO, Boolean]
      ): IO[Unit] =
        ( for
            _ <-  ( if game.showJust.isEmpty
                    then
                     Resource.unit[IO]
                    else
                      ( for
                          _ <- justD.complete(())
                          _ <- doneD.get
                        yield ()
                      ).background
/*
                      ( for
                          _ <- justR.set(Nil)
                          grid = game.grid.toMap
                          over = game.state.map(_.over).toSeq
                          _ <- game.Just.travel.fold(()) { case (_, (color, path)) =>
                            path.headOption match {
                              case Some((intensity, urru, depth, nesting, degree)) =>
                                intensity.data.foreach {
                                  // case it if it(grid, over*) =>
                                    // println(s"it=$it depth=$depth nesting=$nesting degree=$degree")
                                  case _ =>
                                    // assert(false)
                                }
                              case _ =>
                            }
                          }.compile.drain
                          _ <- render(clues, justR, linesR)
                          _ <- justD.complete(())
                          _ <- doneD.get
                          _ <- IO {
                            self.setCursorPosition(0, 2*(game.size.row+1)+9)
                            self.setForegroundColor(Code.colors(1))
                            self.putString("✓ " + " " * (2*2*(game.size.col+1) - 6) + "JUST")
                          }
                        yield ()
                      ).background
*/
                  )
          yield ()
        ).use { _ =>
          for
            _ <-  ( if game.showJust.nonEmpty
                    then
                      justD.get
                    // else if game.showHave.nonEmpty
                    // then
                    //   haveD.get
                    else
                      IO.unit
                  )
            idleTime <- idleTimeR.get
            lines <- linesR.get
            paused <- pausedR.get

            _ <- IO {
              self.clearScreen()
              lines.foreach(_(self))
              self.prompt(game, paused, idleTime)
              self.flush()
            }
            _ <- doneD.complete(())

            started <- startedR.get
            ended = System.currentTimeMillis

            _ <- idleTimeR.update(_ + ended - started)

            _ <- startedR.set(ended)

            keyStroke <- IO.blocking { self.readInput() }

            started <- startedR.get
            elapsed = System.currentTimeMillis - started

            _ <- startedR.update(_ + elapsed)

            _ <-  ( if game.selectionMode != 0
                    then
                      ( if keyStroke.getKeyType eq KeyType.Escape
                        then
                          IO { game.dropOut() } >> render(clues, justR, linesR)

                        else if keyStroke.getKeyType eq KeyType.Backspace
                        then
                          IO { game.dragOn() } >> render(clues, justR, linesR)

                        else if keyStroke.getKeyType eq KeyType.Enter
                        then
                          IO { game.dragOff(elapsed) } >> render(clues, justR, linesR)

                        else if keyStroke.getKeyType eq KeyType.Character
                        then
                          val chr = keyStroke.getCharacter.toChar.toUpper

                          if chr == '#' || chr == '3'
                          then
                            toggle(clues, justR, linesR)

                          else if chr == '@' || chr == '2'
                          then
                            restart(idleTimeR, startedR, clues, justR, linesR)

                          else
                            IO.unit

                        else
                          IO.unit
                      )

                    else if keyStroke.getKeyType eq KeyType.Escape
                    then
                      exitR.set(true) >> IO {
                        self.clearScreen()
                        self.setCursorPosition(0, 0)
                        self.setForegroundColor(Code.colors(0))
                        self.putString("Use TAB to switch colors, - and + to drag'n'drop, / for pad.")
                        println()
                        self.putString("Use symbols <, >, ^, v to move left, right, up, down.")
                        println()
                        self.putString("Use # to toggle grid, @ to restart game, | to pause.")
                        println()
                        self.putString("Use keys BACKSPACE and ENTER to undo or redo.")
                        println()
                      }
                    else
                      ( if paused
                        then
                          idleTimeR.update(_ + elapsed) >>
                          ( if (keyStroke.getKeyType eq KeyType.Character)
                            && keyStroke.getCharacter.toChar == '|'
                            then
                             pausedR.set(false)
                            else
                              IO.unit
                          )

                        else // if !paused then

                          if keyStroke.getKeyType eq KeyType.Character
                          then
                            val chr = keyStroke.getCharacter.toChar.toUpper

                            if "^6V<,>.".contains(chr)
                            then
                              var dir = 0 -> 0
                              if chr == '^' || chr == '6'
                              then
                                dir = -1 -> 0
                              else if chr == 'V'
                              then
                                dir = 1 -> 0
                              else if chr == '<' || chr == ','
                              then
                                dir = 0 -> -1
                              else if chr == '>' || chr == '.'
                              then
                                dir = 0 -> 1

                              IO { game.move(dir)(elapsed) }.flatMap {
                                if _
                                then
                                  render(clues, justR, linesR)
                                else
                                  IO.unit
                              }

                            else if chr == '-'
                            then
                              IO { game.dragOut() } >> render(clues, justR, linesR)

                            else if chr == '+' || chr == '='
                            then
                              IO { game.dropIn() } >> render(clues, justR, linesR)

                            else if chr == '#' || chr == '3'
                            then
                              toggle(clues, justR, linesR)

                            else if chr == '@' || chr == '2'
                            then
                              restart(idleTimeR, startedR, clues, justR, linesR)

                            else if chr == '|'
                            then
                              pausedR.set(true)

                            else if chr == '/'
                            then
                              IO { game.showPad = !game.showPad } >> render(clues, justR, linesR)

                            else if chr == 'J' // Json
                            then
                              for
                                cwd <- Files[IO].currentWorkingDirectory
                                s = Stream.emit[IO, String] {
                                  import spray.json.enrichAny
                                  import fill.util.JsonFormats.GameJsonProtocol._
                                  game.toJson.prettyPrint
                                }
                                _ <- s.through(Files[IO].writeUtf8Lines(cwd / (Pisc.uuid(name) + ".json"))).compile.drain
                              yield ()

                            else if chr == 'P' // PiScala
                            then
                              for
                                _ <- Pisc(name, game)
                              yield ()

                            else if chr == '&'
                            then
                              IO { game.showJust = None; game.showPad = !game.showPad } >> render(clues, justR, linesR)

                            else if chr == '*'
                            then
                              IO { game.showJust = game.showJust.map(!_).orElse(Some(true)); game.showPad = false } >> render(clues, justR, linesR)

                            else
                              IO.unit

                          else if keyStroke.getKeyType eq KeyType.Backspace
                          then
                            IO { game.undo()(elapsed) }.flatMap {
                              if _
                              then
                                render(clues, justR, linesR)
                              else
                                IO.unit
                            }

                          else if keyStroke.getKeyType eq KeyType.Enter
                          then
                            IO { game.redo()(elapsed) }.flatMap {
                              if _
                              then
                                render(clues, justR, linesR)
                              else
                                IO.unit
                            }

                          else if keyStroke.getKeyType eq KeyType.Tab
                          then
                            IO { game.switch }

                          else
                            IO.unit

                      )
                  )
          yield ()
        } >> {
          for
            doneD <- Deferred[IO, Unit]
            justD <- Deferred[IO, Unit]
            _ <-  ( if game.showJust.isEmpty
                    then
                      justD.complete(())
                    else
                      IO.unit
                  )
            exit <- exitR.get
            _ <-  ( if exit
                    then
                      IO.unit
                    else
                      loop(doneD, idleTimeR, startedR, exitR, clues, justR, justD, linesR, pausedR)
                  )
          yield ()
        }

      for
        doneD <- Deferred[IO, Unit]
        idleTimeR <- IO.ref(0L)
        startedR <- IO.ref(System.currentTimeMillis)
        exitR <- IO.ref(false)
        justR <- IO.ref(Code(game))
        justD <- Deferred[IO, Unit]
        _ <-  ( if game.showJust.isEmpty
                then
                  justD.complete(())
                else
                  IO.unit
              )
        clues = Code(game)
        just <- justR.get
        linesR <- IO.ref(Code(game, Code(game, clues, just)))
        pausedR <- IO.ref(false)
        started <- startedR.get
        _ <- IO { game.startTime = started }
        _ <- loop(doneD, idleTimeR, startedR, exitR, clues, justR, justD, linesR, pausedR)
      yield ()
