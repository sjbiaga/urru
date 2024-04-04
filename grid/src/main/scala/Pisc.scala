package urru

import java.util.UUID

import cats.effect.IO

import fs2.Stream
import fs2.io.file.{ Files, Path }

import grid.Game
import Game._

/**
  * @see [[https://github.com/sjbiaga/pisc]]
  */
object Pisc:

  def uuid(pre: String): String =
    pre + "_" + UUID.randomUUID.toString.replaceAll("-", "_")

  private val _args = List("up", "down", "left", "right", "undo", "redo")
  private val _dnd_args = List("drag", "drop") ++ _args
  def args(dnd: Boolean = false) = (if dnd then _dnd_args else _args).mkString(", ")
  private def argsν(dnd: Boolean = false) = (if dnd then _dnd_args else _args).mkString("ν(", ")ν(", ")")
  private def args_4_6(dnd: Boolean = false) = if dnd then _dnd_args.take(6) else _args.take(4)

  def dirs[C <: Cell, K <: Clue, M <: Move[C, K]](it: M): String =
    it.dir match
      case (-1, 0) => "up"
      case (1, 0) => "down"
      case (0, -1) => "left"
      case (0, 1) => "right"
      case (0, 0) => "drag"
      case _ => "drop"

  def apply(name: String, game: Game[?, ?, ?, ?, ?, ?, ?]): IO[Unit] =
    if !game.features(Feature.Pisc)
    then
      IO.unit
    else
      val dnd = game.features(Feature.DnD)
      for
        cwd <- Files[IO].currentWorkingDirectory
        s = Stream.emits[IO, String] {
          ( "Main = " +
             ( for
                 j <- 0 until game.state(0).path.size
               yield
                 s"Main_$j"
             ).mkString(" + ")
          )
          +:
          ( for
              j <- 0 until game.state(0).path.size
            yield
              s"Main_$j = "
              + ( for
                    i <- 0 until game.state.size
                  yield
                    s"Path_${i}_$j"
                ).mkString(" | ")
          ).mkString("\n\n", "\n\n", "\n\n")
          +:
          ( s"""Loop(${args(dnd)}, ur) = 𝟎"""
            + " | "
            + s"""${args_4_6(dnd).map { it => it + "(n) . ( if ur = " + it + " then τ/*_ <- IO.cede*/ . Loop(" + args(dnd) + ", undo) else τ/*_ <- IO.unit*/ . Loop(" + args(dnd) + ", undo) )"}.mkString(" | ")}"""
            + " | "
            + s"""${_args.drop(4).map(_ + "(d) . Loop(" + args(dnd) + ", d)").mkString(" | ")}"""
            + "\n\n"
          )
          +:
          ( for
              i <- 0 until game.state.size
              j <- 0 until game.state(i).path.size
            yield {
              val (main, body) = game.state(i).path(j).PiScala()
              s"""Path_${i}_$j = ${argsν(dnd)}( Loop(${args(dnd)}, undo) | ( $main ) )"""
              + "\n\n"
              + body
              + "\n\n"
            }
          )
        }
        _ <- s.through(Files[IO].writeUtf8Lines(cwd / (uuid(name) + ".pisc"))).compile.drain
      yield ()
