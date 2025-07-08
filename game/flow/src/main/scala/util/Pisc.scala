package urru
package game
package flow
package util

import cats.effect.IO
import cats.effect.std.CyclicBarrier

import common.grid.{ row, col }

import common.Mongo

import common.Tree
import Tree.{ Leaf, Node, Validate }

import Versus.{ Data, Parameter, UR, Key }

import ui.scalafx.util.App


object sΠ:

  private val bsh: String => String = _.replaceAll("([ <(|*!'`\")>])", "\\\\$1")

  def apply(loser: String, player: String, filename: String): String =
    val Loser = s"Loser(ur) = $loser"
    val Loserʹ = s"""⟦ traces = "${filename}_Loser" ⟧\\\\n$Loser"""
    val Player = s"Player(ur) = $player"
    val Playerʹ = s"""⟦ traces = "${filename}_Player" ⟧\\\\n$Player"""
    val Main = "Main = ν(ur) ( Loser(ur) | Player(ur) )"
    val Mainʹ = s"""⟦ traces = off ⟧\\\\n$Main"""
    val main = s"${bsh(Loserʹ)}\\\\n${bsh(Playerʹ)}\\\\n${bsh(Mainʹ)}"
    s"""sh -c "cd sΠ/examples/pisc; echo -en $main > \"$filename.pisc\"""""

  def apply(o: Boolean, fs: String*): String =
    if o
    then
      s"""sh -c 'cd sΠ/examples; spio ${fs.mkString("\"", "\" \"", "\"")}'"""
    else
      s"""sh -c 'cd sΠ; sbt "run ${fs.mkString("\\\"", "\\\" \\\"", "\\\"")}"'"""

  def apply(filename: String): String =
    s"""sh -c 'cd sΠ/examples; spi_ "$filename.scala"'"""

  private object Gather:

    def apply(children: Seq[Tree[Data]]): Seq[Tree[Data]] =
      val (childrenʹ, childrenʹʹ) = children.partition {
        case Leaf(Data(_, Parameter(_, Some(_))))
           | Node(Data(_, Parameter(_, Some(_))), _*) => true
        case _ => false
      }
      childrenʹ ++ childrenʹʹ.flatMap {
        case Node(_, children*) => this(children)
        case _ => Nil
      }

  object Loser:

    def apply(mongo: Mongo, name: String, savepoint: String, game: Game, i: Int): Unit =
      import spray.json.enrichAny
      import common.Tree.Implicits.TreeJsonProtocolʹ.*
      import flow.util.Versus.Data.DataJsonProtocol.*

      val tree = Visitor.`Loser Versus Player`(game, i)

      assert(Validate(tree)())

      val treeʹ = savepoint -> tree

      mongo.save(treeʹ.toJson, s"flow_${name}_looser_vs_player_loosing")

    def apply(tree: Tree[Data])(using xs: List[Int] = Nil): String =
      tree match
        case Node(Data(_, Parameter(Some(_), _)), children*) =>
          val sender = "ur"
          Gather(children).groupBy {
            case Leaf(Data(_, Parameter(_, Some(UR(_, key, _))))) => key
            case Node(Data(_, Parameter(_, Some(UR(_, key, _)))), _*) => key
          }.zipWithIndex.map { case ((key, ts), x) =>
            given xsʹ: List[Int] = x :: xs
            val nameʹ = s"""${xsʹ.mkString("_")}"""
            val (uʹ, rʹ) = s"u_$nameʹ" -> s"r_$nameʹ"
            val new_ur = s"ν($uʹ, $rʹ) "
            val keyʹ = s"""(${key.at.row}, ${key.at.col}).`()` :: "${key.dir}".`()`"""
            val ps = new_ur + sender + s"</*'$uʹ :: '$rʹ :: $keyʹ :: Nil*/>. "
            ps + ts.map(this(_)).mkString("(", " + ", ")")
          }.mkString("(", " | ", ")")
        case Leaf(Data(_, Parameter(_, Some(UR(ur, _, elapsed))))) =>
          val name = s"""${xs.mkString("_")}"""
          val (u, r) = s"u_$name" -> s"r_$name"
          val rate = s" @ ${elapsed}⊤"
          val sender = if ur then u else r
          val senderʹ = sender + rate
          s"$senderʹ<$sender>."
        case Node(Data(_, Parameter(_, Some(UR(ur, _, elapsed)))), children*) =>
          val name = s"""${xs.mkString("_")}"""
          val (u, r) = s"u_$name" -> s"r_$name"
          val rate = s" @ ${elapsed}⊤"
          val sender = if ur then u else r
          val senderʹ = sender + rate
          Gather(children).groupBy {
            case Leaf(Data(_, Parameter(_, Some(UR(_, key, _))))) => key
            case Node(Data(_, Parameter(_, Some(UR(_, key, _)))), _*) => key
          }.zipWithIndex.map { case ((key, ts), x) =>
            given xsʹ: List[Int] = x :: xs
            val nameʹ = s"""${xsʹ.mkString("_")}"""
            val (uʹ, rʹ) = s"u_$nameʹ" -> s"r_$nameʹ"
            val new_ur = s"ν($uʹ, $rʹ) "
            val keyʹ = s"""(${key.at.row}, ${key.at.col}).`()` :: "${key.dir}".`()`"""
            val ps = new_ur + senderʹ + s"</*'$uʹ :: '$rʹ :: $keyʹ :: Nil*/>. "
            ps + ts.map(this(_)).mkString("(", " + ", ")")
          } match
            case it if it.nonEmpty => it.mkString("(", " | ", ")")
            case _ => s"$senderʹ<$sender>."
        case _ =>
          "()"

  object Player:

    def apply(tree: Tree[Data])(using xs: List[Int] = Nil): String =
      tree match
        case Node(Data(_, Parameter(Some(_), _)), children*) =>
          val repl = s"!.ur(list: List[`()`] /**/). "
          val uncons = s"list::(,, at: (Int, Int) /**/, dir: String /**/,). "
          Gather(children).groupBy {
            case Leaf(Data(_, Parameter(_, Some(UR(_, key, _))))) => key
            case Node(Data(_, Parameter(_, Some(UR(_, key, _)))), _*) => key
          }.zipWithIndex.map { case ((key, ts), x) =>
            given xsʹ: List[Int] = x :: xs
            val cases = s"""[at = /*(${key.at.row}, ${key.at.col})*/][dir = "${key.dir}"] """
            val nameʹ = s"""${xsʹ.mkString("_")}"""
            val (uʹ, rʹ) = s"u_$nameʹ" -> s"r_$nameʹ"
            val unconsʹ = s"list::($uʹ, $rʹ,). "
            "(" + cases + unconsʹ + ts.map(this(_)).mkString("(", " + ", ")") + ")"
          } match
            case it if it.nonEmpty => "(" + repl + uncons + it.mkString("(", " + ", ")") + ")"
            case _ => "()"
        case Leaf(Data(_, Parameter(_, Some(UR(ur, _, elapsed))))) =>
          val name = s"""${xs.mkString("_")}"""
          val (u, r) = s"u_$name" -> s"r_$name"
          val rate = s" @ ${elapsed}⊤"
          val recver = if ur then u else r
          val recverʹ = recver + rate
          s"$recverʹ($recver)."
        case Node(Data(_, Parameter(_, Some(UR(ur, _, elapsed)))), children*) =>
          val name = s"""${xs.mkString("_")}"""
          val (u, r) = s"u_$name" -> s"r_$name"
          val rate = s" @ ${elapsed}⊤"
          val recver = if ur then u else r
          val recverʹ = recver + rate
          val repl = s"!.$recverʹ(list: List[`()`] /**/). "
          val uncons = s"list::(,, at: (Int, Int) /**/, dir: String /**/,). "
          Gather(children).groupBy {
            case Leaf(Data(_, Parameter(_, Some(UR(_, key, _))))) => key
            case Node(Data(_, Parameter(_, Some(UR(_, key, _)))), _*) => key
          }.zipWithIndex.map { case ((key, ts), x) =>
            given xsʹ: List[Int] = x :: xs
            val cases = s"""[at = /*(${key.at.row}, ${key.at.col})*/][dir = "${key.dir}"] """
            val nameʹ = s"""${xsʹ.mkString("_")}"""
            val (uʹ, rʹ) = s"u_$nameʹ" -> s"r_$nameʹ"
            val unconsʹ = s"list::($uʹ, $rʹ,). "
            "(" + cases + unconsʹ + ts.map(this(_)).mkString("(", " + ", ")") + ")"
          } match
            case it if it.nonEmpty => "(" + repl + uncons + it.mkString("(", " + ", ")") + ")"
            case _ => s"$recverʹ($recver)."
        case _ =>
          "()"

  object Versus:

    import scalafx.scene.text.Text

    def apply(mongo: Mongo, game: Game, name: String, prompt: Text, busyCB: CyclicBarrier[IO]): IO[String] =
      for
        _ <-  IO { prompt.visible = true }
        p <-  IO {
                import flow.util.sΠ.Player

                val (odd, start) = game.nowStart
                val color = start.color
                val k = -color-1
                val i = 2*k+odd

                val tree = flow.util.Visitor.`Loser Versus Player`(game, i)
                assert(Validate(tree)())
                Player(tree)
              }
        ls <- IO.blocking {
                import spray.json.enrichString
                import common.Tree.Implicits.TreeJsonProtocolʹ.*
                import flow.util.Versus.Data.DataJsonProtocol.*
                import common.Tree
                import flow.util.Versus.Data
                import flow.util.sΠ.Loser

                val (odd, start) = game.nowStart
                val color = start.color
                val k = -color-1
                val i = 2*k+odd

                mongo.item(s"flow_${name}_looser_vs_player_loosing",
                           game.savepoint.current.get, i)
                  .map(_.toJson.parseJson.convertTo[(String, Tree[Data])]._2)
                  .map(Loser(_))
              }
        xs <- IO.blocking {
                import scala.sys.process.*

                ls.foldLeft((0, Nil: Seq[String])) {
                  case ((0, fs), l) =>
                    import flow.util.sΠ
                    val filename = Mongo.uuid(s"flow_${name}_loser_vs_player")
                    if 0 == sΠ(l, p, filename).!
                    then 0 -> (fs :+ filename)
                    else -1 -> Nil
                  case _ => -1 -> Nil
                }
              }
        (_, fs) = xs
        r <- (  if fs.isEmpty
                then
                  busyCB.await >> IO { prompt.visible = false } >> IO.pure("vacuous")
                else
                  import cats.instances.list.*
                  import cats.syntax.flatMap.*
                  import cats.syntax.parallel.*
                  IO.blocking {
                    import scala.sys.process.*
                    import flow.util.sΠ

                    if 0 == sΠ(o = false, fs*).!
                    && 0 == sΠ(o = true, fs*).!
                    then
                      0
                    else
                      -1
                  } <* busyCB.await <* IO { prompt.visible = false } >>= {
                    case 0 =>
                      fs.toList
                        .parTraverse { filename =>
                          IO.blocking {
                            import scala.sys.process.*
                            import flow.util.sΠ
                            sΠ(filename).!
                          }
                        }
                        .map(_.exists(_ == 0))
                        .map {
                          if _
                          then
                            "bisimilar"
                          else
                            "success"
                        }
                    case _ =>
                      IO.pure("error")
                  }
              )
      yield
        r
