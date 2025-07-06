/*
 * Copyright (c) 2023-2025 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * [Except as contained in this notice, the name of Sebastian I. Gliţa-Catina
 * shall not be used in advertising or otherwise to promote the sale, use
 * or other dealings in this Software without prior written authorization
 * from Sebastian I. Gliţa-Catina.]
 */

package pisc

import java.io.{ FileWriter, BufferedWriter }
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths

import scala.collection.{ Map, Set }
import scala.io.Source

import scala.meta.*
import dialects.Scala3

import parser.{ StochasticPi, Calculus }
import StochasticPi.Actions
import Calculus.{ `(*)`,  λ }
import generator.Program


object Main:

  val examples = "examples"

  def main(args: Array[String]): Unit =
    args.foreach { arg =>
      val in = if arg.endsWith(".pisc") then arg else arg + ".pisc"
      val out = Paths.get(s"$examples/in/", in.stripSuffix("pisc") + "scala.in").toString
      var source: Source = null
      var fwr: FileWriter = null
      var bwr: BufferedWriter = null

      val spi = StochasticPi.Main(in)

      try
        source = Source.fromFile(s"$examples/pisc/$in")
        fwr = FileWriter(out, UTF_8)
        bwr = BufferedWriter(fwr)

        val bs = spi(source)

        val bind = bs.zipWithIndex
        val prog_ = bind.filter(_._1.isRight).map(_.right.get -> _)

        val (prog, (discarded, excluded, enabled)) = spi(prog_.map(_._1))

        val ps = Program.Main()(prog)
        val is = prog_.map(_._2).zipWithIndex.map(_.swap).toMap

        val ls = bind.filter(_._1.isLeft).map(_.left.get -> _)

        val code = (ps.zipWithIndex.map { _ -> is(_) } ++ ls)
          .sortBy(_._2)
          .map(_._1)
          .mkString("\n\n")

        val trick = `trick-or-treat`("π-trick", discarded).toString
        val spell = `spell, magic spell`("π-spell", enabled).toString
        val wand = `magic wand`.toString

        val magic = trick + "\n\n" + spell + "\n\n" + wand + "\n\n"
        val elvis = `if-then-else`("π-elvis", excluded).toString + "\n\n"

        val init = this.Π(
          prog
            .find {
              case (`(*)`("Main"), _) => true
              case _ => false
            }
            .get
            ._2
            .enabled
        ).toString + "\n\n"

        bwr.write(magic + elvis + init + code, 0, magic.length + elvis.length + init.length + code.length)
      catch t =>
        Console.err.println(s"Error in file `$in' ${spi.ln}! " + t.getMessage + ".")
        throw t
      finally
        if bwr ne null then bwr.close()
        if fwr ne null then fwr.close()
        if source ne null then source.close()
    }

  val `magic wand`: Defn.Val =
    val t = Type.Apply(Type.Name("Π-Map"),
                       Type.ArgClause(List(Type.Name("String"),
                                           Type.Apply(Type.Name("Π-Set"),
                                                      Type.ArgClause(Type.Name("String") :: Nil)))))
    Defn.Val(Mod.Implicit() :: Nil,
             Pat.Var(Term.Name("π-wand")) :: Nil,
             Some(Type.Tuple(List(t, t))),
             Term.ApplyInfix(Term.Name("π-trick"),
                             Term.Name("->"),
                             Type.ArgClause(Nil),
                             Term.ArgClause(Term.Name("π-spell") :: Nil)))

  def `trick-or-treat`(name: String, discarded: Map[String, Actions]): Defn.Val =
    Defn.Val(Nil,
             Pat.Var(Term.Name(name)) :: Nil,
             Some(Type.Apply(Type.Name("Π-Map"),
                             Type.ArgClause(List(Type.Name("String"),
                                                 Type.Apply(Type.Name("Π-Set"),
                                                            Type.ArgClause(Type.Name("String") :: Nil)))))),
             Term.Apply(scollimmMap,
                        Term.ArgClause(this.Π(discarded).toList)))

  def `spell, magic spell`(name: String, enabled: Map[String, Actions]): Defn.Val =
    Defn.Val(Nil,
             Pat.Var(Term.Name(name)) :: Nil,
             Some(Type.Apply(Type.Name("Π-Map"),
                             Type.ArgClause(List(Type.Name("String"),
                                                 Type.Apply(Type.Name("Π-Set"),
                                                            Type.ArgClause(Type.Name("String") :: Nil)))))),
             Term.Apply(scollimmMap,
                        Term.ArgClause(this.Π(enabled).toList)))

  def `if-then-else`(name: String, excluded: Map[String, Actions]): Defn.Val =
    Defn.Val(Mod.Implicit() :: Nil,
             Pat.Var(Term.Name(name)) :: Nil,
             Some(Type.Apply(Type.Name("Π-Map"),
                             Type.ArgClause(List(Type.Name("String"),
                                                 Type.Apply(Type.Name("Π-Set"),
                                                            Type.ArgClause(Type.Name("String") :: Nil)))))),
             Term.Apply(scollimmMap,
                        Term.ArgClause(this.Π(excluded).toList)))

  private object Π:

    def apply(self: Map[String, Actions]) =
      for
        (id, it) <- self
      yield
        Term.ApplyInfix(Lit.String(id),
                        Term.Name("->"), Type.ArgClause(Nil),
                        Term.ArgClause(Term.Apply(scollimmSet,
                                                  Term.ArgClause(it.map(Lit.String(_)).toList)
                                       ) :: Nil))

    def apply(it: Actions): Defn.Val =
      Defn.Val(Nil,
               Pat.Var(Term.Name("π-main")) :: Nil,
               Some(Type.Apply(Type.Name("Π-Set"),
                               Type.ArgClause(Type.Name("String") :: Nil))),
               Term.Apply(scollimmSet,
                          Term.ArgClause(it.map(Lit.String(_)).toList)
               ))

  private val scollimmMap =
    Term.Select(
      Term.Select(
        Term.Select(
          Term.Select(
            Term.Name("_root_"),
            Term.Name("scala")),
          Term.Name("collection")),
        Term.Name("immutable")),
      Term.Name("Map"))

  private val scollimmSet =
    Term.Select(
      Term.Select(
        Term.Select(
          Term.Select(
            Term.Name("_root_"),
            Term.Name("scala")),
          Term.Name("collection")),
        Term.Name("immutable")),
      Term.Name("Set"))
