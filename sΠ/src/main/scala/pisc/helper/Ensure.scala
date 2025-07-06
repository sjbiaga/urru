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
package helper

import scala.collection.mutable.{ LinkedHashMap => Map }

import parser.Calculus.*


object Ensure:

  case object MainParsingException
      extends EquationParsingException("Exactly one Main parameterless agent not found")

  case object MainParsingExceptionʹ
      extends EquationParsingException("The parameterless Main agent is recursive")

  case class RecRepParsingException(id: String, arity: Int, times: Int)
      extends EquationParsingException(s"""$id#$arity is recursively replicated${if times == 1 then "" else " " + times + " times"}""")


  private def indexʹ(prog: List[Bind]): ((String, Int)) => Int =
    case (identifier, size) =>
      prog
        .indexWhere {
          case (`(*)`(`identifier`, params*), _) if params.size == size => true
          case _ => false
        }

  def main(using prog: List[Bind]): Int =
    if 1 == prog
      .count {
        case (`(*)`("Main"), _) => true
        case _ => false
      }
    then prog
      .indexWhere {
        case (`(*)`("Main"), _) => true
        case _ => false
      }
    else -1

  extension (ast: AST)

    /**
      * When an existing invocation is found on the reachability graph:
      * - all definitions up to its definition are marked as recursive;
      * - if there is a replication up to its definition, the number of
      *   recursive replications is incremented.
      */
    def recursive(using stack: List[(String, Int)])
                 (using rec: Map[(String, Int), Int])
                 (using rep: Map[Int, Int])
                 (using prog: List[Bind])
                 (implicit repl: Int = 0): Unit =

      ast match

        case ∅() =>

        case +(_, it*) =>
         it.foldLeft(())((_, par) => par.recursive)

        case ∥(it*) =>
         it.foldLeft(())((_, seq) => seq.recursive)

        case `.`(end, _*) =>
          end.recursive

        case ?:(_, t, f) =>
          t.recursive
          f.foreach(_.recursive)

        case !(_, sum) =>
          sum.recursive(stack.size)

        case `⟦⟧`(_, _, sum, _, _) =>
          sum.recursive

        case _: `{}` => ???

        case it @ `(*)`(identifier, params*)
            if stack.contains(identifier -> params.size) =>
          val k = stack.lastIndexOf(identifier -> params.size)
          for
            j <- k until stack.size
            i = indexʹ(prog)(stack(j))
            sign = prog(i)._1.identifier -> prog(i)._1.params.size
          do
            if !rec.contains(sign)
            then
              rec(sign) = i+1
            if k < repl
            then
              if !rep.contains(i)
              then
                rep(i) = 0
              rep(i) += 1

        case `(*)`(identifier, params*) =>
          val i = indexʹ(prog)(identifier -> params.size)
          val sum = prog(i)._2
          sum.recursive(using stack :+ identifier -> params.size)
