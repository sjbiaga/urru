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

package scala.util.parsing.combinator
package pisc
package parser

import java.util.regex.Pattern
import scala.util.matching.Regex
import Regex.Match

import scala.collection.mutable.{ LinkedHashMap => Map }

import scala.meta.Term

import _root_.pisc.parser.Expression
import Expression.Code
import _root_.pisc.parser.Encoding
import _root_.pisc.parser.StochasticPi.*
import _root_.pisc.parser.Calculus.*
import Encoding.*
import Expansion.*


abstract class Expansion extends Encoding:

  /**
    * A parser that matches a regex string and returns the Match
    * [[https://stackoverflow.com/questions/1815716/accessing-scala-parser-regular-expression-match-data]]
    */
  def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf SubSequence(source, start)) match {
        case Some(matched) =>
          Success(matched, in.drop(start + matched.end - offset))
        case None =>
          Failure("string matching regex `"+r+"' expected but `"+in.first+"' found", in.drop(start - offset))
      }
    }
  }


  def instance(defs: List[Define], end: String)
              (using Bindings, Duplications): Parser[(`⟦⟧`, Names)] =
    var idx = -1
    val xid = χ_id

    new Parser[(`⟦⟧`, Names)] {

      def expand(in: Input, shadows: List[Option[Symbol]], key: CacheKey)
                (op: String, end: Either[String, String])
                (success: Input => (ParseResult[Fresh], Seq[Term]))
                (using bindings: Bindings)
                (using duplications: Duplications)
                (using substitution: Substitution)
                (using free: Names): (ParseResult[Fresh], Seq[Term]) =

        val source = in.source
        val offset = in.offset
        val start = handleWhiteSpace(source, offset)

        if (open_r findPrefixMatchOf SubSequence(source, start, 1 min (source.length - start))).nonEmpty
        then

          if op.charAt(0).isLower || op == "_"
          then
            Failure("name expected not instantiation", in) -> Nil

          else

            _cache.get(key) match

              case Some((exp: `⟦⟧`, cp, freeʹ, given Bindings, inʹ)) =>
                bindings ++= binders

                substitution(op) = exp
                free ++= freeʹ -- bindings.map(_._1)

                paste(cp)

                success(inʹ)

              case _ =>

                given Bindings = Bindings(bindings)
                parse(instantiation, in) match

                  case Success((exp, freeʹ), in) =>
                    bindings ++= binders

                    substitution(op) = exp
                    free ++= freeʹ -- bindings.map(_._1)

                    val source = in.source
                    val offset = in.offset
                    val start = handleWhiteSpace(source, offset)

                    val n = end.map(_.length).getOrElse(0)

                    if start + n <= source.length
                    && (n == 0 || SubSequence(source, start, n).toString == end.right.get)
                    then
                      val inʹ = in.drop(start + n - offset)

                      _cache(key) = (exp, copy, freeʹ, given_Bindings, inʹ)

                      success(inʹ)

                    else
                      Failure(s"operator '${end.right.get}' expected", in) -> Nil

                  case _ =>
                    Failure("instantiation expected", in) -> Nil

        else balanced(end)(source, start) match

          case Some(matched) =>
            var n = matched.end - end.orElse(end.swap).map(_.length).right.get
            val result = SubSequence(source, start, n).toString

            n += end.map(_.length).getOrElse(0)

            if result.isBlank
            || result.strip == "_"
            then

              if op.charAt(0).isUpper
              then
                Failure("choice expected", in) -> Nil

              else
                val inʹ = in.drop(start + n - offset)
                success(inʹ)

            else if op.charAt(0).isLower || op == "_"
            then

              parseAll(name, result) match

                case Success((λ(it: Symbol), freeʹ), _) =>
                  shadows(idx) match
                    case shadow @ Some(_) =>
                      BindingOccurrence(it, shadow)
                      duplications(xid)._2(op) = it -> shadow
                    case _ =>
                      bindings.find { case (`it`, Shadow(_)) => true case _ => false } match
                        case Some((_, Shadow(υidυ))) =>
                          substitution(op) = λ(υidυ)
                        case _ =>
                          substitution(op) = λ(it)
                      free ++= freeʹ -- bindings.map(_._1)
                  idx += 1

                  val inʹ = in.drop(start + n - offset)
                  success(inʹ)

                case Success((it, freeʹ), _) =>
                  substitution(op) = it
                  free ++= freeʹ -- bindings.map(_._1)
                  idx += 1

                  val inʹ = in.drop(start + n - offset)
                  success(inʹ)

                case _ =>
                  Failure("name expected", in) -> Nil

            else

              _cache.get(key) match

                case Some((sum: +, cp, freeʹ, given Bindings, inʹ)) =>
                  bindings ++= binders

                  substitution(op) = sum
                  free ++= freeʹ -- bindings.map(_._1)

                  paste(cp)

                  success(inʹ)

                case _ =>

                  given Bindings = Bindings(bindings)
                  parseAll(choice, result) match

                    case Success((sum, freeʹ), _) =>
                      bindings ++= binders

                      val sumʹ = sum.flatten.update(using Bindings(given_Bindings))

                      substitution(op) = sumʹ
                      free ++= freeʹ -- bindings.map(_._1)

                      val inʹ = in.drop(start + n - offset)

                      _cache(key) = (sumʹ, copy, freeʹ, given_Bindings, inʹ)

                      success(inʹ)

                    case _ =>
                      Failure("choice expected", in) -> Nil

          case _ =>
            val found = if start == source.length
                        then "end of source"
                        else "'"+source.charAt(start)+"'"
            Failure(end.map("operator '" + _ + "'").orElse(end.swap).right.get+" expected but "+found+" found", in.drop(start - offset)) -> Nil


      def expand(in: Input, _ts: Seq[Term], end: Either[String, String])
                (using Bindings, Duplications, Substitution, Names): ((Fresh, Term)) => (ParseResult[Fresh], Seq[Term]) =

        case (it @ (_, (_, shadows)), _rhs @ (Term.Name(_) | Term.Placeholder())) =>
          val rhs = _rhs match { case Term.Name(rhs) => rhs case Term.Placeholder() => "_" }

          val source = in.source
          val offset = in.offset
          val start = handleWhiteSpace(source, offset)

          val ts = _ts :+ _rhs
          val key = path -> (ts.mkString -> end) -> start

          def success(inʹ: Input) =
            Success(it, inʹ) -> ts

          expand(in, shadows, key)(rhs, end)(success)

        case (it @ (_, (_, shadows)), Term.ApplyInfix(_lhs @ (Term.Name(_) | Term.Placeholder()), _op @ Term.Name(op), _, List(rhs))) =>
          val lhs = _lhs match { case Term.Name(lhs) => lhs case Term.Placeholder() => "_" }

          val source = in.source
          val offset = in.offset
          val start = handleWhiteSpace(source, offset)

          val ts = _ts :+ _lhs :+ _op
          val key = path -> (ts.mkString -> end) -> start

          def success(inʹ: Input) =
            expand(inʹ, ts, end)(it -> rhs)

          expand(in, shadows, key)(lhs, Right(op))(success)

        case (it, Term.ApplyInfix(lhs: Term.ApplyInfix, _op @ Term.Name(op), _, List(rhs))) =>
          expand(in, _ts, Right(op))(it -> lhs) match
            case (Success(_, inʹ), ts) =>
              expand(inʹ, ts :+ _op, end)(it -> rhs)
            case it => it

        case (it, Term.AnonymousFunction(body)) =>
          expand(in, _ts, end)(it -> body)

        case _ => ??? // caught by template


      override def apply(in: Input): ParseResult[(`⟦⟧`, Names)] =
        val duplications = summon[Duplications]
        duplications += xid -> (false, Map())

        try
          val bindings = summon[Bindings]

          var r: Option[(Fresh, (Substitution, (Names, (Bindings, Duplications))), Input)] = None

          var ls = defs
          while ls.nonEmpty
          do
            val (_macro, Definition(code, Some(term), _, _, _)) = ls.head : @unchecked
            ls = ls.tail

            given Bindings = Bindings(bindings)
            given Duplications = Duplications(duplications)
            given Substitution()
            given Names()
            idx = 0

            save(expand(in, Nil, Left(end))(_macro(code, term, _dups)(id, χ_id) -> term)._1) match
              case Some(_) if r.nonEmpty => throw AmbiguousParsingException
              case Some((it @ (_, (arity, _)), in)) if arity == given_Substitution.size =>
                r = Some((it, given_Substitution -> (given_Names -> (given_Bindings, given_Duplications)), in))
              case _ =>

          r match

            case Some(((definition, _), (given Substitution, (free, (given Bindings, given Duplications))), inʹ)) =>
              given_Duplications --= duplications.keySet
              duplications ++= given_Duplications

              val exp = definition(_code, _nest, _dups, duplicated)(id)(using duplications)

              bindings ++= purged

              Success(exp.copy(xid = xid) -> free, inʹ)

            case _ => throw UndefinedParsingException

        catch t =>
          duplications -= xid

          Failure(t.getMessage, in)

    }.named(s"""⟦${if defs.size == 1 then defs.head._2.code.toString else ""}⟧""")


  protected def duplicated(xid: String)
                          (using Bindings)
                          (using duplications: Duplications): Term => Unit =

    case _rhs @ (Term.Name(_) | Term.Placeholder()) =>
      val rhs = _rhs match { case Term.Name(rhs) => rhs case Term.Placeholder() => "_" }

      duplications(xid) match
        case (true, map) =>
          map.get(rhs) match
            case Some(it -> shadow) =>
              BindingOccurrence(it, shadow)
            case _ =>
        case _ =>

    case Term.ApplyInfix(_lhs @ (Term.Name(_) | Term.Placeholder()), _, _, List(rhs)) =>
      val lhs = _lhs match { case Term.Name(lhs) => lhs case Term.Placeholder() => "_" }

      duplications(xid) match
        case (true, map) =>
          map.get(lhs) match
            case Some(it -> shadow) =>
              BindingOccurrence(it, shadow)
            case _ =>
        case _ =>

      duplicated(xid)(rhs)

    case Term.ApplyInfix(lhs: Term.ApplyInfix, _, _, List(rhs)) =>
      duplicated(xid)(lhs)
      duplicated(xid)(rhs)

    case Term.AnonymousFunction(body) =>
      duplicated(xid)(body)


object Expansion:

  type Duplications = Map[String, (Boolean, Map[String, (Symbol, Option[Symbol])])]

  object Duplications:
    def apply(): Duplications = Map()
    def apply(duplications: Duplications): Duplications = Map.from(duplications)

  type Substitution = Map[String, λ | (+ | `⟦⟧`)]

  private lazy val open_r = """⟦\d*""".r
  private lazy val closed_r = """\d*⟧""".r

  // exceptions

  import _root_.pisc.parser.Expression.ParsingException

  case object AmbiguousParsingException
      extends ParsingException(s"An instantiation of a template is ambiguous")

  case object UndefinedParsingException
      extends ParsingException(s"An instantiation of a template is undefined")


  // functions

  private[parser] def balanced(end: Either[String, String])
                              (source: CharSequence, start: Int): Option[Match] =
    var s = SubSequence(source, start)
    val op = end.orElse(end.swap).right.get
    val n = op.length
    val matches = (Pattern.quote(op).r findAllMatchIn s).toList
    var i = 0
    while i < matches.size
    && { s = SubSequence(source, start, matches(i).end - n); true }
    && (open_r findAllMatchIn s).size > (closed_r findAllMatchIn s).size
    do
      i += 1
    if i < matches.size
    && {
      val os = (open_r findAllMatchIn s).toList
      val cs = (closed_r findAllMatchIn s).toList
      if os.size != cs.size
      then
        false
      else if os.isEmpty
      then
        true
      else
        var oi, ci = 0
        while ci <= oi && oi < os.size
        do
          while oi < os.size && os(oi).start < cs(ci).start
          do
            oi += 1
          if oi < os.size
          then
            while ci < cs.size && cs(ci).start < os(oi).start
            do
              ci += 1
          else
            ci = cs.size
        oi == os.size && ci == cs.size
    }
    then
      Some(matches(i))
    else
      None

  def replaced(name: Symbol)
              (using substitution: Substitution): λ =
    substitution.get(name.name) match
      case Some(it: λ) => it
      case _ => λ(name)

  def updated(name: Symbol)
             (using bindings: Bindings): λ =
    bindings.find { case (`name`, Shadow(_)) => true case _ => false } match
      case Some((_, Shadow(it))) => λ(it)
      case _ => λ(name)

  private def recoded(using code: Option[Code])
                     (using substitution: Substitution = null)
                     (using updating: Bindings = null): Option[Code] =
    code.map { (_, orig) =>
      Expression(orig)._1 match
        case term @ Term.ForYield(enums, _) =>
          (Left(enums), term)
        case term =>
          (Right(term), term)
    }


  extension [T <: AST](ast: T)

    def replace(using rename: + | `⟦⟧` => + | `⟦⟧`)
               (using substitution: Substitution): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case +(_, it*) =>
          `+`(nil, it.map(_.replace)*)

        case ∥(it*) =>
          ∥(it.map(_.replace)*)

        case `.`(end, _it*) =>
          val it = _it.map {
            case it @ τ(_, given Option[Code]) =>
              it.copy(code = recoded)(it.id)
            case it @ π(λ(ch: Symbol), _, Some(_), _, given Option[Code]) =>
              it.copy(channel = replaced(ch), code = recoded)(it.id)
            case it @ π(λ(ch: Symbol), λ(arg: Symbol), None, _, given Option[Code]) =>
              it.copy(channel = replaced(ch), name = replaced(arg), code = recoded)(it.id)
            case it @ π(λ(ch: Symbol), _, None, _, given Option[Code]) =>
              it.copy(channel = replaced(ch), code = recoded)(it.id)
            case it => it
          }
          `.`(end.replace, it*)

        case ?:(((λ(lhs: Symbol), λ(rhs: Symbol)), m), t, f) =>
          ?:(((replaced(lhs), replaced(rhs)), m), t.replace, f.map(_.replace))

        case ?:(((λ(lhs: Symbol), rhs), m), t, f) =>
          ?:(((replaced(lhs), rhs), m), t.replace, f.map(_.replace))

        case ?:(((lhs, λ(rhs: Symbol)), m), t, f) =>
          ?:(((lhs, replaced(rhs)), m), t.replace, f.map(_.replace))

        case ?:(cond, t, f) =>
          ?:(cond, t.replace, f.map(_.replace))

        case !(Some(it @ τ(_, given Option[Code])), sum) =>
          `!`(Some(it.copy(code = recoded)(it.id)), sum.replace)

        case !(Some(it @ π(λ(ch: Symbol), _, Some(_), _, given Option[Code])), sum) =>
          `!`(Some(it.copy(channel = replaced(ch), code = recoded)(it.id)), sum.replace)

        case !(Some(it @ π(λ(ch: Symbol), λ(arg: Symbol), None, _, given Option[Code])), sum) =>
          `!`(Some(it.copy(channel = replaced(ch), name = replaced(arg), code = recoded)(it.id)), sum.replace)

        case !(Some(it @ π(λ(ch: Symbol), _, None, _, given Option[Code])), sum) =>
          `!`(Some(it.copy(channel = replaced(ch), code = recoded)(it.id)), sum.replace)

        case it @ !(_, sum) =>
          it.copy(sum = sum.replace)

        case it @ `⟦⟧`(_, _, sum, _, _) =>
          val assignment = it.assignment.map(_ -> replaced(_).asSymbol)
          it.copy(sum = sum.replace, assignment = assignment)

        case `{}`(identifier, pointers, false) =>
          val ast = rename(substitution(identifier).asInstanceOf[+ | `⟦⟧`])
          given List[Symbol] = pointers.map(replaced(_).asSymbol)
          if given_List_Symbol.nonEmpty
          then
            ast.concatenate
          else
            ast

        case `{}`(identifier, pointers, true, params*) =>
          val pointersʹ = pointers.map(replaced(_).asSymbol)
          val paramsʹ = params
            .map {
              case λ(it: Symbol) => replaced(it)
              case it => it
            }

          `{}`(identifier, pointersʹ, true, paramsʹ*)

        case _: `{}` => ???

        case `(*)`(identifier, params*) =>
          val paramsʹ = params
            .map {
              case λ(it: Symbol) => replaced(it)
              case it => it
            }

          `(*)`(identifier, paramsʹ*)


    private def concatenate(using pointers: List[Symbol]): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case +(_, it*) =>
          `+`(nil, it.map(_.concatenate)*)

        case ∥(it*) =>
          ∥(it.map(_.concatenate)*)

        case `.`(end, it*) =>
          `.`(end.concatenate, it*)

        case ?:(cond, t, f) =>
          ?:(cond, t.concatenate, f.map(_.concatenate))

        case it @ !(_, sum) =>
          it.copy(sum = sum.concatenate)

        case it @ `⟦⟧`(_, variables, _, _, _) =>
          it.assignment ++= variables.drop(it.assignment.size) zip pointers
          it

        case it @ `{}`(identifier, _, agent, params*) =>
          `{}`(identifier, it.pointers ++ pointers, agent, params*)

        case _ => ast


    def update(using bindings: Bindings): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case +(_, it*) =>
          `+`(nil, it.map(_.update)*)

        case ∥(it*) =>
          ∥(it.map(_.update)*)

        case `.`(end, _it*) =>
          given Bindings = Bindings(bindings)
          val it = _it.map {
            case it @ ν(names*) =>
              given_Bindings --= names.map(Symbol(_))
              it
            case it @ τ(_, given Option[Code]) =>
              it.copy(code = recoded)(it.id)
            case it @ π(λ(ch: Symbol), λ(params: List[`λ`]), Some(_), _, given Option[Code]) =>
              val chʹ = updated(ch)
              given_Bindings --= params.map(_.asSymbol).filterNot(_.name.isEmpty)
              it.copy(channel = chʹ, code = recoded)(it.id)
            case it @ π(λ(ch: Symbol), λ(par: Symbol), Some(_), _, given Option[Code]) =>
              val chʹ = updated(ch)
              given_Bindings -= par
              it.copy(channel = chʹ, code = recoded)(it.id)
            case it @ π(λ(ch: Symbol), λ(arg: Symbol), None, _, given Option[Code]) =>
              it.copy(channel = updated(ch), name = updated(arg), code = recoded)(it.id)
            case it @ π(λ(ch: Symbol), _, None, _, given Option[Code]) =>
              it.copy(channel = updated(ch), code = recoded)(it.id)
            case it => it
          }
          `.`(end.update, it*)

        case ?:(((λ(lhs: Symbol), λ(rhs: Symbol)), m), t, f) =>
          ?:(((updated(lhs), updated(rhs)), m), t.update, f.map(_.update))

        case ?:(((λ(lhs: Symbol), rhs), m), t, f) =>
          ?:(((updated(lhs), rhs), m), t.update, f.map(_.update))

        case ?:(((lhs, λ(rhs: Symbol)), m), t, f) =>
          ?:(((lhs, updated(rhs)), m), t.update, f.map(_.update))

        case ?:(cond, t, f) =>
          ?:(cond, t.update, f.map(_.update))

        case !(Some(it @ τ(_, given Option[Code])), sum) =>
          `!`(Some(it.copy(code = recoded)(it.id)), sum.update)

        case !(Some(it @ π(λ(ch: Symbol), λ(par: Symbol), Some(_), _, given Option[Code])), sum) =>
          given Bindings = Bindings(bindings)
          val chʹ = updated(ch)
          given_Bindings -= par
          `!`(Some(it.copy(channel = chʹ, code = recoded)(it.id)), sum.update)

        case !(Some(it @ π(λ(ch: Symbol), λ(arg: Symbol), None, _, given Option[Code])), sum) =>
          `!`(Some(it.copy(channel = updated(ch), name = updated(arg), code = recoded)(it.id)), sum.update)

        case !(Some(it @ π(λ(ch: Symbol), _, None, _, given Option[Code])), sum) =>
          `!`(Some(it.copy(channel = updated(ch), code = recoded)(it.id)), sum.update)

        case it @ !(_, sum) =>
          it.copy(sum = sum.update)

        case it @ `⟦⟧`(_, _, sum, _, assignment) =>
          val assignmentʹ = assignment.map(_ -> updated(_).asSymbol)
          it.copy(sum = sum.update, assignment = assignmentʹ)

        case `{}`(identifier, pointers, agent, params*) =>
          val pointersʹ = pointers.map(updated(_).asSymbol)
          val paramsʹ = params
            .map {
              case λ(it: Symbol) => updated(it)
              case it => it
            }

          `{}`(identifier, pointersʹ, agent, paramsʹ*)

        case `(*)`(identifier, params*) =>
          val paramsʹ = params
            .map {
              case λ(it: Symbol) => updated(it)
              case it => it
            }

          `(*)`(identifier, paramsʹ*)
