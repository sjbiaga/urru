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
package parser

import scala.collection.mutable.{ LinkedHashSet => Set }

import scala.meta.{ Term, Type }

import generator.Meta.rateʹ

import Expression.Code
import StochasticPi.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion.Duplications


abstract class Calculus extends StochasticPi:

  def equation(using Duplications): Parser[Bind] =
    invocation(true)<~"=" >> {
      case (bind, bound) =>
        _code = -1
        _dir = None
        given Bindings = Bindings() ++ bound.map(_ -> Occurrence(None, pos()))
        choice ^^ {
          case (_sum, _free) =>
            val sum = _sum.flatten
            val free = _free ++ sum.capitals
            if (free &~ bound).nonEmpty
            then
              throw EquationFreeNamesException(bind.identifier, free &~ bound)
            if _traces.isDefined
            then
              bind -> sum.labelʹ(using bind.identifier -> _traces.get.getOrElse(""))
            else
              bind -> sum
        }
    }

  def choice(using Bindings, Duplications): Parser[(+, Names)] =
    rep1sep(parallel, "+") ^^ { _.unzip match
      case (it, ns) => `+`(nil, it*) -> ns.reduce(_ ++ _)
    }

  def parallel(using Bindings, Duplications): Parser[(∥, Names)] =
    rep1sep(sequential, "|") ^^ { _.unzip match
      case (it, ns) => ∥(it*) -> ns.reduce(_ ++ _)
    }

  def sequential(using bindings: Bindings, _ds: Duplications): Parser[(`.`, Names)] =
    given Bindings = Bindings(bindings)
    prefixes ~ ( leaf | choiceʹ ) ^^ {
      case (it, (bound, free)) ~ (end, freeʹ) =>
        bindings ++= binders
        `.`(end, it*) -> (free ++ (freeʹ &~ bound))
    }

  def choiceʹ(using Bindings, Duplications): Parser[(+, Names)] =
    opt( "("~>choice<~")" ) ^^ { _.getOrElse(`+`(nil) -> Names()) }

  def leaf(using bindings: Bindings, _ds: Duplications): Parser[(-, Names)] =
    "["~condition~"]"~choice ^^ { // (mis)match
      case _ ~ cond ~ _ ~ t =>
        ?:(cond._1, t._1, None) -> (cond._2 ++ t._2)
    } |
    "if"~condition~"then"~choice~"else"~choice ^^ { // if then else
      case _ ~ cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t._1, Some(f._1)) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    condition~"?"~choice~":"~choice ^^ { // Elvis operator
      case cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t._1, Some(f._1)) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    "!"~> opt( "."~>μ<~"." ) >> { // [guarded] replication
      case Some((π(λ(ch: Symbol), _, Some(cons), _, _), _)) if cons.nonEmpty =>
        throw ConsGuardParsingException(cons, ch.name)
      case Some(π @ (π(λ(ch: Symbol), λ(par: Symbol), Some(cons), _, _), _)) =>
        if ch == par
        then
          warn(throw GuardParsingException(ch.name))
        val bound = π._2._1
        BindingOccurrence(bound)
        choice ^^ {
          case (sum, free) =>
            val πʹ: π = {
              π._1 match
                case it: π =>
                  def idʹ: String = '!' + π._1.υidυ
                  it.copy()(idʹ)
            }
            `!`(Some(πʹ), sum) -> ((free &~ bound) ++ π._2._2)
        }
      case Some(μ) =>
        choice ^^ {
          case (sum, free) =>
            val μʹ: μ = {
              μ._1 match
                case it: π =>
                  def idʹ: String = '!' + μ._1.υidυ
                  it.copy()(idʹ)
                case _ => μ._1
            }
            `!`(Some(μʹ), sum) -> (free ++ μ._2._2)
        }
      case _ =>
        choice ^^ {
          case (sum, free) =>
            `!`(None, sum) -> free
        }
    } |
    capital |
    invocation() |
    instantiation

  def capital: Parser[(`{}`, Names)]

  def instantiation(using Bindings, Duplications): Parser[(`⟦⟧`, Names)]

  def prefixes(using Bindings): Parser[(List[Pre], (Names, Names))] =
    rep(prefix) ^^ { _.unzip match
      case (it, _2) => _2.unzip match
        case (bs, names) =>
          val free = Names()
          names
            .zipWithIndex
            .foreach { (ns, i) =>
              val bound = bs
                .take(i)
                .reduceOption(_ ++ _)
                .getOrElse(Names())
              free ++= ns -- bound
            }
          val bound = bs.reduceOption(_ ++ _).getOrElse(Names())
          it -> (bound, free)
    }

  def prefix(using bindings: Bindings): Parser[(Pre, (Names, Names))] =
    "ν"~>"("~>names<~")" ^^ { // restriction
      case it if !it.forall(_._1.isSymbol) =>
        throw PrefixChannelsParsingException(it.filterNot(_._1.isSymbol).map(_._1)*)
      case it => it.unzip match
        case (λs, bs) =>
          val bound = bs.reduce(_ ++ _)
          BindingOccurrence(bound)
          ν(λs.map(_.asSymbol.name)*) -> (bound, Names())
    } |
    μ<~"." ^^ {
      case it @ (_, (bound, _)) =>
        BindingOccurrence(bound)
        it
    }

  def condition: Parser[(((λ, λ), Boolean), Names)] = "("~>condition<~")" |
    name~("="|"≠")~name ^^ {
      case (lhs, free_lhs) ~ mismatch ~ (rhs, free_rhs) =>
        (lhs -> rhs -> (mismatch != "=")) -> (free_lhs ++ free_rhs)
    }

  def invocation(equation: Boolean = false): Parser[(`(*)`, Names)] =
    IDENT ~ opt( "("~>names<~")" ) ^^ {
      case identifier ~ Some(params) if equation && !params.forall(_._1.isSymbol) =>
        throw EquationParamsException(identifier, params.filterNot(_._1.isSymbol).map(_._1)*)
      case "Self" ~ Some(params) =>
        self += _code
        `(*)`("Self_" + _code, params.map(_._1)*) -> params.map(_._2).reduce(_ ++ _)
      case "Self" ~ _ =>
        self += _code
        `(*)`("Self_" + _code) -> Names()
      case identifier ~ Some(params) =>
        identifier match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(identifier, params.map(_._1)*) -> params.map(_._2).reduce(_ ++ _)
      case identifier ~ _ =>
        identifier match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(identifier) -> Names()
    }

  /**
   * Agent identifiers start with upper case.
   * @return
   */
  def IDENT: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isUpperCase)("agent identifier expected but '" + _ + "' found"),
          elem("agent identifier part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)


object Calculus:

  private val qual_r = "[{][^}]*[}]".r

  type Bind = (`(*)`, +)

  export Pre.*
  export AST.*

  enum Pre:

    case ν(names: String*) // forcibly

    case τ(override val rate: Option[Any],
           code: Option[Code])(id: => String)
        extends Pre with Act(() => id)

    case π(channel: λ,
           name: λ,
           polarity: Option[String],
           override val rate: Option[Any],
           code: Option[Code])(id: => String)
        extends Pre with Act(() => id)

    override def toString: String = this match
      case ν(names*) => names.mkString("ν(", ", ", ")")
      case π(channel, name, polarity, _, _) =>
        if polarity.isDefined
        then "" + channel + s"${polarity.get}(" + name + ")."
        else "" + channel + "<" + name + ">."
      case _ => "τ."

  enum AST:

    case +(override val enabled: Actions,
           choices: AST.∥ *) extends AST with Sum

    case ∥(components: AST.`.`*)

    case `.`(end: AST.+ | -, prefixes: Pre*)

    case ?:(cond: ((λ, λ), Boolean), t: AST.+, f: Option[AST.+])

    case !(guard: Option[μ], sum: AST.+)

    case `⟦⟧`(definition: Definition,
              variables: Names,
              sum: AST.+,
              xid: String = null,
              assignment: Set[(Symbol, Symbol)] = Set.empty)

    case `{}`(identifier: String,
              pointers: List[Symbol],
              agent: Boolean = false,
              params: λ*)

    case `(*)`(identifier: String,
               params: λ*)

    override def toString: String = this match
      case ∅() => "()"
      case +(choices*) => choices.mkString(" + ")

      case ∥(components*) => components.mkString(" | ")

      case `.`(∅()) => "()"
      case `.`(∅(), prefixes*) => prefixes.mkString(" ") + " ()"
      case `.`(end: +, prefixes*) =>
        prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + "(" + end + ")"
      case `.`(end, prefixes*) =>
        prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + end

      case ?:(cond, t, f) =>
        val test = "" + cond._1._1 + (if cond._2 then " ≠ " else " = ") + cond._1._2
        if f.isEmpty
        then
          "[ " + test + " ] " + t
        else
          "if " + test + " " + t + " else " + f.get

      case !(guard, sum) => "!" + guard.map("." + _).getOrElse("") + sum

      case `⟦⟧`(definition, variables, sum, _, assignment) =>
        val vars = if (variables.isEmpty)
                   then
                     ""
                   else
                     variables.map {
                       case it if assignment.exists(_._1 == it) =>
                         s"${it.name} = ${assignment.find(_._1 == it).get._2.name}"
                       case it => it.name
                     }.mkString("{", ", ", "}")
        s"""${Definition(definition.code, definition.term)}$vars = $sum"""

      case `{}`(identifier, pointers, agent, params*) =>
        val ps = if agent then params.mkString("(", ", ", ")") else ""
        s"""$identifier$ps{${pointers.map(_.name).mkString(", ")}}"""

      case `(*)`(identifier, params*) =>
        val args = params.map(_.toTerm).toList
        Term.Apply(Term.Name(identifier), Term.ArgClause(args)).toString

  object ∅ :
    def unapply(self: AST): Boolean = self match
      case sum: + => sum.isVoid
      case _ => false

  case class λ(`val`: Any)(using val `type`: Option[(Type, Option[Type])] = None):
    val isSymbol: Boolean = `val`.isInstanceOf[Symbol]
    def asSymbol: Symbol = `val`.asInstanceOf[Symbol]

    type Kind = `val`.type

    val kind: String = `val` match
      case _: Symbol => "channel name"
      case _: BigDecimal => "decimal number"
      case _: Boolean => "True False"
      case _: String => "string literal"
      case _: Term => "Scalameta Term"
      case _ => "polyadic names"

    def toTerm: Term =
      import scala.meta._
      import dialects.Scala3
      `val` match
        case it: Symbol => Term.Name(it.name)
        case it: BigDecimal => Term.Apply(Term.Name("BigDecimal"), Term.ArgClause(Lit.String(it.toString)::Nil))
        case it: Boolean => Lit.Boolean(it)
        case it: String => Lit.String(it)
        case it: Term => it

    override def toString: String = `val` match
      case it: Symbol => it.name
      case it: BigDecimal => "" + it
      case it: Boolean => it.toString.capitalize
      case it: String => "\"" + it + "\""
      case it: Term => "/*" + it + "*/"
      case it: List[`λ`] => it.mkString(", ")


  // exceptions

  import Expression.ParsingException

  abstract class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationParamsException(identifier: String, params: λ*)
      extends EquationParsingException(s"""The "formal" parameters (${params.mkString(", ")}) are not names in the left hand side of $identifier""")

  case class EquationFreeNamesException(identifier: String, free: Names)
      extends EquationParsingException(s"""The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $identifier""")

  case class PrefixChannelsParsingException(names: λ*)
      extends PrefixParsingException(s"""${names.mkString(", ")} are not channel names but ${names.map(_.kind).mkString(", ")}""")

  case class GuardParsingException(name: String)
      extends PrefixParsingException(s"$name is both the channel name and the binding parameter name in an input guard")

  case class ConsGuardParsingException(cons: String, name: String)
      extends PrefixParsingException(s"A name $name that knows how to CONS (`$cons') is used as replication guard")


  // functions

  extension (sum: +)
    @annotation.tailrec
    def isVoid: Boolean = sum match
      case +(_) => true
      case +(_, ∥(`.`(sum: +))) => sum.isVoid
      case _ => false

  extension [T <: AST](ast: T)

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case +(_, ∥(`.`(sum: +)), it*) =>
          val lhs = sum.flatten
          val rhs = `+`(nil, it*).flatten
          `+`(nil, (lhs.choices ++ rhs.choices).filterNot(`+`(nil, _).isVoid)*)

        case +(_, par, it*) =>
          val lhs: + = `+`(nil, par.flatten)
          val rhs = `+`(nil, it*).flatten
          `+`(nil, (lhs.choices ++ rhs.choices).filterNot(`+`(nil, _).isVoid)*)

        case ∥(`.`(+(_, par)), it*) =>
          val lhs = par.flatten
          val rhs = ∥(it*).flatten
          ∥((lhs.components ++ rhs.components)*)

        case ∥(seq, it*) =>
          val lhs: ∥ = ∥(seq.flatten)
          val rhs = ∥(it*).flatten
          ∥((lhs.components ++ rhs.components)*)

        case `.`(+(_, ∥(`.`(end, ps*))), it*) =>
          `.`(end, (it ++ ps)*).flatten

        case `.`(end, it*) =>
          `.`(end.flatten, it*)

        case ?:(cond, t, f) =>
          ?:(cond, t.flatten, f.map(_.flatten))

        case !(None, sum) =>
          sum.flatten match
            case +(_, ∥(`.`(end: !))) => end
            case it => `!`(None, it)

        case it @ !(_, sum) =>
          it.copy(sum = sum.flatten)

        case _ => ast

    def labelʹ(using (String, String)): T =

      ast match

        case `+`(_, ∥(`.`(!(Some(_), _)))) =>
          ast.label("+0")

        case _ =>
          ast.label("")

    def label(l: String)(using (String, String)): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      object Sum:
        inline implicit def lʹ(i: Int): String = l + "+" + i

      object Par:
        inline implicit def lʹ(i: Int): String = l + "∥" + i

      inline def idʹ(id: => String, ch: String, p: String, r: Any): String =
        val (identifier, filename) = summon[(String, String)]
        id + "," + ch + "," + p + "," + l + "," + rateʹ(r) + "," + identifier + "," + filename

      val relabelled: Seq[Pre] => Seq[Pre] =
        _.map {
          case it: τ =>
            it.copy()(idʹ(it.id, "τ", "", it.rate.get))
          case it: π
              if it.polarity.map(_.isEmpty).getOrElse(true) =>
            val polarity = it.polarity.fold(false)(_ => true)
            val name = it.channel.asSymbol.name
            it.copy()(idʹ(it.id, name, polarity.toString, it.rate.get))
          case it => it
        }

      inline def relabelledʹ(it: Option[μ]): Option[μ] =
        relabelled(it.toSeq).headOption.asInstanceOf[Option[μ]]

      ast match

        case ∅() => ast

        case +(_, ∥(it: `.`)) if !it.prefixes.exists { case Act(it) => it } =>
          `+`(nil, ∥(it.label(l)))

        case +(_, ∥(it*)) =>
          import Par.*
          `+`(nil, ∥(it.zipWithIndex.map(_.label(_))*))

        case +(_, it*) =>
          import Sum.*
          `+`(nil, it.zipWithIndex.map(_.label(_))*)

        case ∥(it*) =>
          ∥(it.map(_.label(l))*)

        case `.`(end, it*) =>
          `.`(end.label(l), relabelled(it)*)

        case ?:(cond, t, Some(f)) =>
          import Sum.*
          ?:(cond, t.label(0), Some(f.label(1)))

        case ?:(cond, t, _) =>
          ?:(cond, t.label(l), None)

        case !(None, sum) =>
          `!`(None, sum.label(l))

        case !(it, sum) =>
          `!`(relabelledʹ(it), sum.label(l))

        case it @ `⟦⟧`(_, _, sum, _, _) =>
          it.copy(sum = sum.label(l))

        case _ => ast
