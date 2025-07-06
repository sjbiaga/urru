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

import scala.collection.mutable.{
  LinkedHashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}

import scala.meta.Term

import Expression.Code
import StochasticPi.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion.{ replace, Duplications, Substitution }


abstract class Encoding extends Calculus:

  def definition(using Duplications): Parser[Option[Define]] =
    template ~ opt( "("~>names<~")" ) ~ opt( pointers ) >> {
      case (term, _parameters) ~ _constants ~ _variables =>
        val constants = _constants.map(_.map(_._2).reduce(_ ++ _)).getOrElse(Names())
        val variables = _variables.map(_._2).getOrElse(Names())
        val parameters = _parameters.filterNot(_.name.charAt(0).isUpper)
        if (parameters & constants).nonEmpty
        || (variables & parameters).nonEmpty
        || (constants & variables).nonEmpty
        then
          throw DefinitionParametersException(_code)
        var bound = _parameters ++ constants ++ variables
        given Bindings = Bindings() ++
                         bound
                           .filterNot(_.name.charAt(0).isUpper)
                           .map { it => it -> (if parameters.contains(it) then pos_() else pos()) }
                           .map(_ -> Occurrence(None, _))
        if _dir.isDefined
        then
          Directive()
          Success(Option.empty[Define], _)
        else
          "="~> choice ^^ {
            case (_sum, _free) =>
              val sum = _sum.flatten
              val free = _free ++ sum.capitals
              val (constantsʹ, variablesʹ, bindingsʹ, sumʹ) =
                sum match
                  case +(_, ∥(`.`(exp @ `⟦⟧`(Definition(_, _, constantsʹ, _, _), variablesʹ, _, _, assignmentʹ))))
                      if assignmentʹ.size < variablesʹ.size =>
                    val constantsʹʹ = constantsʹ &~ constants
                    val pointersʹ = variablesʹ.map(_.name.replaceAll("_υ.*υ", "")).map(Symbol(_))
                    val variablesʹʹ = (pointersʹ &~ variables)
                                        .map { it => variablesʹ.find(_.name.startsWith(it.name)).get }
                    val pointersʹʹ = pointersʹ.drop(assignmentʹ.size)
                    if variablesʹʹ.size != pointersʹʹ.size
                    then
                      warn(throw EncodingAliasAbandonedException(_code))
                      (constants, variables, given_Bindings, sum)
                    else
                      val assignmentʹʹ = assignmentʹ ++ (variablesʹʹ zip pointersʹʹ)
                      val boundʹ = constantsʹʹ ++ pointersʹʹ
                      bound ++= boundʹ
                      val bindingsʹ = boundʹ.map(_ -> Occurrence(None, pos()))
                      val expʹ: `⟦⟧` = exp.copy(assignment = assignmentʹʹ)
                      val sumʹ: `+` = `+`(nil, (∥(`.`(expʹ))))
                      (constants ++ constantsʹʹ, variables ++ pointersʹʹ, given_Bindings ++ bindingsʹ, sumʹ)
                  case _ =>
                    (constants, variables, given_Bindings, sum)
              if (free &~ bound).nonEmpty
              then
                throw DefinitionFreeNamesException(_code, free &~ bound)
              if parameters.size == _parameters.size
              then
                if !_exclude
                then
                  val bind: `(*)` = `(*)`("Self_" + _code, bound.map(λ(_)).toSeq*)
                  if _traces.isDefined
                  then
                    eqtn :+= bind -> sumʹ.labelʹ(using bind.identifier -> _traces.get.getOrElse(""))
                  else
                    eqtn :+= bind -> sumʹ
              Some {
                Macro(parameters.toList, _parameters.size, constantsʹ, variablesʹ, bindingsʹ, sumʹ)
                ->
                Definition(_code, term, constants, variables, sum)
              }
          }
      }

  def instantiation(using bindings: Bindings, duplications: Duplications): Parser[(`⟦⟧`, Names)] =
    given Bindings = Bindings(bindings)
    regexMatch("""⟦(\d*)""".r) >> { m =>
      if _nest == 0 then _cache.clear
      nest(true)
      val grp1 = m.group(1)
      val code = if grp1.isEmpty
                 then
                   val def1 = defn.filter { (_, it) => it.size == 1 && it.head._2.term.isEmpty }
                   if def1.size == 1
                   then def1.head._2.head._2.code
                   else -1
                 else
                   grp1.toInt
      defn.get(code) match {
        case Some(it) => it
        case _ if grp1.nonEmpty || defn.isEmpty =>
          throw NoDefinitionException(code max 0)
        case _ =>
          defn.values.reduce(_ ++ _).filterNot(_._2.term.isEmpty)
      } match
        case ((_, definition @ Definition(_, None, _, _, _))) :: Nil =>
          (choice <~ s"$grp1⟧") ~ opt( pointers ) ^^ {
            case (sum, free) ~ ps =>
              val xid = χ_id
              duplications += xid -> (false, Map())
              (`⟦⟧`(definition, definition.variables, sum.flatten, xid) -> free) -> ps
          }
        case it =>
          (instance(it, s"$grp1⟧") <~ s"$grp1⟧") ~ opt( pointers ) ^^ {
            case exp ~ ps =>
              exp -> ps
          }
    } ^^ {
      case ((exp @ `⟦⟧`(Definition(_, _, constants, _, _), variables, _, _, _), free), _pointers) =>
        try
          given MutableList[(Symbol, λ)]()
          val pointers = _pointers.map(_._1.map(renamed(_).asSymbol)).getOrElse(Nil)
          val assignment = variables zip pointers
          given Names()
          val expʹ = exp.copy(assignment = assignment).rename()(id)(free)
          bindings ++= purged
          nest(false)
          expʹ -> (free ++ constants)
        catch
          case it: NoBPEx => throw NoBindingParsingException(_code, _nest, it.getMessage)
          case it => throw it
    }

  def instance(defs: List[Define], end: String)
              (using Bindings, Duplications): Parser[(`⟦⟧`, Names)]

  def pointers: Parser[(List[Symbol], Names)] =
    "{"~>names<~"}" ^^ {
      case ps if !ps.forall(_._1.isSymbol) =>
        throw PointersParsingException(ps.filterNot(_._1.isSymbol).map(_._1)*)
      case ps => ps.unzip match
        case (λs, ns) =>
          λs.map(_.asSymbol) -> ns.reduce(_ ++ _)
    }

  def capital: Parser[(`{}`, Names)] =
    IDENT ~ pointers ^^ {
      case identifier ~ ps =>
        `{}`(identifier, ps._1) -> ps._2
    } |
    IDENT <~"{"<~"}" ^^ (`{}`(_, Nil) -> Names()) |
    IDENT ~ ("("~>opt( names )<~")") ~ pointers ^^ {
      case identifier ~ Some(params) ~ ps =>
        `{}`(identifier, ps._1, true, params.map(_._1)*) -> (ps._2 ++ params.map(_._2).reduce(_ ++ _))
      case identifier ~ _ ~ ps =>
        `{}`(identifier, ps._1, true) -> ps._2
    } |
    IDENT ~ ("("~>opt( names )<~")") <~"{"<~"}" ^^ {
      case identifier ~ Some(params) =>
        `{}`(identifier, Nil, true, params.map(_._1)*) -> params.map(_._2).reduce(_ ++ _)
      case identifier ~ _ =>
        `{}`(identifier, Nil, true) -> Names()
    }

  protected final val _cache = Map[CacheKey, CacheValue]()

  private object Directive:

    private def canonical: String => String =
      case "werr" => "errors"
      case "dups" => "duplications"
      case it => it

    private def key: String => Boolean = canonical andThen {
      case "errors" | "duplications"
         | "exclude" | "include"
         | "parallelism" | "traces" => true
      case _ => false
    }

    private def boolean: Boolean =
      _dir.get._2 match
        case it: String =>
          it.toLowerCase match
            case "0" | "off" | "false" => false
            case "1" | "on" | "true" => true
            case _ => throw DirectiveValueParsingException(_dir.get, "a boolean")
        case _ => throw DirectiveValueParsingException(_dir.get, "a boolean")

    private def number: Double =
      _dir.get._2 match
        case it: String =>
          try
            it.toDouble
          catch
            case _: NumberFormatException =>
              throw DirectiveValueParsingException(_dir.get, "a number")

    private def file: Option[String] =
      _dir.get._2 match
        case it: String if it.toLowerCase == "console" => None
        case _: String => Some(string("<console> or a filename"))
        case _ => throw DirectiveValueParsingException(_dir.get, "<console> or a filename")

    private def keys: Set[String] =
      _dir.get._2 match
        case it: String if key(it) => Set(canonical(it))
        case it: List[String] if it.forall(key) => Set.from(it.map(canonical))
        case _ => throw DirectiveValueParsingException(_dir.get, "a comma separated list of valid keys")

    private def string(`type`: String = "a string"): String =
      _dir.get._2 match
        case it: String
            if (it.startsWith("\"") ||  it.startsWith("'"))
            && it.endsWith(s"${it.charAt(0)}") && it.length >= 2 =>
          it.substring(1, it.length-1)
        case _ => throw DirectiveValueParsingException(_dir.get, s"${`type`}")

    def apply(): Unit =

      canonical(_dir.get._1.toLowerCase) match

        case "errors" =>
          _werr = boolean

        case "duplications" =>
          _dups = boolean

        case "exclude" =>
          _exclude = boolean

        case "include" =>
          _exclude = !boolean

        case "parallelism" =>
          _par = 1 max number.toInt

        case "traces" =>
          try
            if boolean
            then
              _traces = Some(None)
            else
              _traces = None
          catch _ =>
             try
               _traces = Some(file)
             catch _ =>
               throw DirectiveValueParsingException(_dir.get, "a boolean, <console> or a filename")

        case "push" =>
          try
            if boolean
            then
              _dirs ::= Map("errors" -> _werr,
                            "duplications" -> _dups,
                            "exclude" -> _exclude,
                            "parallelism" -> _par,
                            "traces" -> _traces)
          catch _ =>
            _dirs ::= Map.from {
              keys.map {
                case it @ "errors" => it -> _werr
                case it @ "duplications" => it -> _dups
                case "exclude" | "include" => "exclude" -> _exclude
                case it @ "parallelism" => it -> _par
                case it @ "traces" => it -> _traces
              }
            }

        case "pop" =>
          if boolean
          then
            _dirs.head.foreach {
              case ("errors", it: Boolean) => _werr = it
              case ("duplications", it: Boolean) => _dups = it
              case ("exclude", it: Boolean) => _exclude = it
              case ("parallelism", it: Int) => _par = it
              case ("traces", it: Option[Option[String]]) => _traces = it
              case _ => ???
            }
            _dirs = _dirs.tail

          if _dirs.isEmpty
          then
            val dir = _dir
            _dir = Some("push" -> "1")
            this()
            _dir = dir

        case _ => throw DirectiveKeyParsingException(_dir.get)


object Encoding:

  type Define = (Macro, Definition)

  type Fresh = (Definition, (Int, List[Option[Symbol]]))

  type CacheKey = ((Seq[Long], (String, Either[String, String])), Int)

  private type CacheValue = (+ | `⟦⟧`, ((Any, Any), Any), Names, Bindings, Encoding#Input)

  case class Macro(parameters: List[Symbol],
                   arity: Int,
                   constants: Names,
                   variables: Names,
                   bindings: Bindings,
                   sum: +):
    def apply(code: Int, term: Term, dups: Boolean)
             (id: => String, χ_id: => String)
             (using Duplications): Fresh =
      given Bindings = Bindings(bindings)
      given MutableList[(Symbol, λ)]()
      val variablesʹ = variables
        .map { it =>
          val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
          given_Bindings(υidυ) = given_Bindings(it)
          given_Bindings -= it
          given_MutableList_Symbol_λ.prepend(it -> λ(υidυ))
          υidυ
        }
      given Names()
      val sumʹ = sum.rename(expansion = true, dups)(id, χ_id)()
      val shadows = (
        parameters.map(_ -> None).toMap
        ++
        purged.collect { case (it, Binder(υidυ)) => it -> Some(υidυ) }
      ) .toList
        .sortBy { (it, _) => parameters.indexOf(it) }
        .map(_._2)
      Definition(code, Some(term), constants, variablesʹ, sumʹ)
      ->
      (arity - shadows.count(_.nonEmpty) -> shadows)

  case class Definition(code: Int,
                        term: Option[Term],
                        constants: Names,
                        variables: Names,
                        sum: +):
    def apply(_code: Int, nest: Int, dups: Boolean,
              duplicated: (Bindings, Duplications) ?=> String => Term => Unit)
             (id: => String)
             (using duplications: Duplications)
             (using Bindings, Substitution): `⟦⟧` =
      if dups
      then
        val ids = MutableList[String]()
        val idsʹ = MutableList.from {
          duplications.flatMap {
            case (xid, (true, _)) => Some(xid)
            case _ => None
          }
        }
        given (+ | `⟦⟧` => + | `⟦⟧`) = { ast =>
          lazy val count: AST => Unit =
            case ∅() =>
            case +(_, it*) => it.foreach(count)
            case ∥(it*) => it.foreach(count)
            case `.`(end, _*) => count(end)
            case ?:(_, t, f) => count(t); f.foreach(count)
            case !(_, sum) => count(sum)
            case it: `⟦⟧` if ids.contains(it.xid) =>
              duplications += it.xid -> (true -> duplications(it.xid)._2)
              count(it.sum)
            case it: `⟦⟧` if idsʹ.contains(it.xid) =>
              count(it.sum)
            case it: `⟦⟧` =>
              ids += it.xid
              count(it.sum)
            case _ =>
          count(ast)
          try
            given MutableList[(Symbol, λ)]()
            given Names()
            ast.rename(false, dups, duplicated)(id)()
          catch
            case it: NoBPEx => throw NoBindingParsingException(_code, nest, it.getMessage)
            case it => throw it
        }
        lazy val reset: AST => Unit =
          case ∅() =>
          case +(_, it*) => it.foreach(reset)
          case ∥(it*) => it.foreach(reset)
          case `.`(end, _*) => reset(end)
          case ?:(_, t, f) => reset(t); f.foreach(reset)
          case !(_, sum) => reset(sum)
          case it: `⟦⟧` if ids.contains(it.xid) =>
            duplications += it.xid -> (false -> duplications(it.xid)._2)
            reset(it.sum)
          case it: `⟦⟧` => reset(it.sum)
          case _ =>
        val exp: `⟦⟧` = `⟦⟧`(this, variables, sum.replace.flatten)
        reset(exp.sum)
        exp
      else
        given (+ | `⟦⟧` => + | `⟦⟧`) = { ast =>
          try
            given MutableList[(Symbol, λ)]()
            given Names()
            ast.rename(false, dups, duplicated)(id)()
          catch
            case it: NoBPEx => throw NoBindingParsingException(_code, nest, it.getMessage)
            case it => throw it
        }
        `⟦⟧`(this, variables, sum.replace.flatten)

    override def toString: String = Definition(code, term)
      + (if constants.isEmpty then "" else constants.map(_.name).mkString("(", ", ", ")"))
      + (if variables.isEmpty then "" else variables.map(_.name).mkString("{", ", ", "}"))
      + " = " + sum

  object Definition:

    def apply(code: Int, term: Option[Term]): String =
      term match
        case Some(term) => if code == 0 then s"⟦ $term ⟧" else s"⟦$code $term $code⟧"
        case _ => if code == 0 then s"⟦ ⟧" else s"⟦$code $code⟧"

  final case class Position(counter: Long, binds: Boolean)

  final case class Occurrence(shadow: Symbol | Option[Symbol], position: Position):
    val isBinding = position.binds && position.counter < 0

  object Binder:
    def apply(self: Occurrence)(υidυ: Symbol) = Occurrence(υidυ, self.position)
    def unapply(self: Occurrence): Option[Symbol] =
      self.shadow match
        case it: Symbol => Some(it)
        case _ => None

  object Shadow:
    def apply(self: Occurrence)(υidυ: Symbol) = self.copy(shadow = Some(υidυ))
    def unapply(self: Occurrence): Option[Symbol] =
      self.shadow match
        case it @ Some(_) => it
        case _ => None

  type Bindings = Map[Symbol, Occurrence]

  object Bindings:
    def apply(): Bindings = Map()
    def apply(bindings: Bindings): Bindings = Map.from(bindings)


  // exceptions

  import Expression.ParsingException

  class PointersParsingException(names: λ*)
      extends PrefixChannelsParsingException(names*)

  case class NoDefinitionException(code: Int)
      extends ParsingException(s"No definition for encoding $code")

  case class DefinitionParametersException(code: Int)
      extends EquationParsingException(s"The parameters, constants, and variables must all be different in the left hand side of encoding $code")

  case class DefinitionFreeNamesException(code: Int, free: Names)
      extends EquationParsingException(s"""The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of encoding $code""")

  abstract sealed class BindingParsingException(code: Int, nest: Int, msg: String, cause: Throwable = null)
      extends ParsingException(msg
                                 + s" at nesting level #$nest"
                                 + (if code >= 0 then s" in the right hand side of encoding $code" else ""), cause)

  case class NoBindingParsingException(code: Int, nest: Int, name: String)
      extends BindingParsingException(code, nest, s"No binding for $name")

  final private class NoBPEx(name: String) extends Throwable(name)

  case class UniquenessBindingParsingException(code: Int, nest: Int, name: Symbol, hardcoded: Boolean)
      extends BindingParsingException(code, nest, s"""A binding name (${name.name}) does not correspond to a unique ${if hardcoded then "hardcoded" else "encoded"} binding occurrence, but is duplicated""")

  case class NonParameterBindingParsingException(code: Int, nest: Int, name: Symbol, hardcoded: Boolean)
      extends BindingParsingException(code, nest, s"""A binding name (${name.name}) in ${if hardcoded then "a hardcoded" else "an encoded"} binding occurrence does not correspond to a parameter""")

  case class EncodingAliasAbandonedException(code: Int)
      extends ParsingException(s"An encoding alias was abandoned as the right hand side of encoding $code")

  abstract sealed class DirectiveParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  private object DirectiveParsingException:
    def apply(dir: (String, String | List[String])): String =
      dir._2 match
        case it: String => s"⟦ ${dir._1} = $it ⟧"
        case it: List[String] => s"""⟦ ${dir._1} = ${it.mkString("(", ",", ")")} ⟧"""

  case class DirectiveKeyParsingException(dir: (String, String | List[String]))
      extends DirectiveParsingException(s"The key in the directive ${DirectiveParsingException(dir)} is not valid")

  case class DirectiveValueParsingException(dir: (String, String | List[String]), `type`: String)
      extends DirectiveParsingException(s"The value in the directive ${DirectiveParsingException(dir)} is not ${`type`}")


  // functions

  def renamed(it: Symbol)
             (using refresh: MutableList[(Symbol, λ)])
             (using bindings: Bindings): λ =
    refresh.find(_._1 == it) match
      case Some((_, r)) => r
      case _ =>
        bindings.find { case (`it`, Binder(_) | Shadow(_)) => true case _ => false } match
          case Some((_, Binder(it))) => λ(it)
          case Some((_, Shadow(it))) => λ(it)
          case _ =>
            bindings.find { case (`it`, _) | (_, Shadow(`it`)) => true case _ => false } match
              case Some(_) => λ(it)
              case _ => throw NoBPEx(it.name)

  def recoded(free: Names)
             (using code: Option[Code])
             (using MutableList[(Symbol, λ)])
             (using Bindings)
             (using bound: Names): Option[Code] =
    code.map { (_, orig) =>
      val term = Expression(orig)._1
      val (codeʹ, names) = Expression.recode(term)
      free ++= names.filterNot(bound.contains(_))
      codeʹ
    }

  def purged(using bindings: Bindings): Bindings =
    bindings.flatMap {
      case (name, Shadow(it)) =>
        bindings.find { case (`it`, Binder(_) | Shadow(_)) => true case _ => false } match
          case Some((_, occurrence)) =>
            Some(name -> (it -> occurrence))
          case _ =>
            None
      case _ =>
        None
    }.foreach {
      case (name, (it, occurrence)) =>
        bindings -= it
        bindings += name -> occurrence
    }
    binders

  inline def binders(using bindings: Bindings): Bindings =
    bindings.filter(_._2.isBinding)


  extension [T <: AST](ast: T)

    def capitals: Names =

      ast match

        case ∅() => Names()

        case +(_, it*) => it.map(_.capitals).reduce(_ ++ _)

        case ∥(it*) => it.map(_.capitals).reduce(_ ++ _)

        case `.`(end, _*) =>
          end.capitals

        case ?:(_, t, f) =>
          t.capitals ++ f.map(_.capitals).getOrElse(Names())

        case !(_, sum) =>
          sum.capitals

        case `⟦⟧`(_, _, sum, _, _) =>
          sum.capitals

        case `{}`(identifier, _, false) => Set(Symbol(identifier))

        case _ => Names()


    def rename(expansion: Boolean = false, dups: Boolean = false,
               duplicated: (Bindings, Duplications) ?=> String => Term => Unit = { (_, _) ?=> { _ => { _ => } } })
              (id: => String, χ_id: => String = null)
              (free: Names = Names())
              (using bindings: Bindings)
              (using duplications: Duplications)
              (using refresh: MutableList[(Symbol, λ)])
              (using bound: Names): T =

      def rebind(it: Symbol)
                (using bound: Names): λ =
        val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
        bindings.find { case (_, Shadow(`it`)) => true case _ => false } match
          case Some((_, occurrence)) if expansion && occurrence.isBinding =>
            bindings += it -> Binder(occurrence)(υidυ)
          case Some((_, occurrence)) =>
            bindings += it -> Shadow(occurrence)(υidυ)
          case _ =>
            refresh.prepend(it -> λ(υidυ))
        bound += υidυ
        λ(υidυ)

      inline def rename[S <: AST](ast: S)(using Names): S =
        ast.rename(expansion, dups, duplicated)(id, χ_id)(free)

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case +(_, it*) =>
          `+`(nil, it.map(rename(_))*)

        case ∥(it*) =>
          ∥(it.map(rename(_))*)

        case `.`(end, _it*) =>
          val n = refresh.size
          given Names = Names(bound)
          val it = _it.map {
            case ν(_names*) =>
              val names = _names.map(Symbol(_)).map(rebind(_))
              ν(names.map(_.asSymbol.name)*)
            case it @ τ(_, given Option[Code]) =>
              it.copy(code = recoded(free))(it.id)
            case it @ π(λ(ch: Symbol), λ(params: List[`λ`]), Some(_), _, given Option[Code]) =>
              val paramsʹ = params.map(_.asSymbol).filterNot(_.name.isEmpty).map(rebind(_))
              it.copy(channel = renamed(ch), name = λ(paramsʹ), code = recoded(free))(it.id)
            case it @ π(λ(ch: Symbol), λ(par: Symbol), Some(_), _, given Option[Code]) =>
              it.copy(channel = renamed(ch), name = rebind(par), code = recoded(free))(it.id)
            case it @ π(λ(ch: Symbol), λ(arg: Symbol), None, _, given Option[Code]) =>
              it.copy(channel = renamed(ch), name = renamed(arg), code = recoded(free))(it.id)
            case it @ π(λ(ch: Symbol), _, None, _, given Option[Code]) =>
              it.copy(channel = renamed(ch), code = recoded(free))(it.id)
            case it => it
          }
          val endʹ = rename(end)
          refresh.dropInPlace(refresh.size - n)
          `.`(endʹ, it*)

        case ?:(((λ(lhs: Symbol), λ(rhs: Symbol)), m), t, f) =>
          ?:(((renamed(lhs), renamed(rhs)), m), rename(t), f.map(rename(_)))

        case ?:(((λ(lhs: Symbol), rhs), m), t, f) =>
          ?:(((renamed(lhs), rhs), m), rename(t), f.map(rename(_)))

        case ?:(((lhs, λ(rhs: Symbol)), m), t, f) =>
          ?:(((lhs, renamed(rhs)), m), rename(t), f.map(rename(_)))

        case ?:(cond, t, f) =>
          ?:(cond, rename(t), f.map(rename(_)))

        case !(Some(it @ τ(_, given Option[Code])), sum) =>
          `!`(Some(it.copy(code = recoded(free))(it.id)), rename(sum))

        case !(Some(it @ π(λ(ch: Symbol), λ(par: Symbol), Some(_), _, given Option[Code])), sum) =>
          val n = refresh.size
          given Names = Names(bound)
          val π = it.copy(channel = renamed(ch), name = rebind(par), code = recoded(free))(it.id)
          val sumʹ = rename(sum)
          refresh.dropInPlace(refresh.size - n)
          `!`(Some(π), sumʹ)

        case !(Some(it @ π(λ(ch: Symbol), λ(arg: Symbol), None, _, given Option[Code])), sum) =>
          val π = it.copy(channel = renamed(ch), name = renamed(arg), code = recoded(free))(it.id)
          `!`(Some(π), rename(sum))

        case !(Some(it @ π(λ(ch: Symbol), _, None, _, given Option[Code])), sum) =>
          val π = it.copy(channel = renamed(ch), code = recoded(free))(it.id)
          `!`(Some(π), rename(sum))

        case it @ !(_, sum) =>
          it.copy(sum = rename(sum))

        case it @ `⟦⟧`(Definition(_, term, _, _, _), variables, sum, xid, assignment) =>
          if dups then term.foreach(duplicated(xid))
          val n = refresh.size
          val assignmentʹ = assignment
                              .map { (it, pt) =>
                                val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
                                refresh.prepend(it -> λ(υidυ))
                                υidυ -> renamed(pt).asSymbol
                              }
          val variablesʹ = assignmentʹ.map(_._1)
                        ++ variables
                             .drop(assignment.size)
                             .map { it =>
                               val υidυ = Symbol(it.name.replaceAll("_υ.*υ", "") + id)
                               refresh.prepend(it -> λ(υidυ))
                               υidυ
                             }
          val sumʹ = rename(sum)
          refresh.dropInPlace(refresh.size - n)
          val xidʹ =
            if dups && expansion && term.isDefined
            then
              val υidυ = χ_id
              duplications += υidυ -> (false, Map())
              duplications(υidυ)._2.addAll(duplications(xid)._2)
              υidυ
            else
              xid
          it.copy(variables = variablesʹ, sum = sumʹ, xid = xidʹ, assignment = assignmentʹ)

        case `{}`(identifier, pointers, agent, params*) =>
          val pointersʹ = pointers.map(renamed(_).asSymbol)
          val paramsʹ = params
            .map {
              case λ(it: Symbol) => renamed(it)
              case it => it
            }

          `{}`(identifier, pointersʹ, agent, paramsʹ*)

        case `(*)`(identifier, params*) =>
          val paramsʹ = params
            .map {
              case λ(it: Symbol) => renamed(it)
              case it => it
            }

          `(*)`(identifier, paramsʹ*)
