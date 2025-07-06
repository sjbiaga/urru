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

import scala.io.Source

import scala.collection.mutable.{
  LinkedHashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}

import scala.meta.Lit

import StochasticPi.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion
import Expansion.Duplications


abstract class StochasticPi extends Expression:

  def μ: Parser[(μ, (Names, Names))] =
    "τ"~opt("@"~>rate) ~ opt( expression ) ^^ { // silent prefix
      case _ ~ r ~ code =>
        val free = code.map(_._2).getOrElse(Names())
        val rʹ = Some(r.getOrElse(None))
        τ(rʹ, code.map(_._1))(sπ_id) -> (Names(), free)
    } |
    name ~ opt("@"~>rate) ~ ("<"~>name<~">") ~ opt( expression ) ^^ { // negative prefix i.e. output
      case (ch, _) ~ _ ~ _ ~ _  if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case (ch, name) ~ r ~  (arg, free) ~ Some((it, freeʹ)) =>
        val rʹ = Some(r.getOrElse(None))
        π(ch, arg, polarity = None, rʹ, Some(it))(sπ_id) -> (Names(), name ++ free ++ freeʹ)
      case (ch, name) ~ r ~ (arg, free) ~ _ =>
        val rʹ = Some(r.getOrElse(None))
        π(ch, arg, polarity = None, rʹ, None)(sπ_id) -> (Names(), name ++ free)
    } |
    name ~ opt("@"~>rate) ~ ("("~>nameʹ<~")") ~ opt( expression ) ^^ { // positive prefix i.e. input
      case (ch, _) ~ _ ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case _ ~ _ ~ (par, _) ~ _ if !par.isSymbol =>
        throw PrefixChannelParsingException(par)
      case _ ~ _ ~ _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case (ch, name) ~ r ~ (par, bound) ~ code =>
        val free = code.map(_._2).getOrElse(Names())
        val rʹ = Some(r.getOrElse(None))
        π(ch, par, polarity = Some(""), rʹ, code.map(_._1))(sπ_id) -> (bound, name ++ free)
    } |
    name ~ cons_r ~ ("("~>namesʹ<~")") ~ opt( expression ) ^^ { // polyadic unconsing
      case (ch, _) ~ _ ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case _ ~ _ ~ params ~ _ if !params.forall(_._1.isSymbol) =>
        throw PrefixChannelsParsingException(params.filterNot(_._1.isSymbol).map(_._1)*)
      case (ch, _) ~ _ ~ params ~ _
          if {
            val paramsʹ = params.filterNot { case (λ(Symbol("")), _) => true case _ => false }
            paramsʹ.size > paramsʹ.distinctBy { case (λ(Symbol(it)), _) => it }.size
          } =>
        throw PrefixChannelsParsingExceptionʹ(ch.asSymbol,
                                              params
                                                .filterNot { case (λ(Symbol("")), _) => true case _ => false }
                                                .map(_._1.asSymbol.name)*)
      case _ ~ _ ~ _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case (λ(ch: Symbol), _) ~ cons ~ params ~ _
          if params.exists { case (λ(`ch`), _) => true case _ => false } =>
        throw ConsItselfParsingException(ch, cons)
      case (ch, name) ~ cons ~ params ~ code =>
        val args = λ(params.map(_._1))
        val bound = params.map(_._2).reduce(_ ++ _)
        val free = code.map(_._2).getOrElse(Names())
        π(ch, args, polarity = Some(cons), None, code.map(_._1))("") -> (bound, name ++ free &~ bound)
    }

  def name: Parser[(λ, Names)] = ident ^^ (Symbol(_)) ^^ { it => λ(it) -> Set(it) } |
                                 floatingPointNumber ^^ (BigDecimal(_)) ^^ { λ(_) -> Names() } |
                                 stringLiteral ^^ (_.stripPrefix("\"").stripSuffix("\"")) ^^ { λ(_) -> Names() } |
                                 ( "True" | "False" ) ^^ (_ == "True") ^^ { λ(_) -> Names() } |
                                 expression ^^ {
                                   case ((Right(term), _), free) => λ(term) -> free
                                   case ((Left(enums), _), _) => throw TermParsingException(enums)
                                 }

  def rate: Parser[Any] = "("~>rate<~")" |
                          "∞" ^^ { _ => -1L } |
                          wholeNumber<~"∞" ^^ { -_.toLong.abs } |
                          "⊤" ^^ { _ => 1L } |
                          wholeNumber<~"⊤" ^^ { _.toLong.abs } |
                          floatingPointNumber ^^ { BigDecimal(_) } |
                          super.ident ^^ { Symbol(_) } |
                          expression ^^ {
                            case ((Right(term), _), free) =>
                              if free.nonEmpty
                              then
                                val werr = _werr
                                _werr = false
                                warn(throw RateFreeNamesException(free.map(_.name).toSeq*))
                                _werr = werr
                              term
                            case ((Left(enums), _), _) =>
                              throw TermParsingException(enums)
                          }

  def nameʹ: Parser[(λ, Names)] =
    name ~ opt(`type`) ^^ {
      case (λ, free) ~ tpe =>
        (λ.copy()(using tpe), free)
    }

  def names: Parser[List[(λ, Names)]] =
    rep1sep(name, ",")

  def namesʹ: Parser[List[(λ, Names)]] =
    rep1sep(opt(nameʹ) ^^ { _.getOrElse(λ(Symbol("")) -> Names()) }, ",")

  /**
   * Channel names start with lower case.
   * @return
   */
  override def ident: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isLowerCase)("channel name expected but '" + _ + "' found"),
          elem("channel name part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)

  /** A natural number. */
  override def wholeNumber: Parser[String] =
    """\d+""".r ^^ { s =>
      try
        if s.toLong == 0
        then
          throw WholeNumberFormatException("not be zero")
        else
          s
      catch
        case t: NumberFormatException =>
          throw WholeNumberFormatException("be a Long", t)
    }

  private[parser] var eqtn: List[Bind] = null
  private[parser] var defn: Map[Int, List[Define]] = null
  private[parser] var self: Set[Int] = null
  protected var _nest = -1
  private[parser] var _nth: Map[Int, Long] = null
  protected final def nest(b: Boolean) =
    _nth(_nest) += 1
    _nest += (if b then 1 else -1)
    if b
    then
      _nth(_nest) = 0L
      _cntr(_nest) = 0L
    else
      _nth -= _nest+1
      _cntr -= _nest+1
  private[parser] var _cntr: Map[Int, Long] = null

  private[parser] def pos(bound: Boolean = false) = { _cntr(_nest) += 1; Position(_cntr(_nest), bound) }
  private[parser] def pos_(bound: Boolean = false) = { _cntr(_nest) += 1; Position(-_cntr(_nest), bound) }

  protected final def path = (0 until _nest).map(_nth(_))

  protected var _dirs = List[Map[String, Any]]()

  protected var _dups: Boolean = false

  protected var _par: Int = 9

  protected var _traces: Option[Option[String]] = None

  protected var _exclude: Boolean = false

  private[parser] var _id: helper.υidυ = null

  private[parser] var _sπ_id: helper.υidυ = null

  private[parser] var _χ_id: helper.υidυ = null

  protected final def id = _id()

  protected final def sπ_id = _sπ_id()

  protected final def χ_id = _χ_id()

  protected final def copy: ((Any, Any), Any) =
    _id.copy -> _sπ_id.copy -> _χ_id.copy

  protected final def paste(it: ((Any, Any), Any)) =
    _id.paste(it._1._1)
    _sπ_id.paste(it._1._2)
    _χ_id.paste(it._2)

  protected final def save[T](r: => ParseResult[T]): Option[(T, Input)] =
    val nest = _nest
    val cntr = Map.from(_cntr)
    _id.save {
      _sπ_id.save {
        _χ_id.save {
          r match
            case Success(it, in) => Some(it -> in)
            case _ =>
              _cntr = cntr
              _nest = nest
              None
        }
      }
    }

  protected object BindingOccurrence:
    def apply(names: Names)
             (using Bindings): Unit =
      names.foreach { it => this(it, if _code < 0 then None else Some(it), hardcoded = true) }
    def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
             (using bindings: Bindings): Unit =
      bindings.get(name) match
        case Some(Occurrence(_, it @ Position(k, false))) if k < 0 =>
          bindings += name -> Occurrence(shadow, it.copy(binds = true))
        case Some(Occurrence(Some(_), Position(k, true))) if _code >= 0 && (!hardcoded || k < 0) =>
          throw UniquenessBindingParsingException(_code, _nest, name, hardcoded)
        case Some(Occurrence(_, Position(_, false))) if _code >= 0 =>
          throw NonParameterBindingParsingException(_code, _nest, name, hardcoded)
        case Some(Occurrence(_, Position(_, false))) =>
        case _ =>
          bindings += name -> Occurrence(shadow, pos(true))


object StochasticPi:

  type Actions = Set[String]

  object Actions:
    def apply(ps: Pre*): Actions = Set.from(
      ps
        .filter { case Act(it) => it }
        .headOption
        .map(_.asInstanceOf[Act].υidυ)
    )

  def nil = Actions()


  trait Act(fun: () => String):
    val rate: Option[Any]
    final def id = fun()
    final lazy val υidυ: String = id

  object Act:
    def unapply(self: Pre): Option[Boolean] =
      self match
        case it: Act => Some(it.rate.isDefined)
        case _ => Some(false)

  trait Sum:
    val enabled: Actions


  private val cons_r = """[^/*{\[(<.,"'\p{Alnum}@\p{Space}'",.>)\]}*/]+""".r

  type Names = Set[Symbol]

  object Names:
    def apply(os: λ*): Names = Set.from(os
      .filter(_.isSymbol)
      .map(_.asSymbol)
    )
    def apply(names: Names): Names = Set.from(names)


  // exceptions

  import scala.meta.Enumerator

  import Expression.ParsingException

  abstract class PrefixParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class WholeNumberFormatException(msg: String, cause: Throwable = null)
      extends PrefixParsingException("The weight is a natural number that must " + msg, cause)

  case class RateFreeNamesException(names: String*)
      extends PrefixParsingException(s"""A rate that is a Scalameta Term may have free names '${names.mkString(", ")}' which are ignored""")

  case object TracesRateException
      extends PrefixParsingException("When traces are active, used rates must not be Scala code")

  case class PrefixChannelParsingException(name: λ)
      extends PrefixParsingException(s"${name.`val`} is not a channel name but a ${name.kind}")

  case class PrefixChannelsParsingExceptionʹ(name: Symbol, ps: String*)
      extends PrefixParsingException(s"""For a polyadic name ${name.name}, the parameters names '${ps.mkString(", ")}' must be distinct""")

  case class TermParsingException(enums: List[Enumerator])
      extends PrefixParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")

  case class ConsItselfParsingException(name: Symbol, cons: String)
      extends PrefixParsingException(s"A name ${name.name} that knows how to CONS (`$cons') is itself the result")


  // functions

  extension [T <: AST](ast: T)

    def shallow: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case +(_, it*) =>
          `+`(nil, it.map(_.shallow)*)

        case ∥(it*) =>
          ∥(it.map(_.shallow)*)

        case `.`(end, it*) =>
          `.`(end.shallow, it*)

        case ?:(cond, t, f) =>
          ?:(cond, t.shallow, f.map(_.shallow))

        case it @ !(_, sum) =>
          it.copy(sum = sum.shallow)

        case it @ `⟦⟧`(_, _, sum, _, _) =>
          it.copy(sum = sum.shallow)

        case `{}`(identifier, pointers, true, params*) =>
          `(*)`(identifier, (params ++ pointers.map(λ(_)))*)

        case _ => ast


  final class Main(override protected val in: String) extends Expansion:

    def line(using Duplications): Parser[Either[Bind, Option[Define]]] =
      equation ^^ { Left(_) } | definition ^^ { Right(_) }

    private def ensure(implicit prog: List[Bind]): Unit =
      import helper.Ensure.*

      val i = main

      if i < 0 then throw MainParsingException

      given rec: Map[(String, Int), Int] = Map()
      given rep: Map[Int, Int] = Map()

      prog(i)._2.recursive(using "Main" -> 0 :: Nil)

      if rec.contains("Main" -> 0) then throw MainParsingExceptionʹ

      for
        (i, n) <- rep
      do
        prog(i)._1 match
          case `(*)`(identifier, params*) =>
            warn(throw RecRepParsingException(identifier, params.size, n))

    private def `(*) => +`(prog: List[Bind]): `(*)` => + = {
      case `(*)`(identifier, args*) =>
        prog
          .find {
            case (`(*)`(`identifier`, params*), _) if params.size == args.size => true
            case _ => false
          }
          .get
          ._2
    }

    extension [T <: AST](ast: T)

      def parse(using excluded: Map[String, Actions]): (T, Actions) =

        inline given Conversion[AST, T] = _.asInstanceOf[T]

        inline def τ: Calculus.Pre.τ = Calculus.Pre.τ(Some(-Long.MaxValue), None)(sπ_id)

        def insert[S](end: + | -, ps: Pre*): (S, Actions) =
          val psʹ = ps :+ τ
          `.`(end, psʹ*).asInstanceOf[S] -> Actions(psʹ*)

        def insert_+(sum: +): + =
          val (seq, enabled) = insert[`.`](sum)
          `+`(enabled, ∥(seq))

        ast match

          case ∅() => (ast, nil)

          case it: + =>
            val sum = it.choices.foldLeft(`+`(nil)) {
              case (sum: +, par) =>
                val (parʹ, enabledʹ) = par.parse
                assert(enabledʹ.nonEmpty)
                assert((sum.enabled & enabledʹ).isEmpty)
                (`+`(sum.enabled ++ enabledʹ, (sum.choices :+ parʹ)*))
            }
            (sum, sum.enabled)

          case it: ∥ =>
            val (par, enabled) = it.components.foldLeft((∥(), nil)) {
              case ((par: ∥, enabled), seq) =>
                val (seqʹ, enabledʹ) = seq.parse
                assert(enabledʹ.nonEmpty)
                assert((enabled & enabledʹ).isEmpty)
                (∥((par.components :+ seqʹ)*), enabled ++ enabledʹ)
            }
            (par, enabled)

          case `.`(end, ps*) =>
            val (it, enabled) = end.parse

            if Actions(ps*).nonEmpty
            then
              (`.`(it, ps*), Actions(ps*))
            else if enabled.nonEmpty
            then
              (`.`(it, ps*), enabled)
            else
              assert(it.isInstanceOf[+] && it.asInstanceOf[+].isVoid || it.isInstanceOf[`(*)`])
              insert(it, ps*)

          case ?:(c, _t, _f) =>
            def _t_f(_sum: +): + =
              val (sum, _) = _sum.parse

              if sum.enabled.isEmpty
              || sum.enabled.exists(excluded.contains(_))
              then
                insert_+(sum)
              else
                sum

            val (t, f) = _t_f(_t) -> _f.map(_t_f(_))

            f.foreach { sum => t.enabled.foreach(excluded(_) = sum.enabled) }
            f.foreach(_.enabled.foreach(excluded(_) = t.enabled))

            assert(t.enabled.nonEmpty)
            assert(f.map(_.enabled.nonEmpty).getOrElse(true))

            assert((t.enabled & f.map(_.enabled).getOrElse(nil)).isEmpty)
            (?:(c, t, f), t.enabled ++ f.map(_.enabled).getOrElse(nil))

          case !(Some(μ), sum) =>
            val (it, _) = sum.parse
            (`!`(Some(μ), it), Actions(μ))

          case !(_, sum) =>
            `!`(Some(τ), sum).parse

          case `⟦⟧`(definition, variables, _sum, xid, assignment) =>
            val n = assignment.size

            val sum: + =
              if variables.size == n
              then
                _sum
              else
                `+`(nil, ∥(`.`(_sum, ν(variables.drop(n).map(_.name).toSeq*))))

            var (it, _) = sum.parse

            if it.enabled.isEmpty
            then
              it = insert_+(it)

            (`⟦⟧`(definition, variables, it, xid, assignment), it.enabled)

          case _: `{}` => ???

          case it: `(*)` => (it, nil)

      def split(using discarded_excluded: (Map[String, Actions], Map[String, Actions])): Actions =

        ast match

          case ∅() => nil

          case +(enabled, par) =>
            par.split
            enabled

          case +(enabled, ps*) =>
            val ls = ps.map(_.split)
            var ts = ls.take(0)
            var ds = ls.drop(1)

            val (discarded, _) = discarded_excluded

            ls.foreach { it =>
              assert(it.nonEmpty)
              val ks = (ts ++ ds).reduce(_ ++ _)
              assert((it & ks).isEmpty)
              it.foreach { k =>
                if !discarded.contains(k)
                then
                  discarded(k) = nil
                discarded(k) ++= ks
              }
              ts :+= it
              ds = ds.drop(1)
            }

            enabled

          case ∥(ss*) =>
            ss.map(_.split).reduce(_ ++ _)

          case `.`(_: `(*)`, ps*) =>
            assert(Actions(ps*).nonEmpty)
            Actions(ps*)

          case `.`(end, ps*) =>
            var enabled = end.split

            if Actions(ps*).nonEmpty then
              enabled = Actions(ps*)

            enabled

          case ?:(_, t, f) =>
            t.split
            f.foreach(_.split)

            val (_, excluded) = discarded_excluded

            t.enabled.foreach { k => assert(!excluded.contains(k)) }
            f.foreach(_.enabled.foreach { k => assert(!excluded.contains(k)) })

            f.foreach { sum => t.enabled.headOption.foreach(excluded(_) = sum.enabled) }
            f.foreach(_.enabled.headOption.foreach(excluded(_) = t.enabled))

            assert((t.enabled & f.map(_.enabled).getOrElse(nil)).isEmpty)
            t.enabled ++ f.map(_.enabled).getOrElse(nil)

          case !(Some(μ), sum) =>
            sum.split
            Actions(μ)

          case _: ! => ??? // impossible by 'parse'

          case `⟦⟧`(_, _, sum, _, _) =>
            sum.split
            sum.enabled

          case _: `{}` => ???

          case _: `(*)` => ??? // always "guarded"

      def graph(using prog: List[Bind]): Seq[(Act, Act | Sum)] =

        ast match

          case ∅() => Nil

          case +(_, ps*) =>
            ps.map(_.graph).reduce(_ ++ _)

          case ∥(ss*) =>
            ss.map(_.graph).reduce(_ ++ _)

          case `.`(end, ps*) =>

            inline given Conversion[(Pre, Act | Sum), (Act, Act | Sum)] = _.asInstanceOf[Act] -> _

            end.graph ++
            ( for
                i <- 0 until ps.size
                if ps(i) match { case Act(it) => it }
                j = ps.drop(i+1).indexWhere { case Act(it) => it }
                if j >= 0
              yield
                ps(i).asInstanceOf[Act] -> ps(i+1+j).asInstanceOf[Act]
            ) ++
            {
              val k = ps.lastIndexWhere { case Act(it) => it }
              if k >= 0
              then end match
                case sum: + =>
                  Seq(ps(k) -> sum)
                case ?:(_, t, Some(f)) =>
                  Seq(ps(k) -> t, ps(k) -> f)
                case ?:(_, t, _) =>
                  Seq(ps(k) -> t)
                case !(Some(μ), _) =>
                  Seq(ps(k) -> μ)
                case _: ! => ??? // impossible by 'parse'
                case `⟦⟧`(_, _, sum, _, _) =>
                  Seq(ps(k) -> sum)
                case _: `{}` => ???
                case it: `(*)` =>
                  val sum = `(*) => +`(prog)(it)
                  Seq(ps(k) -> sum)
              else Nil
            }

          case ?:(_, t, f) =>
            t.graph ++ f.map(_.graph).getOrElse(Nil)

          case !(Some(μ), sum) =>
            sum.graph ++ Seq(μ -> μ, μ -> sum)

          case _: ! => ??? // impossible by 'parse'

          case `⟦⟧`(_, _, sum, _, _) =>
            sum.graph

          case _: `{}` => ???

          case _: `(*)` => Nil

    def apply(prog: List[Bind]): (List[Bind], (Map[String, Actions], Map[String, Actions], Map[String, Actions])) =

      given excluded: Map[String, Actions] = Map()

      given List[Bind] = prog.map(_ -> _.shallow.parse._1)

      ensure

      val discarded = Map[String, Actions]()

      excluded.clear

      val enabled = Map[String, Actions]()

      given_List_Bind
        .tapEach {
          case (_, sum) =>
            sum.split(using discarded -> excluded)

            sum.graph.foreach { (s, t) =>
              if !enabled.contains(s.υidυ)
              then
                enabled(s.υidυ) = nil
              t match
                case it: Act =>
                  enabled(s.υidυ) += it.υidυ
                case it: Sum =>
                  enabled(s.υidυ) ++= it.enabled
            }
        } -> (discarded, excluded, enabled)


    private var i: Int = -1
    private var l: (Int, Int) = (-1, -1)
    override def ln: String = if l._1 == l._2 then s"line #${l._2}" else s"lines #${l._1}-#${l._2}"

    def apply(source: Source, errors: Boolean = false): List[Either[String, Bind]] =
      _werr = errors
      _dups = false
      _exclude = false
      _par = 9
      _traces = None
      _dirs = List(Map("errors" -> _werr,
                       "duplications" -> _dups,
                       "exclude" -> _exclude,
                       "parallelism" -> _par,
                       "traces" -> _traces))
      eqtn = List()
      defn = Map()
      self = Set()
      _nest = 0
      _id = new helper.υidυ
      _sπ_id = new helper.υidυ
      _χ_id = new helper.υidυ
      i = 0
      l = (0, 0)

      given Duplications()

      val r =
        (source.getLines() ++ Some(""))
          .zipWithIndex
          .foldLeft(List[(String, (Int, Int))]() -> false) {
            case ((r, false), (l, n)) => (r :+ (l, (n, n))) -> l.endsWith("\\")
            case ((r, true), (l, n)) => (r.init :+ (r.last._1.stripSuffix("\\") + l, (r.last._2._1, n))) -> l.endsWith("\\")
          }._1
          .flatMap { case (it, (m, n)) =>
            l = (m+1, n+1)
            if it.matches("^[ ]*#.*") // commented lines
            || it.isBlank // empty lines
            then
              None
            else if it.matches("^[ ]*@.*")
            then // Scala
              Some(Left(it.replaceFirst("^([ ]*)@(.*)$", "$1$2")))
            else // SPi
              _cntr = Map(0 -> 0L)
              _nth = Map(0 -> 0L)
              parseAll(line, it) match
                case Success(Left(equation), _) =>
                  if !_exclude
                  then
                    eqtn :+= equation
                    val equations = eqtn.slice(i, eqtn.size)
                    i = eqtn.size
                    equations.map(Right(_))
                  else
                    Nil
                case Success(Right(Some(definition)), _) =>
                  if !_exclude
                  then
                    if !defn.contains(_code) then defn(_code) = Nil
                    defn(_code) ::= definition
                  Nil
                case Success(Right(_), _) => // directive
                  Nil
                case failure: NoSuccess =>
                  scala.sys.error(failure.msg)
          }
          .filter {
            case Right((`(*)`(s"Self_$n", _*), _))
                if { try { n.toInt; true } catch _ => false } =>
              self.contains(n.toInt)
            case _ => true
          }

      val prog =
        ( if i < eqtn.size && !_exclude
          then
            r ::: eqtn.slice(i, eqtn.size).map(Right(_))
          else
            r
        ).filter {
          case Right((`(*)`(s"Self_$n", _, _*), _))
              if { try { n.toInt; true } catch _ => false } =>
            self.contains(n.toInt)
          case _ => true
        }

      Right(`(*)`("_par", λ(Lit.Int(_par))), `+`(nil)) :: prog
