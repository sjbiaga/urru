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
package generator

import scala.annotation.tailrec

import scala.meta.*
import dialects.Scala3

import parser.StochasticPi.{ Actions, TracesRateException }
import parser.Calculus.{ λ, `(*)` }


object Meta:

  def defn(body: Term): `(*)` => Defn.Def =
    case `(*)`("Main") =>
      Defn.Def(Nil,
               "Main",
               Member.ParamClauseGroup(
                 Type.ParamClause(Nil),
                 `String*`("args") :: `(using ^ : String)(using % : %, / : /, \\ : \\)`,
               ) :: Nil,
               `: IO[Any]`,
               body
      )
    case `(*)`(identifier, _params*) =>
      val params = _params.map(_.asSymbol.name)
      Defn.Def(Nil,
               identifier,
               Member.ParamClauseGroup(
                 Type.ParamClause(Nil),
                 `(…)`(params*) :: `(using ^ : String)(using % : %, / : /, \\ : \\)`,
               ) :: Nil,
               `: IO[Any]`,
               body
      )


  def ====(lhs: λ, rhs: λ): Term =
    Term.ApplyInfix(lhs.toTerm,
                    \("===="),
                    Type.ArgClause(Nil),
                    Term.ArgClause(rhs.toTerm :: Nil))


  val rate: Any => Term = {
    case w: Long if w < 0 => Term.Apply(\("∞"), Term.ArgClause(Lit.Long(-w) :: Nil))
    case r: BigDecimal => Term.Apply(\("ℝ⁺"),
                                     Term.ArgClause(Term.Apply(\("BigDecimal"),
                                                               Term.ArgClause(Lit.String(r.toString) :: Nil)) :: Nil))
    case r: Term => Term.Apply(\("ℝ⁺"), Term.ArgClause(r :: Nil))
    case Symbol(r) => Term.Apply(\("ℝ⁺"), Term.ArgClause(\(r) :: Nil))
    case w: Long => Term.Apply(\("⊤"), Term.ArgClause(Lit.Long(w) :: Nil))
    case _ => rate(1L)
  }

  val rateʹ: Any => Term = {
    case w: Long if w < 0 => Term.Apply(\("∞"), Term.ArgClause(Lit.Long(-w) :: Nil))
    case r: BigDecimal => Term.Apply(\("ℝ⁺"),
                                     Term.ArgClause(Term.Apply(\("BigDecimal"),
                                                               Term.ArgClause(Lit.String(r.toString) :: Nil)) :: Nil))
    case w: Long => Term.Apply(\("⊤"), Term.ArgClause(Lit.Long(w) :: Nil))
    case _: Term | Symbol(_) => throw TracesRateException
    case _ => rateʹ(1L)
  }


  inline implicit def \(* : Enumerator): List[Enumerator] = * :: Nil

  inline implicit def \\(* : Enumerator): Term = \(*)

  implicit def \(* : List[Enumerator]): Term =
    if *.nonEmpty then `for * yield ()`(* *)
    else \(`_ <- IO.unit`)

  inline implicit def \(* : String): Term.Name = Term.Name(*)


  def `String*`(* : String) =
    Term.ParamClause(Term.Param(Nil, *, Some(Type.Repeated(Type.Name("String"))), None) :: Nil
                    ,None)

  def `(…)`(* : String*) =
    Term.ParamClause(*.map(\(_)).map(Term.Param(Nil, _, Some(Type.Name("()")), None)).toList
                    ,None)


  val `(using ^ : String)(using % : %, / : /, \\ : \\)` =
    Term.ParamClause(Term.Param(Mod.Using() :: Nil,
                                "^", Some(Type.Name("String")),
                                None) :: Nil
                    ,Some(Mod.Using())) ::
    Term.ParamClause(List("%", "/", "\\")
                       .map { it => Term.Param(Mod.Using() :: Nil,
                                               it, Some(Type.Name(it)),
                                               None)
                       }
                    ,Some(Mod.Using())) ::
    Nil


  def `:`(name: String, clause: String): Option[Type.Apply] =
    Some(Type.Apply(Type.Name(name), Type.ArgClause(Type.Name(clause) :: Nil)))

  val `: IO[Any]` = `:`("IO", "Any")


  def `* <- …`(* : String*): Pat =
    if *.size == 0
    then
      Pat.Wildcard()
    else if *.size == 1
    then
      Pat.Var(*.head)
    else
      Pat.Tuple(*.map(\(_)).map(Pat.Var(_)).toList)


  def `* <- *`(* : (String, Term)): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(*._1), *._2)

  def `* = *: *`(* : (String, String, Type)): Enumerator.Val =
    Enumerator.Val(Pat.Typed(Pat.Var(*._1), *._3),
                   Term.ApplyType(
                     Term.Select(*._2, "()"),
                     Type.ArgClause(*._3 :: Nil)
                   ))

  def `* = *: * …`(* : (String, String, Type, Type)): Enumerator.Val =
    Enumerator.Val(Pat.Typed(Pat.Var(*._1), *._3),
                   Term.Select(
                     Term.Select(
                       Term.Select(
                         Term.Apply(
                           Term.ApplyType(
                             \("refineV"),
                             Type.ArgClause(*._4 :: Nil)
                           ),
                           Term.ArgClause(
                             Term.ApplyType(
                               Term.Select(*._2, "()"),
                               Type.ArgClause(*._3 :: Nil)
                             ) :: Nil)
                         ),
                         "right"
                       ),
                       "get"
                     ),
                     "value"
                   ))

  def `* :: … :: * = *`(* : (String, String), `…`: String*) =
    def pat(** : String*): Pat =
      val head =
        if **.head.isEmpty then
          Pat.Wildcard()
        else
          Pat.Var(**.head)
      if **.size == 1
      then
        head
      else
        Pat.ExtractInfix(head, \(*._1), Pat.ArgClause(pat(**.tail*) :: Nil))
    Enumerator.Val(pat(`…`*), *._2)


  val `_ <- IO.unit` = `_ <- IO.*`("unit")

  val `IO.cede` = Term.Select("IO", "cede")


  def `_ <- *`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), *)


  def `_ <- IO.*`(* : String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Select("IO", *))

  def `_ <- IO { * }`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(),
                         Term.Apply(\("IO"),
                                    Term.ArgClause(Term.Block(* :: Nil) :: Nil)))


  @tailrec
  def `for * yield ()`(* : Enumerator*): Term =
    if *.nonEmpty
    then
      if !(*.head.isInstanceOf[Enumerator.Generator])
      then
        `for * yield ()`((`_ <- IO.unit` +: *)*)
      else if *.size == 1
      then
        *.head match
          case Enumerator.Generator(Pat.Wildcard(), it: Term.ForYield) =>
            `for * yield ()`(it.enums*)
          case Enumerator.Generator(Pat.Wildcard(), it) =>
            it
          case _ =>
            Term.ForYield(*.toList, Lit.Unit())
      else
        *.last match
          case Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name("IO"), Term.Name("unit" | "cede"))) =>
            `for * yield ()`(*.init*)
          case _ =>
            Term.ForYield(*.toList, Lit.Unit())
    else
      `for * yield ()`(`_ <- IO.unit`)


  private def `π-supervised(*)`(* : Term): Option[Term] =
    * match
      case Term.Select(Term.Name("IO"), Term.Name("unit" | "cede")) => None
      case _ => Some(Term.Apply(\("π-supervised"), Term.ArgClause(* :: Nil)))

  @tailrec
  def `NonEmptyList( *, … ).parSequence`(* : Term*): Term =
    if *.exists {
      case Term.Select(Term.Apply(Term.Name("πLs"), _), Term.Name("πparSequence")) => true
      case _ => false
    } then
      `NonEmptyList( *, … ).parSequence`((
        *.flatMap {
          case Term.Select(Term.Name("IO"), Term.Name("unit" | "cede")) => None
          case Term.Select(Term.Apply(Term.Name("πLs"), ls), Term.Name("πparSequence")) =>
            ls.flatMap {
              case Term.Select(Term.Name("IO"), Term.Name("unit" | "cede")) => None
              case Term.Apply(Term.Name("π-supervised"), it :: Nil) => Some(it)
              case it => Some(it)
            }
          case it => Some(it)
        })*)
      else
        Term.Select(Term.Apply(\("πLs"), Term.ArgClause(*.flatMap(`π-supervised(*)`).toList)), "πparSequence")


  def `if * then … else …`(* : Term, `…`: Term*): Term.If =
    Term.If(*, `…`(0), `…`(1), Nil)


  val `: String => IO[Any]` =
    `: IO[Any]`.map(Type.Function(Type.FuncParamClause(Type.Name("String") :: Nil), _))


  def `val * = *: *`(* : (String, String, Type)): Defn.Val =
    Defn.Val(Nil,
             Pat.Var(*._1) :: Nil,
             Some(*._3),
             Term.ApplyType(
               Term.Select(*._2, "()"),
               Type.ArgClause(*._3 :: Nil)
             )
    )

  def `val * = *: * …`(* : (String, String, Type, Type)): Defn.Val =
   Defn.Val(Nil,
            Pat.Var(*._1) :: Nil,
            Some(*._3),
            Term.Select(
              Term.Select(
                Term.Select(
                  Term.Apply(
                    Term.ApplyType(\("refineV"),
                      Type.ArgClause(*._4 :: Nil)
                    ),
                    Term.ArgClause(
                      Term.ApplyType(
                        Term.Select(*._2, "()"),
                        Type.ArgClause(*._3 :: Nil)) :: Nil)
                  ),
                  "right"
                ),
                "get"
              ),
              "value"
            )
   )

  def `IO { def *(*: ()): String => IO[Any] = { implicit ^ => … } * }`(* : (String, String), `…`: Term): Term =
    Term.Apply(\("IO"),
               Term.ArgClause(
                 Term.Block(
                   Defn.Def(Nil,
                            *._1,
                            Member.ParamClauseGroup(Type.ParamClause(Nil),
                                                    Term.ParamClause(Term.Param(Nil,
                                                                                *._2,
                                                                                Some(Type.Name("()")),
                                                                                None) :: Nil, None) :: Nil) :: Nil,
                            `: String => IO[Any]`,
                            Term.Block(
                              Term.Function(
                                Term.ParamClause(Term.Param(Mod.Implicit() :: Nil,
                                                            "^",
                                                            None,
                                                            None) :: Nil, None), `…`
                              ) :: Nil
                            )
                   ) :: \(*._1) :: Nil
                 ) :: Nil
               )
    )


  def `IO { lazy val *: String => IO[Any] = { implicit ^ => … } * }`(* : String, `…`: Term): Term =
    Term.Apply(\("IO"),
               Term.ArgClause(
                 Term.Block(
                   Defn.Val(Mod.Lazy() :: Nil,
                            `* <- …`(*) :: Nil,
                            `: String => IO[Any]`,
                            Term.Block(
                              Term.Function(
                                Term.ParamClause(Term.Param(Mod.Implicit() :: Nil,
                                                            "^",
                                                            None,
                                                            None) :: Nil, None), `…`
                              ) :: Nil
                            )
                   ) :: \(*) :: Nil
                 ) :: Nil
               )
    )


  def `π-exclude`(enabled: Actions): Term =
    Term.Apply(\("π-exclude"),
               Term.ArgClause(enabled.map(Lit.String(_)).toList))
