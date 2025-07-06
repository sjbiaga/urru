package urru
package common

import cats.Monoid

import base.Visitor.*

export Tree.{ Implicits => _, * }


enum Tree[+A >: Null]:
  case Empty extends Tree[Null]
  case Leaf[A >: Null](override val parameter: A) extends Tree[A]
  case Node[A >: Null](override val parameter: A,
                       children: Tree[A]*) extends Tree[A]
  private case Input[A >: Null](entity: Entity,
                                transition: (Phase, Phase),
                                override protected val input: Tree[A]) extends Tree[A]
  private case Output[A >: Null](override val parameter: A,
                                 children: Tree[A]*) extends Tree[A]
  private case Node1[A >: Null](override protected val input: Tree[A],
                                override val next: Tree[A]) extends Tree[A]
  private case Push[A >: Null](override protected val input: Tree[A],
                               override protected val next: Tree[A]) extends Tree[A]
  private case Pop[A >: Null](override protected val input: Tree[A],
                              override protected val next: Tree[A]) extends Tree[A]

  protected def input: Tree[A] = ???
  def parameter: A = ???
  protected val next: Tree[A] = Empty

  private def apply(): Tree[A] =

    def reduce(parameter: A, childrenʹ: Tree[A]*): Tree[A] =
      val children = childrenʹ.map(_()).filterNot {
        case Empty | Leaf(null) => true
        case _ => false
      }
      if children.isEmpty
      then
        Leaf(parameter)
      else if children.size == 1 && parameter == null
      then
        children.head
      else if children.size == 1
      then
        children.head match
          case Node(null, children*) => Node(parameter, children*)
          case _ => Node(parameter, children*)
      else
        Node(parameter, children*)

    this match
      case Output(parameter, children*) => reduce(parameter, children*)
      case Node(parameter, children*) => reduce(parameter, children*)
      case Empty => Empty


object Tree:

  given [A >: Null]: Conversion[(Entity, (Phase, Phase)), Tree[A]] =
    { case (entity, transitition) => Input(entity, transitition, Empty) }

  given [A >: Null]: Conversion[((Entity, (Phase, Phase)), A), Tree[A]] =
    { case ((entity, transitition), input) => Input(entity, transitition, Output(input)) }

  object Implicits:

    implicit def treeMonoid[A >: Null]: Monoid[Tree[A]] =

      new Monoid[Tree[A]]:

        override def empty: Tree[A] = Empty

        override def combine(lhs: Tree[A], rhs: Tree[A]): Tree[A] =

          def fold1(rhs: Tree[A]): (Tree[A], Tree[A]) =
            rhs match
              case Push(input, stack) =>
                input -> stack
              case Node1(input: Tree[A], next) =>
                val (inputʹ, stack) = fold1(next)
                combine(inputʹ, input) -> stack

          (lhs, rhs) match
            case (Input(entity, transition, input), rhs) =>
              transition match
                case (_, OPEN) =>
                  Push(input, rhs)
                case (_, LOOP | NEXT) =>
                  rhs match
                    case Pop(inputʹ, stack) =>
                      Node1(Output(try { input.parameter } catch _ => null, inputʹ), stack)
                    case _ =>
                      Node1(input, rhs)
                case (_, CLOSE) =>
                  fold1(rhs) match
                    case (output @ Output(_, _*), stack) =>
                      Pop(Output(try { input.parameter } catch _ => null, output), stack)
                    case (Node(_, children*), stack) =>
                      Pop(Output(try { input.parameter } catch _ => null, children*), stack)
                    case (_, stack) =>
                      Pop(input, stack)
                  match
                    case Pop(inputʹ, Empty) =>
                      inputʹ()
                    case rhsʹ =>
                      rhsʹ
            case _ =>
              (lhs, rhs) match
                case (lhs, Empty) => lhs
                case (Empty, rhs) => rhs
                case (Output(_, _*), Output(_, _*)) =>
                  Node(null, lhs, rhs)
                case (Node(_, children*), Output(_, _*)) =>
                  Node(null, (children :+ rhs)*)
                case (Output(_, _*), Node(_, children*)) =>
                  Node(null, (lhs +: children)*)

    import io.github.greenleafoss.mongo.GreenLeafJsonProtocol

    trait TreeJsonProtocol extends GreenLeafJsonProtocol:
      import spray.json.*

      implicit def TreeFormat[A >: Null: JsonFormat]: JsonFormat[Tree[A]] =

        new JsonFormat[Tree[A]]:

          def write(self: Tree[A]) =
            self match
              case Empty => JsObject()
              case Leaf(null) => JsNull
              case Leaf[A](parameter) => parameter.toJson
              case Node[A](null, children*) =>
                val childrenField = "children" -> JsArray(children.map(_.toJson).toList)
                val sizeField = "size" -> JsNumber(children.size)
                JsObject(childrenField, sizeField)
              case Node[A](parameter, children*) =>
                val childrenField = "children" -> JsArray(children.map(_.toJson).toList)
                val sizeField = "size" -> JsNumber(children.size)
                JsObject(parameter.toJson.asJsObject.fields + childrenField + sizeField)

          def read(value: JsValue): Tree[A] =
            value match
              case JsObject(fields) if fields.isEmpty => Empty
              case JsObject(fields) if fields.contains("children") =>
                Node(value.convertTo[A],
                     fields("children").asInstanceOf[JsArray].elements.map(_.convertTo[Tree[A]])*)
              case _ =>
                Leaf(value.convertTo[A])

    object TreeJsonProtocol extends TreeJsonProtocol

    trait TreeJsonProtocolʹ extends GreenLeafJsonProtocol:
      import spray.json.*
      import TreeJsonProtocol.*

      implicit def TreeFormatʹ[A >: Null: JsonFormat]: JsonFormat[(String, Tree[A])] =
        jsonFormat(Tuple2.apply[String, Tree[A]], "savepoint", "tree")

    object TreeJsonProtocolʹ extends TreeJsonProtocolʹ

  object Validate:

    abstract trait HasDepth:
      val depth: Int

    object HasDepth:
      def unapply(self: HasDepth): Option[Int] = Some(self.depth)

    def apply(self: Tree[HasDepth])
             (depthʹ: Option[Int] = None): Boolean =
      depthʹ match
        case Some(depth) =>
          self match
            case Node(HasDepth(`depth`), children*) => children.forall(this(_)(Some(depth + 1)))
            case Empty | Leaf(HasDepth(`depth`)) => true
            case _ => false
        case _ =>
          self match
            case Node(HasDepth(depth), children*) => children.forall(this(_)(Some(depth + 1)))
            case Node(null, children*) => children.forall(this(_)())
            case _ => true
