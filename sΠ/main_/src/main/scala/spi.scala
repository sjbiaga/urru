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

package object sΠ:

  import _root_.scala.collection.immutable.{ List, Map, Set }

  import _root_.cats.effect.{ IO, Clock, Deferred, Ref }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.Supervisor

  import `Π-loop`.{ -, %, /, \ }
  import `Π-magic`.><
  export `Π-magic`.>*<
  import `Π-stats`.Rate


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]


  /**
    * Supervised [[code]].
    * @param code
    */
  private def exec[T](code: => IO[T]): IO[T] =
    Supervisor[IO](await = true)
      .use(_.supervise(code))
      .flatMap(_.join
                .flatMap
                { case Succeeded(it) => it
                  case _ => IO(null.asInstanceOf[T]) }
              )


  inline def `π-exclude`(enabled: String*)
                        (using % : %, \ : \): IO[Unit] =
    `π-exclude`(Set.from(enabled)) >> \()

  private def `π-exclude`(enabled: `Π-Set`[String])
                         (using % : %): IO[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                   val n = m(key).asInstanceOf[Int] - 1
                                   if n == 0
                                   then
                                     m - key
                                   else
                                     m + (key -> n)
                                 }
    )

  private def exclude(key: String)
                     (using % : %)
                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]): IO[Unit] =
    if `π-elvis`.contains(key)
    then
      `π-exclude`(`π-elvis`(key))
    else
      IO.unit


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): IO[B] = flatMap(f andThen IO.pure)
    def flatMap[B](f: `()` => IO[B]): IO[B] =
      ( for
          ref <- Ref.of[IO, ><](><())
        yield
          f(ref)
      ).flatten


  /**
    * silent transition
    */

  object τ:

    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _         <- exclude(key)
        deferred  <- Deferred[IO, Option[(Double, (-, -))]]
        dummy_ref <- Ref.of[IO, ><](><())
        timestamp <- Clock[IO].monotonic.map(_.toNanos)
        _         <- /.offer(^ -> key -> (deferred -> (timestamp, (dummy_ref, None, rate))))
        opt       <- deferred.get
        _         <- if opt eq None then IO.canceled else IO.unit
        (delay,
        (b, b2))   = opt.get
        _         <- b.await
        _         <- b2.await
      yield
        delay

  /**
    * prefix
    */
  final implicit class `()`(private val name: Any) extends AnyVal:

    private def ref = `()`[>*<]

    def ====(that: `()`) =
      try
        this.ref eq that.ref
      catch
        case _ =>
          this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _         <- exclude(key)
        deferred  <- Deferred[IO, Option[(Double, (-, -))]]
        timestamp <- Clock[IO].monotonic.map(_.toNanos)
        _         <- /.offer(^ -> key -> (deferred -> (timestamp, (ref, Some(false), rate))))
        delay     <- ><(key, value.name)(deferred)(ref)
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String)(code: => IO[Any])
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _         <- exclude(key)
        deferred  <- Deferred[IO, Option[(Double, (-, -))]]
        timestamp <- Clock[IO].monotonic.map(_.toNanos)
        _         <- /.offer(^ -> key -> (deferred -> (timestamp, (ref, Some(false), rate))))
        delay     <- ><(key, value.name)(code)(deferred)(ref)
      yield
        delay

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[(`()`, Double)] =
      for
        _          <- exclude(key)
        deferred   <- Deferred[IO, Option[(Double, (-, -))]]
        timestamp  <- Clock[IO].monotonic.map(_.toNanos)
        _          <- /.offer(^ -> key -> (deferred -> (timestamp, (ref, Some(true), rate))))
        (r, delay) <- ><(key)(deferred)(ref)
      yield
        new `()`(r) -> delay

    /**
      * positive prefix i.e. input
      */
    def apply[T](rate: Rate)(key: String)(code: T => IO[T])
                (using % : %, / : /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[(`()`, Double)] =
      for
        _          <- exclude(key)
        deferred   <- Deferred[IO, Option[(Double, (-, -))]]
        timestamp  <- Clock[IO].monotonic.map(_.toNanos)
        _          <- /.offer(^ -> key -> (deferred -> (timestamp, (ref, Some(true), rate))))
        (r, delay) <- ><(key)(code)(deferred)(ref)
      yield
        new `()`(r) -> delay

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

    /**
      * Adapted from cats-effect tutorial [[https://typelevel.org/cats-effect/docs/tutorial]].
      *
      * @see [[https://github.com/lrodero/cats-effect-tutorial/blob/series/3.x/src/main/scala/catseffecttutorial/producerconsumer/ProducerConsumerBoundedCancelable.scala]]
      */
    /*
     *
     * Copyright (c) 2020 Luis Rodero-Merino
     *
     * Licensed under the Apache License, Version 2.0 (the "License");
     * you may not use this file except in compliance with the License.
     * You may obtain a copy of the License at.
     *
     *     http://www.apache.org/licenses/LICENSE-2.0
     *
     * Unless required by applicable law or agreed to in writing, software
     * distributed under the License is distributed on an "AS IS" BASIS,
     * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
     * See the License for the specific language governing permissions and
     * limitations under the License.
     */

    final case class ><(takers: List[Deferred[IO, Any]],
                        offerers: List[(Any, Deferred[IO, Unit])])

    type >*< = Ref[IO, ><]

    object >< :

      inline def apply(): >< = ><(Nil, Nil)

      def apply(key: String, name: Any)
               (deferred: Deferred[IO, Option[(Double, (-, -))]])
               (`>R`: >*<): IO[java.lang.Double] =
        for
          opt     <- deferred.get
          _       <- if opt eq None then IO.canceled else IO.unit
          (delay,
          (b, b2)) = opt.get
          offerer <- Deferred[IO, Unit]
          _       <- IO.uncancelable { poll =>
                       `>R`.modify {
                         case it @ ><(takers, _) if takers.nonEmpty =>
                           val (taker, rest) = takers.head -> takers.tail
                           it.copy(takers = rest) -> taker.complete(name).void
                         case it =>
                           val cleanup = `>R`.update { it => it.copy(offerers = it.offerers.filter(_._2 ne offerer)) }
                           it.copy(offerers = name -> offerer :: it.offerers) -> poll(offerer.get).onCancel(cleanup)
                       }.flatten
                     }
          _       <- b.await
          _       <- b2.await
        yield
          delay

      def apply(key: String, name: Any)(code: => IO[Any])
               (deferred: Deferred[IO, Option[(Double, (-, -))]])
               (`>R`: >*<): IO[java.lang.Double] =
        for
          opt     <- deferred.get
          _       <- if opt eq None then IO.canceled else IO.unit
          (delay,
          (b, b2)) = opt.get
          offerer <- Deferred[IO, Unit]
          _       <- IO.uncancelable { poll =>
                       `>R`.modify {
                         case it @ ><(takers, _) if takers.nonEmpty =>
                           val (taker, rest) = takers.head -> takers.tail
                           it.copy(takers = rest) -> taker.complete(name).void
                         case it =>
                           val cleanup = `>R`.update { it => it.copy(offerers = it.offerers.filter(_._2 ne offerer)) }
                           it.copy(offerers = name -> offerer :: it.offerers) -> poll(offerer.get).onCancel(cleanup)
                       }.flatten <* exec(code)
                     }
          _       <- b.await
          _       <- b2.await
        yield
          delay

      def apply(key: String)
               (deferred: Deferred[IO, Option[(Double, (-, -))]])
               (`<R`: >*<): IO[(Any, Double)] =
        for
          opt     <- deferred.get
          _       <- if opt eq None then IO.canceled else IO.unit
          (delay,
          (b, b2)) = opt.get
          taker   <- Deferred[IO, Any]
          name    <- IO.uncancelable { poll =>
                       `<R`.modify {
                         case it @ ><(_, offerers) if offerers.nonEmpty =>
                           val ((name, offerer), rest) = offerers.head -> offerers.tail
                           it.copy(offerers = rest) -> offerer.complete(()).as(name)
                         case it =>
                           val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_ ne taker)) }
                           it.copy(takers = taker :: it.takers) -> poll(taker.get).onCancel(cleanup)
                       }.flatten
                     }
          _       <- b.await
          _       <- b2.await
        yield
          name -> delay

      def apply[T](key: String)(code: T => IO[T])
                  (deferred: Deferred[IO, Option[(Double, (-, -))]])
                  (`<R`: >*<): IO[(Any, Double)] =
        for
          opt     <- deferred.get
          _       <- if opt eq None then IO.canceled else IO.unit
          (delay,
          (b, b2)) = opt.get
          taker   <- Deferred[IO, Any]
          name    <- IO.uncancelable { poll =>
                       `<R`.modify {
                         case it @ ><(_, offerers) if offerers.nonEmpty =>
                           val ((name, offerer), rest) = offerers.head -> offerers.tail
                           it.copy(offerers = rest) -> offerer.complete(()).as(name)
                         case it =>
                           val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_ ne taker)) }
                           it.copy(takers = taker :: it.takers) -> poll(taker.get).onCancel(cleanup)
                       }.flatten
                        .flatMap { case it: T => (code andThen exec)(it) }
                     }
          _       <- b.await
          _       <- b2.await
        yield
          name -> delay
