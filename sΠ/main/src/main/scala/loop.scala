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

import _root_.scala.collection.immutable.Map

import _root_.cats.instances.list.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.parallel.*
import _root_.cats.syntax.traverse.*

import _root_.cats.effect.{ IO, Deferred, ExitCode, Ref }
import _root_.cats.effect.std.{ CyclicBarrier, Queue, Semaphore }

import `Π-stats`.*


package object `Π-loop`:

  private val spirsx = "pisc.stochastic.replications.exitcode.ignore"

  import sΠ.{ `Π-Map`, `Π-Set`, >*< }
  export sΠ.`π-exclude`

  type - = CyclicBarrier[IO]

  type + = (Deferred[IO, Option[(Double, (-, -))]], (>*<, Option[Boolean], Rate))

  type % = Ref[IO, Map[String, Int | +]]

  type ! = Deferred[IO, ExitCode]

  type * = Queue[IO, Unit]

  type / = Queue[IO, ((String, String), +)]

  type \ = () => IO[Unit]


  def `π-enable`(enabled: `Π-Set`[String])
                (using % : %): IO[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                   val n = if m.contains(key)
                                           then m(key).asInstanceOf[Int]
                                           else 0
                                   m + (key -> (n + 1))
                                 }
    )

  private def ready(key: String)
                   (using % : %)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    val (_, spell) = `π-wand`
    ( if spell.contains(key)
      then
        `π-enable`(spell(key))
      else
        IO.unit
    )


  private def unblock(m: Map[String, Int | +], k: String)
                     (implicit ^ : String): IO[Unit] =
    if m.contains(^ + k)
    then m(^ + k).asInstanceOf[+]._1.complete(None).void
    else IO.unit

  private def `π-discard`(discarded: `Π-Set`[String])
                         (using % : %)
                         (implicit ^ : String): IO[Unit] =
    for
      m <- %.get
      _ <- if discarded.isEmpty then IO.unit
           else discarded.toList.traverse(unblock(m, _)).void
      _ <- %.update(discarded.map(^ + _).foldLeft(_)(_ - _))
    yield
      ()

  private def discard(key: String, scope: String)
                     (using % : %)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    val (trick, _) = `π-wand`
    if trick.contains(key)
    then
      implicit val ^ : String = scope
      `π-discard`(trick(key))
    else
      IO.unit


  private def exit(ks: List[String])
                  (using % : %, ! : !): IO[Unit] =
    if ks.isEmpty
    then
      !.complete(ExitCode.Success).void
    else
      ks
        .traverse { key =>
          %.modify { m => m -> m(key).asInstanceOf[+]._1 } >>= (_.complete(None))
        }
        .as {
          if !sys.BooleanProp.keyExists(spirsx).value
          && ks.forall(_.charAt(36) == '!')
          then ExitCode.Success
          else ExitCode.Error
        } >>= (!.complete(_).void)


  def loop(parallelism: Int)
          (using % : %, ! : !, * : *)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    %.modify { m =>
               m -> ( if m.exists(_._2.isInstanceOf[Int])
                      then Map.empty -> false
                      else m
                           .map(_ -> _.asInstanceOf[+]._2)
                           .toMap
                        -> m.forall { case (key, _: +) => key.charAt(36) == '!' case _ => false }
                    )
    } >>= { case (it, exit) =>
            if exit
            then
              this.exit(it.map(_._1).toList)
            else if it.isEmpty
            then
              *.take >> loop(parallelism)
            else
              ∥(it)(`π-wand`._1)(parallelism) match
                case Nil =>
                  this.exit(it.map(_._1).toList)
                case nel =>
                  nel.parTraverse { case (key1, key2, delay) =>
                                    val k1 = key1.substring(36)
                                    val k2 = key2.substring(36)
                                    val ^  = key1.substring(0, 36)
                                    val ^^ = key2.substring(0, 36)
                                    for
                                      -  <- CyclicBarrier[IO](if k1 == k2 then 2 else 3)
                                      -- <- CyclicBarrier[IO](if k1 == k2 then 2 else 3)
                                      d1 <- %.modify { m => m -> m(key1).asInstanceOf[+]._1 }
                                      d2 <- %.modify { m => m -> m(key2).asInstanceOf[+]._1 }
                                      _  <- discard(k1, ^)
                                      _  <- if k1 == k2 then IO.unit else discard(k2, ^^)
                                      _  <- %.update(_ - key1 - key2)
                                      _  <- d1.complete(Some(delay -> (-, --)))
                                      _  <- if k1 == k2 then IO.unit else d2.complete(Some(delay -> (-, --)))
                                      _  <- -.await
                                      _  <- ready(k1)
                                      _  <- if k1 == k2 then IO.unit else ready(k2)
                                      _  <- --.await
                                    yield
                                      ()
                                  } >> loop(parallelism)
          }

  def poll(using % : %, / : /, * : *): IO[Unit] =
    for
      h <- /.take
      ((_, key), it) = h
      _ <- %.update { m =>
                      val ^ = h._1._1
                      val n = m(key).asInstanceOf[Int] - 1
                      ( if n == 0
                        then
                          m - key
                        else
                          m + (key -> n)
                      ) + (^ + key -> it)
           }
      _ <- *.offer(())
      _ <- IO.cede >> poll
    yield
      ()
