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

import _root_.scala.collection.immutable.{ List, Map, Set }
import _root_.scala.collection.mutable.HashMap
import _root_.scala.concurrent.duration.*

import _root_.breeze.stats.distributions.Exponential
import _root_.breeze.stats.distributions.Rand.VariableSeed.*

import _root_.cats.effect.{ IO, Ref }

import _root_.com.github.blemale.scaffeine.{ Scaffeine, Cache }


package object `Π-stats`:

  import sΠ.{ `Π-Map`, `Π-Set`, >*< }

  sealed trait Rate extends Any
  case class ∞(weight: Long) extends AnyVal with Rate
  case class `ℝ⁺`(rate: BigDecimal) extends AnyVal with Rate
  case class ⊤(weight: Long) extends AnyVal with Rate

  private val distributionCache: Cache[Double, Exponential] =
    Scaffeine()
      .recordStats()
      .expireAfterWrite(1.hour)
      .maximumSize(500)
      .build[Double, Exponential]()

  private inline def distribution(rate: Double) =
    distributionCache.getIfPresent(rate).getOrElse {
      val it = Exponential(rate)
      distributionCache.put(rate, it)
      it
    }

  private inline def delta(rate: BigDecimal): Double =
    distribution(rate.toDouble).draw()

  class StatisticsException(msg: String, cause: Throwable = null)
      extends RuntimeException(msg, cause)

  case class CombinedActivitiesException(how: String)
      extends StatisticsException("The immediate and/or timed and/or passive activities must not be " + how)

  def ∥(% : Map[String, (>*<, Option[Boolean], Rate)])
       (`π-trick`: `Π-Map`[String, `Π-Set`[String]])
       (parallelism: Int, check: Boolean = false): List[(String, String, Double)] =
                                                      // ^^^^^^  ^^^^^^  ^^^^^^
                                                      // key1    key1|2  duration

    val mls = HashMap[(>*<, Option[Boolean]), List[Either[Long, Either[BigDecimal, Long]]]]() // lists

    %
      .foreach {
        case (_, (e, p, _)) =>
          mls(e -> p) = Nil
      }

    %
      .foreach {
        case (_, (e, p, r: ∞)) => // immediate
          mls(e -> p) :+= Left(r.weight)
        case (_, (e, p, r: `ℝ⁺`)) => // timed
          mls(e -> p) :+= Right(Left(r.rate))
        case (_, (e, p, r: ⊤)) => // passive
          mls(e -> p) :+= Right(Right(r.weight))
      }

    val msrt = HashMap[(>*<, Option[Boolean]), BigDecimal]() // [timed] sums of rates

    mls // timed
      .foreach {
        case (ep, ls) =>
          val rs = ls
            .filter(_.isRight)
            .filter(_.right.get.isLeft)
            .map(_.right.get.left.get)
          if rs.nonEmpty
          then
            msrt(ep) = rs.sum
      }

    val mswi = HashMap[(>*<, Option[Boolean]), Long]() // [immediate] sums of weights

    mls // immediate
      .foreach {
        case (ep, ls) =>
          val ws = ls
            .filter(_.isLeft)
            .map(_.left.get)
          if ws.nonEmpty
          then
            mswi(ep) = ws.sum
      }

    val mswp = HashMap[(>*<, Option[Boolean]), Long]() // [passive] sums of weights

    mls // passive
      .foreach {
        case (ep, ls) =>
          val ws = ls
            .filter(_.isRight)
            .filter(_.right.get.isRight)
            .map(_.right.get.right.get)
          if ws.nonEmpty
          then
            mswp(ep) = ws.sum
      }

    if check
    then
      val ert = msrt.keySet.map(_._1)
      val ewi = mswi.keySet.map(_._1)
      val ewp = mswp.keySet.map(_._1)

      if (ert & ewi).nonEmpty
      || (ert & ewp).nonEmpty
      || (ewi & ewp).nonEmpty
      then
        throw CombinedActivitiesException("mixed")

    val χ = %
      .map {
        case (k, (e, p, r: ∞)) => k -> (e, p, Double.NaN -> r.weight) // immediate
        case (k, (e, p, r: `ℝ⁺`)) => k -> (e, p, r.rate.toDouble -> 0L) // timed
        case (k, (e, p, r: ⊤)) => k -> (e, p, Double.NaN -> r.weight) // passive
      }.toSeq

    var r = List[((String, String, Double), (Int, Double))]()
    //             ^^^^^^  ^^^^^^  ^^^^^^    ^^^  ^^^^^^
    //             key1    key1|2  duration  pri  delay

    for
      i <- 0 until χ.size
      (key1, (ether1, polarity1, (rate1, weight1))) = χ(i)
    do
      if polarity1 eq None
      then
        val (rate, (priority, duration)) =
          if msrt.contains(ether1 -> polarity1)
          then
            BigDecimal(1) * rate1 -> (2 -> Double.PositiveInfinity)
          else if mswi.contains(ether1 -> polarity1)
          then
            BigDecimal(1) * weight1 -> (1 -> 0.0)
          else if mswp.contains(ether1 -> polarity1)
          then
            BigDecimal(1) * weight1 -> (3 -> Double.NaN)
          else
            ???
        val delay = delta(rate)
        r :+= (key1, key1, if priority == 2 then delay else duration) -> (priority -> delay)
      else
        val ^ = key1.substring(0, 36)
        for
          j <- i+1 until χ.size
          (key2, (ether2, polarity2, (rate2, weight2))) = χ(j)
          if polarity2 ne None
        do
          if (ether1 eq ether2) && polarity1.get != polarity2.get
          then
            val ^^ = key2.substring(0, 36)
            if ^ != ^^
            || {
              val k1 = key1.substring(36)
              val k2 = key2.substring(36)
              !`π-trick`.contains(k1) || !`π-trick`(k1).contains(k2)
            }
            then
              val (rate, (priority, duration)) =
                if msrt.contains(ether1 -> polarity1)
                && msrt.contains(ether2 -> polarity2)
                then
                  val apr1 = msrt(ether1 -> polarity1)
                  val apr2 = msrt(ether2 -> polarity2)
                  ((rate1 / apr1) * (rate2 / apr2) * (apr1 min apr2)) -> (2 -> Double.PositiveInfinity)
                else if mswi.contains(ether1 -> polarity1)
                     && mswi.contains(ether2 -> polarity2)
                then
                  val apr1 = mswi(ether1 -> polarity1)
                  val apr2 = mswi(ether2 -> polarity2)
                  ((BigDecimal(1) * weight1 / apr1) * (BigDecimal(1) * weight2 / apr2) * (apr1 min apr2)) -> (1 -> 0.0)
                else if mswp.contains(ether1 -> polarity1)
                     && mswp.contains(ether2 -> polarity2)
                then
                  val apr1 = mswp(ether1 -> polarity1)
                  val apr2 = mswp(ether2 -> polarity2)
                  ((BigDecimal(1) * weight1 / apr1) * (BigDecimal(1) * weight2 / apr2) * (apr1 min apr2)) -> (3 -> Double.NaN)
                else
                  ???
              val delay = delta(rate)
              if polarity2.get
              then
                r :+= (key1, key2, if priority == 2 then delay else duration) -> (priority -> delay)
              else
                r :+= (key2, key1, if priority == 2 then delay else duration) -> (priority -> delay)

    r = r.sortBy(_._2).reverse

    ( for
        (((key1, key2, _), _), i) <- r.zipWithIndex
      yield
        val k1 = key1.substring(36)
        val k2 = key2.substring(36)
        val ^ = key1.substring(0, 36)
        val ^^ = key2.substring(0, 36)
        r(i)._1 -> {
          0 > r.indexWhere(
            {
              case ((`key1` | `key2`, _, _), _) | ((_, `key1` | `key2`, _), _) => true
              case ((key, _, _), _)
                  if {
                    val k = key.substring(36)
                    `π-trick`.contains(k) && {
                      val ^^^ = key.substring(0, 36)
                      `π-trick`(k).contains(k1) && ^ == ^^^ || `π-trick`(k).contains(k2) && ^^ == ^^^
                    }
                  } => true
              case ((_, key, _), _)
                  if {
                    val k = key.substring(36)
                    `π-trick`.contains(k) && {
                      val ^^^ = key.substring(0, 36)
                      `π-trick`(k).contains(k1) && ^ == ^^^ || `π-trick`(k).contains(k2) && ^^ == ^^^
                    }
                  } => true
              case _ => false
            }
            , i + 1
          )
        }
    )
    .filter(_._2)
    .map(_._1)
    .reverse
    .take(parallelism)
