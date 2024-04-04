package urru
package common

import scala.annotation.tailrec

import common.grid.{ row, x, col, adj1 }


// a spiral holds a token: at each next point, if the 'tokens' set
// has the token, then it may pass it to the adjacent points,
// otherwise does not
case class Spiral(livens: Seq[Point],   // initial or reliven points
                  tokens: Set[Point],   // points where token was passed
                  passed: Set[Point]):  // points which passed the token

  /**
    * Return the token points and the final result.
    * Traverse [the points of] a spiral with a recursive algorithm;
    * token points that did not "pass" the token reliven afresh.
    */
  protected def apply[R](hist: Boolean                                         // history tokens
                        ,stop: R => Boolean                                    // stop condition
                        ,path: ((Point, Point), Set[Point], R) => (Option[Boolean], R)  // path callback
                        ): (((Point, Int), (Int, Int)))                        // size, translate coordinate
                        => R                                                   // start result
                        => (Set[Point],                                        // token points,
                                        R)                                     // end result
                        =
    { case ((s, d), (dx, dy)) =>

        extension(self: Spiral)
          @tailrec
          protected def spin(retn: R): (Set[Point], R) =

////////////////////////////////////////////////////////////////////////////////

            inline def map_(ps: Set[Point]) = ps.map(d_tr)
            def d_tr(p: Point) = (p.row + dx) x (p.col + dy)

            def trim(p: Point) =
              val q = d_tr(p)
              (-d+1) <= q.row && q.row <= (s.row+d) &&
              (-d+1) <= q.col && q.col <= (s.col+d)

            inline def pt = self.livens.head

            def adj1 = pt.adj1.filter(trim).filterNot(self.tokens.contains)

            def pass(srl1: (Boolean, R, Seq[Point]), p: Point): (Boolean, R, Seq[Point]) =
              val (s1, r1, l1) = srl1
              if s1 then
                srl1
              else
                val tokens1 = if hist then map_(self.tokens) else Set.empty
                path(d_tr(pt) -> d_tr(p), tokens1, r1) match
                  case (Some(t), r) => (stop(r), r, if t then p +: l1 else l1)
                  case (_      , r) => (true   , r, l1)


            inline def loop(ps: Set[Point]) =
              self
                .copy(livens = Seq.from(ps))
                .spin(retn)

            inline def next(retn1: R, tokens1: Set[Point]) =
              self1
                .copy(tokens = tokens1,
                      passed = self.passed + pt)
                .spin(retn1)

            inline def self1 = self.copy(livens = self.livens.tail)

////////////////////////////////////////////////////////////////////////////////

            if self.livens.isEmpty
            then
              val ps = self.tokens -- self.passed
              if ps.isEmpty
              then
                map_(self.tokens) -> retn
              else
                loop(ps)
            else if self.tokens.contains(pt) -> self.passed.contains(pt)
            then
              self1.spin(retn)
            else
              val (stop1, retn1, ps) = adj1.foldLeft((false, retn, Nil))(pass)
              val tokens1 = self.tokens ++ ps
              if stop1
              then
                map_(tokens1) -> retn1
              else
                next(retn1, tokens1)

        this.spin(_)
    }

  def apply[R](history: Boolean                                      // history tokens
              ,returning: R                                          // start result
              ,stop: R => Boolean                                    // stop condition
             )(magic: ((Point, Int), (Int, Int))                     // size, translate coordinate
             )(path: ((Point, Point), Set[Point], R) => (Option[Boolean], R)  // path callback
             ): (Set[Point],                                         // token points,
                             R)                                      // end result
              = this(history, stop, path)(magic)(returning)


object Spiral:

  /**
    * Adapted from source "Spiral.scala" in chapter 10 of the book:
    * "Programming in Scala" - fourth edition, by Martin Odersky, Lex Spoon, Bill Venners
    *
    * Create a list of anti-clockwise-ordered points as a spiral.
    */
  private def apply(s: Int, d: Int, n: Int, pt: Point, ps: Point*): Seq[Point] =
    if s == ps.size then
      ps
    else
      d match
        case 0 =>
          this(s, 1, n, pt.row x (pt.col-n), ps ++ (for i <- 1 to n yield pt.row x (pt.col-i))*)
        case 1 =>
          this(s, 2, n+1, (pt.row+n) x pt.col, ps ++ (for i <- 1 to n yield (pt.row+i) x pt.col)*)
        case 2 =>
          this(s, 3, n, pt.row x (pt.col+n), ps ++ (for i <- 1 to n yield pt.row x (pt.col+i))*)
        case _ =>
          this(s, 0, n+1, (pt.row-n) x pt.col, ps ++ (for i <- 1 to n yield (pt.row-i) x pt.col)*)

  def apply(size: Point, d: Int): Spiral =
    val pt = (1 + (size.row + 2*d) / 2) x (1 + (size.col + 2*d) / 2)
    val s = size.row max size.col
    val ps = this((2*(s+2*d)*(s+2*d)*2), 0, 1, pt)
    Spiral(pt +: ps, Set(pt), Set())

  /**
    * Return the four [[Int]] parameters to [[urru.common.Spiral#apply]].
    */
  def magic0(p: Point)(s: Point, d: Int = 0): ((Point, Int), (Int, Int)) =
    (s -> d) -> ((p.row - (1+(s.row+2*0)/2)) x (p.col - (1+(s.col+2*0)/2)))

  def magic1(p: Point)(s: Point, d: Int = 1): ((Point, Int), (Int, Int)) =
    (s -> d) -> ((p.row - (1+(s.row+2*1)/2)) x (p.col - (1+(s.col+2*1)/2)))

  def magic2(p: Point)(s: Point, d: Int = 2): ((Point, Int), (Int, Int)) =
    (s -> d) -> ((p.row - (1+(s.row+2*2)/2)) x (p.col - (1+(s.col+2*2)/2)))
