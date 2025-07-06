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

import munit.FunSuite

import StochasticPi.*
import Calculus.*
import Encoding.*
import StochasticPiSuite.*


class StochasticPiSuite extends FunSuite:

  test("agent-no-binding-1") {

    Main(getClass.getSimpleName) {
      source("""
             ⟦ t"Output" ⟧{x} = x<x>.
             ⟦1 t"$x Hardcoded ${$P}" 1⟧{u} = u(x). P{u}
             ⟦2 t"Encoded $x" 2⟧{u} = ⟦1 x Hardcoded ⟦ Output ⟧{x} 1⟧
             ⟦3 t"Encoded $x" 3⟧{u} = ⟦2 Encoded x 2⟧
             ⟦4 t"Encoded $x" 4⟧{u} = ⟦1 x Hardcoded x(y). 1⟧
             Main = ⟦4 Encoded x 4⟧
             """)
    }

  }

  test("agent-no-binding") {

    interceptMessage[NoBindingParsingException]("No binding for z at nesting level #1") {
      Main(getClass.getSimpleName) {
        source("""
               ⟦⟧ =
               P(u) = u(x). u(x). ( u(v). ⟦ z(x). ⟧ )
               """)
      }
    }

  }

  test("encoding-no-binding") {

    interceptMessage[NoBindingParsingException]("No binding for z at nesting level #1 in the right hand side of encoding 1") {
      Main(getClass.getSimpleName) {
        source("""
               ⟦⟧ =
               ⟦1 t"X" 1⟧ = ⟦ z(x). ⟧
               """)
      }
    }

  }

  test("encoding-uniqueness-hardcoded-binding") {

    interceptMessage[UniquenessBindingParsingException]("A binding name (x) does not correspond to a unique hardcoded binding occurrence, but is duplicated at nesting level #0 in the right hand side of encoding 1") {
      Main(getClass.getSimpleName) {
        source("""
               ⟦1 t"λ $x . ${$M}" 1⟧{u} = u(x). u(x). M{v}
               """)
      }
    }

  }

  test("encoding-uniqueness-encoded-binding") {

    interceptMessage[RuntimeException]("A binding name (z) does not correspond to a unique encoded binding occurrence, but is duplicated at nesting level #1 in the right hand side of encoding 3") {
      Main(getClass.getSimpleName) {
        source("""
               ⟦ 'x ⟧{u} = x<u>.
               ⟦2 t"λ $x,$y . ${$M}" 2⟧{u} = u(x). u(y). M{y}
               ⟦3 t"λλ $z" 3⟧ = ⟦2 λ z,z . ⟦ z ⟧ 2⟧
               """)
      }
    }

  }

  test("encoding-non-parameter-hardcoded-binding") {

    interceptMessage[NonParameterBindingParsingException]("A binding name (u) in a hardcoded binding occurrence does not correspond to a parameter at nesting level #0 in the right hand side of encoding 1") {
      Main(getClass.getSimpleName) {
        source("""
               ⟦1 t"λ $x . ${$M}" 1⟧{u} = x(u).
               """)
      }
    }

  }

  test("encoding-non-parameter-encoded-binding") {

    interceptMessage[RuntimeException]("A binding name (u) in an encoded binding occurrence does not correspond to a parameter at nesting level #1 in the right hand side of encoding 3") {
      Main(getClass.getSimpleName) {
        source("""
               ⟦ 'x ⟧{u} = x<u>.
               ⟦1 t"λ $x . ${$M}" 1⟧{u} = u(x). u(v). M{v}
               ⟦3 t"Id" 3⟧{u} = ⟦1 λ u . ⟦ z ⟧ 1⟧
               """)
      }
    }

  }


object StochasticPiSuite:

  def source(src: String) = Source.fromString(src)
