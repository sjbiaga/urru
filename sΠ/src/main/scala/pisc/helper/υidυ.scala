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
package helper

import scala.collection.mutable.Seq


final class υidυ:

  private var id = Seq[Char]('0')
  private var ix = 0

  /**
    * @return unique identifier of the form "_υ[0-9a-zA-Z]+υ"
    */
  def apply(): String =
    var reset = false
    while ix >= 0 && id(ix) == 'Z'
    do
      id(ix) = '0'
      ix -= 1
      reset = true
    if ix < 0
    then
      id :+= '1'
    else
      id(ix) match
        case 'z' =>
          id(ix) = 'A'
        case '9' =>
          id(ix) = 'a'
        case it =>
          id(ix) = (it + 1).toChar
    if reset then ix = id.size - 1
    "_υ" + id.mkString + "υ"

  def copy: Any =
    ix -> Seq.from(id)

  def paste(it: Any): Unit =
    val (ix, id) = it.asInstanceOf[(Int, Seq[Char])]
    this.ix = ix
    this.id = id

  def save[T](e: => Option[T]): Option[T] =
    val it = copy
    e match
      case it @ Some(_) => it
      case _ =>
        paste(it)
        None
