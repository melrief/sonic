/*
 * Copyright (c) 2017 Mario Pastorelli (pastorelli.mario@gmail.com)
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package sonic

package object shrink {

  private def two[A](implicit I: Integral[A]): A = I.fromInt(2)

  def towards[A](dest: A, x: A)(implicit I: Integral[A]): List[A] =
    if (I.equiv(dest, x)) List.empty[A]
    else {
      val diff = I.minus(I.quot(x, two), I.quot(dest, two))
      consNub(dest, halves(diff).map(y => I.minus(x, y)))
    }

  def towardsFloat(dest: Float, x: Float): List[Float] = {
    if (dest == x) List.empty[Float]
    else {
      val diff = x - dest
      def isOk(y: Float): Boolean = y != x && !y.isNaN && !y.isInfinite
      Stream.iterate(diff)(_ / 2).map(x - _).takeWhile(isOk).toList
    }
  }

  /**
    * Shrink a list by edging towards the empty list.
    *
    * @note we always try the empty list first, as that is the optimal shrink.
    */
  def shrinkList[A](as: List[A]): List[List[A]] =
    halves(as.length).flatMap(k => removes(k, as))

  /**
   * Produce all permutations of removing 'k' elements from a list.
   */
  def removes[A](k0: Int, as0: List[A]): List[List[A]] = {
    def loop(k: Int, n: Int, as: List[A]): List[List[A]] = {
      if (k > n) List.empty[List[A]]
      else {
        as.splitAt(k) match {
          case (_, List()) =>
            List(List.empty[A])
          case (head, tail) =>
            tail +: loop(k, n - k, tail).map(head ++ _)
        }
      }
    }
    loop(k0, as0.size, as0)
  }

  /**
   * Produce a list containing the progressive halving of an integral.
   */
  def halves[A](a: A)(implicit I: Integral[A]): List[A] =
     Stream
      .iterate(a)(x => I.quot(x, I.fromInt(2)))
      .takeWhile(x => !I.equiv(x, I.fromInt(0)))
      .toList

  /**
   * Cons an element on to the front of a list unless it is already there.
   *
   * @param x the element to add if it's not equal to the head of the `ys0`
   * @param ys0 the list
   */
  def consNub[A](x: A, ys0: List[A])(implicit i: Integral[A]): List[A] = {
    ys0 match {
      case Nil => List(x)
      case yys if i.equiv(x, yys.head) => yys
      case yys => x +: yys
    }
  }
}
