/*
 * Copyright (c) 2017 Mario Pastorelli (pastorelli.mario@gmail.com)
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package sonic

import scala.math.Integral
import cats.Functor
import cats.instances.tuple._
import cats.syntax.bifunctor._
import sonic.numeric.ZeroOrPositive

/**
  * A [[Range]] describes the bounds of a number to generate, which may or may not be dependent on a 'Size'.
  *
  * @param origin the origin of this [[Range]]. It may be the mid-point or the lower bound, depending on what this
  *               [[Range]] represents. The bounds of this [[Range]] are scaled around this value when using the 'linear'
  *               family of combinators. When using a [[Range]] to generate numbers, the shrinking function will shrink
  *               toward the origin.
  * @param bounds get the extents of this [[Range]], for a given size
  */
final case class Range[A](origin: A, bounds: ZeroOrPositive => (A, A)) {
  def lowerBound(size: ZeroOrPositive): A = bounds(size)._1
  def upperBound(size: ZeroOrPositive): A = bounds(size)._2
}

object Range extends RangeInstances1 {

  /** Construct a [[Range]] which represents a constant value */
  def singleton[A](a: A): Range[A] = new Range(a, _ => (a, a))

  /** Construct a range which is unaffected by the size parameter */
  def constant[A](lowerBoundAndOrigin: A, upperBound: A): Range[A] =
    constantFrom[A](lowerBoundAndOrigin, lowerBoundAndOrigin, upperBound)

  /** Same as `constant` but with the origin specified manually */
  def constantFrom[A](origin: A, lowerBound: A, upperBound: A): Range[A] =
    new Range(origin, _ => (lowerBound, upperBound))

  /** Construct a range which scales the second bound relative to the size parameter */
  def linear[A](x: A, y: A)(implicit i: Integral[A]): Range[A] =
    linearFrom[A](x, x, y)

  /** Same as `linear` but with the origin specified manually */
  def linearFrom[A](origin: A, x: A, y: A)(implicit i: Integral[A]): Range[A] = {
    val boundsF =
      (size: ZeroOrPositive) => {
        val xSized = clamp(x, y, scaleLinear(size, origin, x))
        val ySized = clamp(x, y, scaleLinear(size, origin, y))
        (xSized, ySized)
      }
    new Range(origin, boundsF)
  }

  /** Same as `linear` but for fractional values */
  def linearFrac[A](x: A, y: A)(implicit f: Fractional[A]): Range[A] =
    linearFracFrom[A](x, x, y)

  /** Same as `linearFrom` but for fractional values */
  def linearFracFrom[A](origin: A, x: A, y: A)(implicit f: Fractional[A]): Range[A] = {
    val boundsF =
      (size: ZeroOrPositive) => {
        val xSized = clamp(x, y, scaleLinearFrac(size, origin, x))
        val ySized = clamp(x, y, scaleLinearFrac(size, origin, y))
        (xSized, ySized)
      }
    new Range(origin, boundsF)
  }


  // utils

  /** Truncate a value so it stays within some range. */
  def clamp[A](x: A, y: A, n: A)(implicit ordA: Ordering[A]): A =
    if (ordA.gt(x, y)) ordA.min(x, (ordA.max(y, n)))
    else ordA.min(y, ordA.max(x, n))

  /** Scale an integral linearly with the size parameter. */
  def scaleLinear[A](size0: ZeroOrPositive, z: A, n: A)(implicit i: Integral[A]): A = {
    val sz = i.fromInt(0 max (99 min size0.value))
    i.plus(z, i.quot(i.times(i.minus(n, z), sz), i.fromInt(99)))
  }

  /** Same as `scaleLinear` but for fractional values */
  def scaleLinearFrac[A](size0: ZeroOrPositive, z: A, n: A)(implicit f: Fractional[A]): A = {
    val sz = f.fromInt(0 max (99 min size0.value))
    val diff = f.times(f.minus(n, z), f.div(sz, f.fromInt(99)))
    f.plus(z, diff)
  }
}

private[sonic] abstract class RangeInstances1 {

  implicit val catsFunctorForRange: Functor[Range] = new Functor[Range] {
    def map[A, B](range: Range[A])(f: A => B): Range[B] =
      new Range(f(range.origin), (size: ZeroOrPositive) => range.bounds(size).bimap(f, f))
  }
}
