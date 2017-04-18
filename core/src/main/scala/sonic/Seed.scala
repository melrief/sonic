/*
 * Copyright (c) 2017 Mario Pastorelli (pastorelli.mario@gmail.com)
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package sonic

import java.security.SecureRandom

import sonic.numeric.{mix32, mix64, mixGamma}

/** A splittable random number generator. */
// TODO refine gamma such that it's odd when github.com/fthomas/refined/issue/265
final case class Seed(value: Long, gamma: Long) {
  def next: (Long, Seed) = {
    val nextValue = value + gamma
    (nextValue, Seed(nextValue, gamma))
  }

  /** Split this in to two random number generators. */
  def split: (Seed, Seed) = {
    val (value0, seed0) = this.next
    val (gamma0, seed1) = seed0.next
    (seed1, Seed(mix64(value0), mixGamma(gamma0)))
  }

  def nextLong: (Long, Seed) = {
    val (value0, seed) = this.next
    (mix64(value0), seed)
  }
  
  def nextInt: (Int, Seed) = {
    val (value0, seed) = this.next
    (mix32(value0), seed)
  }

  def nextBool: (Boolean, Seed) = {
    val (value0, seed)  = this.nextInt
    (value0 % 2 == 0,  seed)
  }

  def nextIntBetween(min: Int, maxExcluded: Int): (Int, Seed) = {
    require(min <= maxExcluded, s"min($min) not <= max($maxExcluded)")
    if (min == maxExcluded) (min, this.next._2)
    else {
      val (value0, seed) = this.nextInt
      (min + (value0 % (maxExcluded - min)).abs, seed)
    }
  }

  def nextLongBetween(min: Long, maxExcluded: Long): (Long, Seed) = {
    require(min <= maxExcluded, s"min($min) not <- max($maxExcluded)")
    if (min == maxExcluded) (min, this.next._2)
    else {
      val (value0, seed) = this.nextLong
      (min + (value0 % (maxExcluded - min)).abs, seed)
    }
  }
}

object Seed {
  val goldenGamma: Long = -7046029254386353131L

  /** Create a `Seed` from the given long and [[goldenGamma]] as gamma */
  def from(long: Long): Seed =
    Seed(long, goldenGamma)

  /** Create a `Seed` from a random number and [[goldenGamma]] as gamma */
  def random(): Seed = {
    from(new SecureRandom().nextLong())
  }
}
