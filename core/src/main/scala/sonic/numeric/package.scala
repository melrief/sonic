/*
 * Copyright (c) 2017 Mario Pastorelli (pastorelli.mario@gmail.com)
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package sonic

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.numeric.GreaterEqual

package object numeric {

  type GreaterEqualZero = GreaterEqual[W.`0`.T]
  type ZeroOrPositive = Int Refined GreaterEqualZero

  def mix64(x: Long): Long = {
    val y = (x ^ (x >> 33)) * -49064778989728563L
    val z = (y ^ (y >> 33)) * -4265267296055464877L
    z ^ (z >> 33)
  }

  def mix32(x: Long): Int = {
    val y = (x ^ (x >> 33)) * -49064778989728563L
    val z = (y ^ (y >> 33)) * -4265267296055464877L
    (z >> 32).toInt
  }
  
  def mix64variant13(x: Long): Long = {
    val y = (x ^ (x >> 30)) * -4658895280553007687L
    val z = (y ^ (y >> 27)) * -7723592293110705685L
    z ^ (z >> 31)
  }

  def mixGamma(x: Long): Long = {
    val y = mix64variant13(x) | 1
    val n = popCount(y ^ (y >> 1))
    if (n < 24) y ^ -6148914691236517206L
    else y

  }

  def popCount(x: Long): Int = {
    @annotation.tailrec
    def bits (i: Long, sofar: Int): Int =
      if (i==0) sofar else bits (i >> 1, (1 & i).toInt + sofar)
    bits(x, 0)
  }
}
