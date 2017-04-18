/*
 * Copyright (c) 2017 Mario Pastorelli (pastorelli.mario@gmail.com)
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package sonic

import cats.data.NonEmptyVector
import eu.timepit.refined._
import sonic.numeric.{GreaterEqualZero, ZeroOrPositive}


final case class FailedInput(typ: String, value: String)

final case class FailureReport(
    size: ZeroOrPositive,
    seed: Seed,
    shrinks: ZeroOrPositive,
    inputs: NonEmptyVector[FailedInput],
    /* maybeLocation: Option[Span] */
    message: String,
    maybeDiff: Option[Diff],
    messages: List[String])

sealed abstract class Result
final case class Failed(report: FailureReport) extends Result
final case object GaveUp extends Result
final case object Ok extends Result

final case class Report(
  numTests: ZeroOrPositive,
  numDiscarded: ZeroOrPositive,
  status: Result)

object Report {

  @throws[NoSuchElementException](cause = s"numTests or numDiscarded are negative")
  def apply(numTests: Int, numDiscarded: Int, status: Result): Report =
    new Report(refineV[GreaterEqualZero](numTests).right.get, refineV[GreaterEqualZero](numDiscarded).right.get, status)
}