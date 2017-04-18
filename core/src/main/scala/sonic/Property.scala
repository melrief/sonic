/*
 * Copyright (c) 2017 Mario Pastorelli (pastorelli.mario@gmail.com)
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package sonic

import cats.data.{EitherT, Writer, WriterT}
import cats.{Alternative, Monad, MonoidK}
import cats.instances.either._
import cats.instances.vector._
import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import sonic.numeric.{GreaterEqualZero, ZeroOrPositive}

final case class Property(config: PropertyConfig, test: Test[Unit]) {

  def mapConfig(f: PropertyConfig => PropertyConfig): Property =
    copy(config = f(config))

  def withTests(testLimit: ZeroOrPositive): Property =
    mapConfig(_.copy(testLimit = testLimit))

  def withDiscards(discardLimit: ZeroOrPositive): Property =
    mapConfig(_.copy(discardLimit = discardLimit))

  def withShrinks(shrinkLimit: ZeroOrPositive): Property =
    mapConfig(_.copy(shrinkLimit = shrinkLimit))

  def withTestsUnsafe(testLimit: Int): Property =
    withTests(refineV[GreaterEqualZero](testLimit).right.get)

  def withDiscardsUnsafe(discardLimit: Int): Property =
    withDiscards(refineV[GreaterEqualZero](discardLimit).right.get)

  def withShrinksUnsafe(shrinkLimit: Int): Property =
    withShrinks(refineV[GreaterEqualZero](shrinkLimit).right.get)
}

final case class Failure(
  /*maybeSpan: Option[Span],*/
  why: String,
  maybeDiff: Option[Diff]
)

sealed abstract class Log
final case class Info(str: String) extends Log

final case class PropertyConfig(
  testLimit: ZeroOrPositive,
  discardLimit: ZeroOrPositive,
  shrinkLimit: ZeroOrPositive)

final case class PropertyGroup(name: String, properties: Map[String, Property])

object PropertyGroup {
  // TODO: compile time check of the property names?
  @throws[RuntimeException](cause = "property names contains duplicates")
  def apply(name: String)(properties: (String, Property)*): PropertyGroup = {
    val propNamesDuplicated = properties.map(_._1).groupBy(identity).mapValues(_.size).filter(_._2 > 1)
    if (propNamesDuplicated.isEmpty) {
      new PropertyGroup(name, properties.toMap)
    } else {
      throw new RuntimeException(s"Found properties with duplicated names: ${propNamesDuplicated.mkString(",")}")
    }
  }
}

final case class Diff(
  prefix: String,
  removed: String,
  infix: String,
  added: String,
  suffix: String/**,
  value: ValueDiff*/
)

object PropertyConfig {
  def default: PropertyConfig =
    PropertyConfig(
      testLimit = 100,
      discardLimit = 100,
      shrinkLimit = 1000
    )
}

object Property {
  def apply(test: Test[Unit]): Property = Property(PropertyConfig.default, test)
}
