/*
 * Copyright (c) 2017 Mario Pastorelli (pastorelli.mario@gmail.com)
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package sonic

import cats.Id
import cats.data.NonEmptyVector
import cats.instances.all._
import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.auto._
import monix.eval._
import monix.cats._
import monix.execution.Scheduler.Implicits.global
import sonic.numeric.{GreaterEqualZero, ZeroOrPositive}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Runner {

  def checkGroup(group: PropertyGroup): Boolean = {
    def nameAndPropertyToTask(nameAndProperty: (String, Property)): Task[Boolean] =
      Task {
        val isSuccess = checkNamed(nameAndProperty._1, nameAndProperty._2)
        println(s"  ${nameAndProperty._1}: $isSuccess")
        isSuccess
      }

    val tasks: List[Task[Boolean]] = group.properties.map(nameAndPropertyToTask)(scala.collection.breakOut)

    println("━━━ " ++ group.name ++ " ━━━")

    Await.result(tasks.sequence[Task, Boolean].map(_.forall(identity)).runAsync, Duration.Inf)
  }

  def checkNamed(name: String, property: Property): Boolean = {
    val seed = Seed.random()
    check(name, 0, seed, property).status == Ok
  }

  def check(name: String, size0: ZeroOrPositive, seed0: Seed, property: Property): Report = {
    def loop(testCount: Int, discardCount: Int, size: Int, seed: Seed): Report = {
      if (size == 99) {
        loop(testCount, discardCount, 0, seed)
      } else if (testCount >= property.config.testLimit) {
        Report(testCount, discardCount, Ok)
      } else if (discardCount >= property.config.discardLimit) {
        Report(testCount, discardCount, GaveUp)
      } else {
        val refinedSize = refineV[GreaterEqualZero](size).right.get
        val (s0, s1) = seed.split
        val node@(Node(x, _)) = Tree.runOption(property.test.run.run(refinedSize)(s0)).runTree
        x match {
          case None =>
            loop(testCount, refineV[GreaterEqualZero](discardCount + 1).right.get, size + 1, s1)
          case Some((_, Left(failure))) =>
            Report(testCount + 1, discardCount, takeSmallest(refinedSize, seed, 0, property.config.shrinkLimit, node))
          case Some((_, Right(()))) =>
            loop(testCount + 1, discardCount, size + 1, s1)
        }
      }
    }
    loop(0, 0, size0.value, seed0)
  }

  def takeSmallest(
    size: ZeroOrPositive,
    seed: Seed,
    shrinkCount0: ZeroOrPositive,
    shrinkLimit: ZeroOrPositive,
    node0: Node[Id, Option[(scala.Vector[Log], scala.Either[Failure, Unit])]]): Result = {

    def loop(shrinkCount: Int, node: Node[Id, Option[(scala.Vector[Log], scala.Either[Failure, Unit])]]): Result =
      node match {
        case Node(None, _) =>
          GaveUp
        case Node(Some((logs, leftOrRight)), xs) =>
          leftOrRight match {
            case Left(failure) =>
              val failed =
                Failed(FailureReport(size, seed, refineV[GreaterEqualZero](shrinkCount).right.get,
                  NonEmptyVector.of(FailedInput("", "")), failure.why, failure.maybeDiff, List.empty[String]))
              if (shrinkCount >= shrinkLimit) {
                failed
              } else {
                findM(xs(), failed.asInstanceOf[Result]) {
                  subTree =>
                    val node = subTree.runTree
                    if (isFailure(node)) {
                      loop(shrinkCount + 1, node).some
                    } else None
                }
              }
            case Right(()) =>
              Ok
          }
    }

    loop(shrinkCount0.value, node0)
  }

  @annotation.tailrec
  def findM[A, B](xs0: List[A], default: B)(f: A => Option[B]): B = {
    xs0 match {
      case List() =>
        default
      case v =>
        f(v.head) match {
          case None => findM(v.tail, default)(f)
          case Some(x) => x
        }
    }
  }

  def isFailure[A](node: Node[cats.Id, Option[(Vector[Log], Either[Failure, Unit])]]): Boolean =
    node.value.map(_._2.isLeft).getOrElse(false)
}
