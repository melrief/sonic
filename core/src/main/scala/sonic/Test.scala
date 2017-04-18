/*
 * Copyright (c) 2017 Mario Pastorelli (pastorelli.mario@gmail.com)
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package sonic

import cats.{Alternative, Monad, MonoidK}
import cats.data.{EitherT, WriterT}
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.flatMap._

final case class Test[A](unTest: EitherT[WriterT[Gen[?], Vector[Log], ?], Failure, A]) {

  def run: Gen[(Vector[Log], Either[Failure, A])] =
    unTest.value.run
}

object Test extends TestInstances1

trait TestMethods {

  /** Write a log to be displayed if the test fails. */
  def writeLog(log: Log): Test[Unit] =
    liftRight(WriterT.tell(Vector(log)))

  /** Logs an information message to be displayed if the test fails. */
  def info(str: String): Test[Unit] =
    writeLog(Info(str))

  def fromGen[A](gen: Gen[A]): Test[A] =
    liftRight(WriterT.lift[Gen, Vector[Log], A](gen))

  /** Discards a test entirely. */
  def discard[A]: Test[A] =
    fromGen[A](Gen.discard[A])

  /** Generates a random input for the test by running the provided generator. */
  def forAll[A](gen: Gen[A]): Test[A] =
    fromGen[A](gen) >>= (x => info("dummy") >> x.pure[Test])

  /** Fail with an error message, useful for building other failure combinators. */
  def failWith[A](maybeDiff: Option[Diff], msg: String): Test[A] =
    liftLeft(WriterT.lift[Gen, Vector[Log], Failure](Failure(msg, maybeDiff).pure[Gen]))

  /** Causes a test to fail. */
  def failure[A]: Test[A] =
    failWith[A](None, "")

  /** Another name for `().pure[Test]`. */
  def success: Test[Unit] =
    ().pure[Test]

  /** Fails the test if the condition provided is `false`. */
  def assert(b: => Boolean): Test[Unit] =
    if (b) success else failure

  /** Fails the test if the `Either` is `Left`, otherwise returns the value in the `Right`. */
  def liftEither[X, A](either: Either[X, A]): Test[A] =
    either.fold(x => failWith(None, x.toString), _.pure[Test])

  /** Utility method to lift a `writerT` into a `EitherT.right` */
  def liftRight[A](writerT: WriterT[Gen, Vector[Log], A]): Test[A] =
    Test(EitherT.right[WriterT[Gen, Vector[Log], ?], Failure, A](writerT))

  /** Utility method to lift a `writerT` into a `EitherT.left` */
  def liftLeft[A](writerT: WriterT[Gen, Vector[Log], Failure]): Test[A] =
    Test(EitherT.left[WriterT[Gen, Vector[Log], ?], Failure, A](writerT))


}

private[sonic] abstract class TestInstances1 extends TestMethods {

  implicit val catsInstancesForTest: Monad[Test] with Alternative[Test] =
    new Monad[Test] with Alternative[Test] {
      def pure[A](a: A): Test[A] =
        Test.fromGen[A](a.pure[Gen])

      def flatMap[A, B](test: Test[A])(f: A => Test[B]): Test[B] =
        Test(test.unTest.flatMap(x => f(x).unTest))

      def tailRecM[A, B](a: A)(f: A => Test[Either[A, B]]): Test[B] =
        Test(Monad[EitherT[WriterT[Gen, Vector[Log], ?], Failure, ?]].tailRecM[A, B](a)(x => f(x).unTest))

      def empty[A]: Test[A] = Test.discard[A]

      def combineK[A](x: Test[A], y: Test[A]): Test[A] =
        Test(EitherT(WriterT(MonoidK[Gen].combineK(x.run, y.run))))
    }
}