/*
 * Copyright (c) 2017 Mario Pastorelli (pastorelli.mario@gmail.com)
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package sonic

import scala.language.higherKinds
import scala.collection.generic.CanBuildFrom
import scala.math.Integral
import cats.{Alternative, Applicative, Functor, Monad, MonadFilter, MonoidK, Traverse}
import cats.data.{NonEmptyList, NonEmptyVector}
import cats.instances.option._
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.functorFilter._
import cats.syntax.flatMap._
import cats.syntax.option._
import cats.syntax.semigroupk._
import cats.syntax.traverse._
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import sonic.Tree._
import sonic.shrink._
import sonic.numeric.{GreaterEqualZero, ZeroOrPositive}

import scala.collection.mutable

/**
 * Generator of random values of tye `A`.
 *
 * Given a `size` and a `seed`, a generator creates a `Tree` of possible values.
 */
final case class Gen[A](run: ZeroOrPositive => Seed => Tree[Option, A]) {

  /** Map a function `f` over the genereated shrink Tree */
  def mapTree[B](f: Tree[Option, A] => Tree[Option, B]): Gen[B] =
    Gen((size: ZeroOrPositive) => (seed: Seed) => f(run(size)(seed)))

  /**
   * Freeze the size and the seed of this generator, so that we can inspect
   * the value which it will produce.
   *
   * It allows, for instance, to shrink a collection before shrinking the
   * values inside the collection.
   */
  def freeze: Gen[(A, Gen[A])] =
    Gen((size: ZeroOrPositive) => (seed: Seed) => {
      run(size)(seed).runTree match {
        case None =>
          MonoidK[Tree[Option, ?]].empty[(A, Gen[A])]
        case Some(node@Node(x, _)) =>
          val tree = Tree(node.pure[Option])
          ((x, Gen.liftTree(tree))).pure[Tree[Option, ?]]
      }
    })

  /**
    * Apply a shrinking function to this generator
    *
    * This will add additional shrinking options, while keeping the existing shrink intact.
    *
    */
  def shrink(f: A => List[A]): Gen[A] = mapTree(_.expand(f))

  /** Throw away the shrink tree. */
  def prune: Gen[A] = mapTree(_.prune)

  /**
   * Return a generator which uses the given `size` instead of the
   * runtime-size parameter
   */
  def resize(size: ZeroOrPositive): Gen[A] =
    Gen((_: ZeroOrPositive) => (seed: Seed) => run(size)(seed))

  /** Adjust the `size` parameter */
  def scale(f: ZeroOrPositive => ZeroOrPositive): Gen[A] =
    Gen.sized((size: ZeroOrPositive) => resize(f(size)))

  /** Make a smaller generator by scaling its size parameter */
  def smaller: Gen[A] =
    scale(scaleByGolden)

  def scaleByGolden: ZeroOrPositive => ZeroOrPositive = (x: ZeroOrPositive) =>
    refineV[GreaterEqualZero]((x.value.toDouble * 0.61803398875D).toInt).right.get

  /**
   * Generate a value that satisfies a predicate.
   */ 
  def filter(p: A => Boolean): Gen[A] = {
    def tryK(k: Int): Gen[A] = {
      if (k > 100) MonoidK[Gen].empty[A]
      else
        scale(x => refineV[GreaterEqualZero](2 * k + x.value).right.get) >>= {
          value =>
            if (p(value)) value.pure[Gen]
            else tryK(k + 1)
        }
    }
    tryK(0)
  }

  def collect[B](pf: PartialFunction[A, B]): Gen[B] =
    this >>= (a => if (pf.isDefinedAt(a)) pf(a).pure[Gen] else MonoidK[Gen].empty[B])
}

object Gen extends GenInstances1 {

  /** Generate a value with no shrinks */
  def generate[A](f: ZeroOrPositive => Seed => A): Gen[A] =
    Gen((size: ZeroOrPositive) => (seed: Seed) => f(size)(seed).pure[Tree[Option, ?]])

  /** Lift a predefined shrink tree in to a generator, ignoring seed and size */
  def liftTree[A](tree: Tree[Option, A]): Gen[A] =
    Gen(_ => _ => tree)

  /** Construct a generator that depends on the size parameter */
  def sized[A](f: ZeroOrPositive => Gen[A]): Gen[A] =
    Gen((size: ZeroOrPositive) => (seed: Seed) => f(size).run(size)(seed))

  /**
    * Construct a generator of random integers within the range
    *
    * This generator doesn't shrink
    */
  def intNoShrink(range: Range[Int]): Gen[Int] =
    generate {
      (size: ZeroOrPositive) => (seed: Seed) =>
        val (x, y) = range.bounds(size)
        seed.nextIntBetween(x, y)._1
    }

  /** Same as `intNoShrink` but shrinks towards the range origin */
  def int(range: Range[Int]): Gen[Int] =
    intNoShrink(range).shrink(towards(range.origin, _))

  def longNoShrink(range: Range[Long]): Gen[Long] =
    generate {
      (size: ZeroOrPositive) => (seed: Seed) =>
        val (x, y) = range.bounds(size)
        seed.nextLongBetween(x, y)._1
    }

  def long(range: Range[Long]): Gen[Long] =
    longNoShrink(range).shrink(towards(range.origin, _))

  /**
    * Randomly selects one of the elements from the given list
    *
    * This generator shrinks towards the first element of the list.
    */
  def element[A](xs: NonEmptyVector[A]): Gen[A] =
    int(Range.constant(0, xs.length - 1)).map(i => xs.getUnsafe(i))

  /** Alias for [[element]] */
  def oneOf[A](xs: NonEmptyVector[A]): Gen[A] =
    element[A](xs)

  /**
    * Generate a random boolean.
    *
    * Shrinks towards `false`.
    */
  def bool: Gen[Boolean] =
    element(NonEmptyVector.of(false, true))

  /**
    * Generate a random char between `'0'` and `'1'`
    *
    * Shrinks towards `'0'`
    */
  def binit: Gen[Char] =
    element(NonEmptyVector.of('0', '1'))

  /**
    * Generate a random char between `'0'` and `'7'`
    *
    * Shrinks towards `'0'`
    */
  def octit: Gen[Char] =
    element(NonEmptyVector.of('0', '1', '2', '3', '4', '5', '6', '7'))

  /**
    * Generate a random char between `'0'` and `'9'`
    *
    * Shrinks towards `'0'`
    */
  def digit: Gen[Char] =
    element(NonEmptyVector.of('0', '1', '2', '3', '4', '5', '6', '7', '8', '9'))

  /**
    * Generate a random hexadecimal char
    *
    * Shrinks towards `'0'`
    */
  def hexit: Gen[Char] =
    element(NonEmptyVector.of('0', "123456789aAbBcCdDeEfF" : _*))

  /**
    * Generate a random char in the alphabet lower-case
    *
    * Shrinks towards `'a'`
    */
  def lower: Gen[Char] =
    element(NonEmptyVector.of('a', 'b' to 'z' :_*))

  /**
    * Generate a random char in the alphabet upper-case
    *
    * Shrinks towards `'A'`
    */
  def upper: Gen[Char] =
    element(NonEmptyVector.of('A', 'B' to 'Z' :_*))

  /**
    * Generate a random char in the alphabet
    *
    * Shrinks towards `'a'`
    */
  def alpha: Gen[Char] =
    element(NonEmptyVector.of('a', "bcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" :_*))

  /**
    * Generate a random char in the alphabet or a number
    *
    * Shrinks towards `'a'`
    */
  def alphaNum: Gen[Char] =
    element(NonEmptyVector.of(
      'a', "bcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" :_*))

  /**
    * Generate a random string
    */
  def string(range: Range[Int])(charGen: Gen[Char]): Gen[String] =
    collection[List, Char](range)(charGen).map(_.mkString)

  /**
    * Randomly select one generator from the given ones.
    *
    * Shrinks towards the first generator of the list
    */
  def choice[A](gens: NonEmptyVector[Gen[A]]): Gen[A] =
    int(Range.constant(0, gens.length -1)).flatMap(gens.getUnsafe)

  /**
    * Randomly select one generator from the given ones using a weighted distribution.
    *
    * Shrinks towards the first generator in the list
    */
  def frequency[A](gens: NonEmptyList[(Int, Gen[A])]): Gen[A] = {
    val total = gens.foldLeft(0)(_ + _._1)
    def pick(gens: List[(Int, Gen[A])])(n :Int): Gen[A] = {
      if (gens.head._1 >= n) gens.head._2
      else pick(gens.tail)(n - gens.head._1)
    }
    int(Range.constant(1, total)) >>= pick(gens.toList)
  }

  def recursive[A](f: List[Gen[A]] => Gen[A], nonrec: List[Gen[A]], rec: List[Gen[A]]): Gen[A] =
    sized((size: ZeroOrPositive) => if (size.value <= 1) f(nonrec) else f(nonrec ++ rec.map(_.smaller)))

  /** Discard the whole generator. This is another name for `MonoidK[Gen].empty[A]` */
  def discard[A]: Gen[A] = MonoidK[Gen].empty[A]

  /**
    * Runs the given `Option` generator until it produces `Some` value
    */
  def some[A](gen: Gen[Option[A]]): Gen[A] =
    gen.collect { case Some(x) => x }

  /**
    * Generate a random collection using the given `range` to determine the length.
    */
  def collection[C[_], A](range: Range[Int])(gen0: Gen[A])(implicit cbf: CanBuildFrom[C[A], A, C[A]]): Gen[C[A]] =

    Gen.sized {
      (_: ZeroOrPositive) =>
        Gen.int(range) >>= {
          length =>
            val refinedLength = refineV[GreaterEqualZero](length).right.get

            List.fill(length)(gen0.freeze).sequence[Gen, (A, Gen[A])]
              .shrink(shrinkList)
              .filter(_.size >= range.lowerBound(refinedLength))
              .flatMap(s => s.traverse(_._2))
              .map {
                s =>
                  val b = cbf()
                  b ++= s
                  b.result()
              }
        }
    }

  def nonEmptyList[A](range: Range[Int])(gen: Gen[A]): Gen[NonEmptyList[A]] =
    collection[List, A](range.map(_ max 1))(gen).map(NonEmptyList.fromListUnsafe)

  def nonEmptyVector[A](range: Range[Int])(gen: Gen[A]): Gen[NonEmptyVector[A]] =
    collection[Vector, A](range.map(_ max 1))(gen).map(NonEmptyVector.fromVectorUnsafe)

  /** @note the generated set could have a length not in `range` */
  // TODO fix this such that either the size is respected or an error is thrown
  def set[A](range: Range[Int])(gen: Gen[A]): Gen[Set[A]] =
    collection[Set, A](range)(gen)

  def list[A](range: Range[Int])(gen: Gen[A]): Gen[List[A]] =
    collection[List, A](range)(gen)

  def map[K, V](range: Range[Int])(gen: Gen[(K, V)]): Gen[Map[K, V]] =
    collection[List, (K, V)](range)(gen).map(_.toMap)

  /** Generate either `Some` value of type `A` or `None`. `None` has lower weight the values */
  def option[A](gen: Gen[A]): Gen[Option[A]] =
    Gen.sized(
      (size: ZeroOrPositive) =>
        Gen.frequency(
          NonEmptyList.of(
            2 -> Option.empty[A].pure[Gen],
            (1 + size.value) -> gen.map(_.some))))

}

private[sonic] abstract class GenInstances1 {

  implicit val catsInstancesForGen: MonadFilter[Gen[?]] with MonoidK[Gen[?]] =
    new MonadFilter[Gen[?]] with MonoidK[Gen[?]]{

      override def map[A, B](gen: Gen[A])(f: A => B): Gen[B] =
        Gen((size: ZeroOrPositive) => (seed: Seed) =>  gen.run(size)(seed).map(f))

      override def ap[A, B](ff: Gen[A => B])(fa: Gen[A]): Gen[B] =
        Gen(
          (size: ZeroOrPositive) => (seed: Seed) =>
            ff.run(size)(seed) >>= (
              f => fa.run(size)(seed) >>= (
                a => f(a).pure[Tree[Option, ?]])))

      override def pure[A](a: A): Gen[A] = Gen.liftTree(a.pure[Tree[Option, ?]])

      override def flatMap[A, B](gen: Gen[A])(f: A => Gen[B]): Gen[B] =
        Gen(
          (size: ZeroOrPositive) => (seed: Seed) => {
            val (seedK, seedM) = seed.split
            gen.run(size)(seedM) >>= (a => f(a).run(size)(seedK))
          }
        )
      
      override def tailRecM[A, B](a: A)(f: A => Gen[Either[A, B]]): Gen[B] =
        f(a) >>= (_.fold(tailRecM[A, B](_)(f), pure))

      override def empty[A]: Gen[A] = Gen.liftTree(MonoidK[Tree[Option, ?]].empty[A])

      override def combineK[A](gl: Gen[A], gr: Gen[A]): Gen[A] = {
        Gen(
          (size: ZeroOrPositive) => (seed: Seed) => {
            val (seedK, seedM) = seed.split
            MonoidK[Tree[Option, ?]].combineK(gl.run(size)(seedK), gr.run(size)(seedM))
          }
        )
      }
    }
}
