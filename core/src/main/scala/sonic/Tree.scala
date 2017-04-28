/*
 * Copyright (c) 2017 Mario Pastorelli (pastorelli.mario@gmail.com)
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package sonic

import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.{Alternative, Applicative, Functor, Id, Monad, MonadFilter, MonoidK, SemigroupK, TransLift}

import scala.language.higherKinds

/**
 * An effectful tree, each node in the tree can have an effect before
 * it is produced
 */
final case class Tree[M[_], A](runTree: M[Node[M, A]]) {
  
  /**
   * Expand a [[Tree]] using an unfolding function.
   *
   * A [[Node]] is expanded by applying `f` to its `value` and then
   * adding the produced children to the result of expanding in
   * recursively all its `children`
   */
  def expand(f: A => List[A])(implicit M0: Monad[M]): Tree[M, A] = {
    Tree(
      runTree >>= {
        case Node(x, xs) =>
          M0.pure(Node(x, () => xs().map(_.expand(f)) ++ Tree.unfoldForest(f)(x)))
      })
  }
  
  /** Throw away a [[Tree]]'s children */
  def prune(implicit M0: Monad[M]): Tree[M, A] = {
    Tree(
      runTree >>= {
        case Node(x, _) => M0.pure(Node(x, () => List.empty[Tree[M, A]]))
      })
  }

}

/** A node of an effectful [[Tree]] with a `value` and the unevaluated `children` */
final case class Node[M[_], A](value: A, children: () => List[Tree[M, A]])

object Tree extends TreeInstances1 {
  
  def unfold[M[_], A](f: A => List[A])(z: A)(implicit M0: Monad[M]): Tree[M, A] = {
    Tree(M0.pure(Node(z, () => unfoldForest[M, A](f)(z))))
  }
  
  def unfoldForest[M[_], A](f: A => List[A])(z: A)(implicit M0: Monad[M]): List[Tree[M, A]] = {
    f(z).map(unfold[M, A](f))
  }

  def liftF[F[_], A](ma: F[A])(implicit F: Functor[F]): Tree[F, A] =
    Tree(F.map(ma)(a => Node(a, () => List.empty[Tree[F, A]])))

  def runOption[A](tree: Tree[Option, A]): Tree[Id, Option[A]] =
    Tree[Id, Option[A]](
      tree.runTree match {
        case None => Node[Id, Option[A]](None, () => List())
        case Some(Node(x, subTrees)) => Node[Id, Option[A]](Some(x), () => subTrees().map(runOption))
      }
    )
}

private[sonic] abstract class TreeInstances1 extends TreeInstances2 {
  implicit def catsMonadForNode[M[_]](implicit M0: Monad[M]): Monad[Node[M, ?]] =
    new NodeMonad[M] { override val M = M0 }

  implicit def catsMonadForTree[M[_]](implicit M0: Monad[M], F0: MonoidK[M]): Monad[Tree[M, ?]] =
    new TreeMonadFilter[M] { override val M = M0; override val F = F0 }

  implicit def catsTransLiftForTree: TransLift.Aux[Tree, Functor] =
    new TransLift[Tree] {
      type TC[M[_]] = Functor[M]
      def liftT[M[_]: Functor, A](ma: M[A]): Tree[M, A] = Tree.liftF(ma)
    }
}

private[sonic] abstract class TreeInstances2 extends TreeInstances3 {
  implicit def catsAlternativeForTree[M[_]](implicit M0: Monad[M], F0: MonoidK[M]): Alternative[Tree[M, ?]] =
    new TreeAlternative[M] { override val M = M0; override val F = F0 }
}

private[sonic] abstract class TreeInstances3 {
  implicit def catsFunctorForNode[F[_]](implicit F0: Functor[F]): Functor[Node[F, ?]] =
    new NodeFunctor[F] { override val F = F0 }

  implicit def catsFunctorForTree[F[_]](implicit F0: Functor[F]): Functor[Tree[F, ?]] =
    new TreeFunctor[F] { override val F = F0 }

  implicit def catsApplicativeForNode[M[_]](implicit M0: Monad[M]): Applicative[Node[M, ?]] =
    new NodeApplicative[M] { implicit val M = M0 }

  implicit def catsApplicativeForTree[M[_]](implicit M0: Monad[M]): Applicative[Tree[M, ?]] =
    new TreeApplicative[M] { implicit val M = M0 }

  implicit def catsMonoidKForTree[M[_]](implicit M0: MonoidK[M]): MonoidK[Tree[M, ?]] =
    new TreeMonoidK[M] { override def F = M0 }
}

private[sonic] trait NodeFunctor[F[_]] extends Functor[Node[F, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](node: Node[F, A])(f: A => B): Node[F, B] = {
    Node(f(node.value), () => node.children().map(child => Functor[Tree[F, ?]].map(child)(f)))
  }
}

private[sonic] trait TreeFunctor[F[_]] extends Functor[Tree[F, ?]] { self =>
  implicit def F: Functor[F]
  val NF = new NodeFunctor[F] { override val F = self.F }

  override def map[A, B](tree: Tree[F, A])(f: A => B): Tree[F, B] = {
      Tree(F.map(tree.runTree)(node => NF.map(node)(f)))
  }
}

private[sonic] trait TreeSemigroupK[F[_]] extends SemigroupK[Tree[F, ?]] {
  implicit def F: SemigroupK[F]

  override def combineK[A](x: Tree[F, A], y: Tree[F, A]): Tree[F, A] =
    Tree(F.combineK(x.runTree, y.runTree))
}

private[sonic] trait TreeMonoidK[F[_]] extends MonoidK[Tree[F, ?]] with TreeSemigroupK[F] {
  implicit def F: MonoidK[F]

  override def empty[A]: Tree[F, A] = Tree(F.empty[Node[F, A]])
}

private[sonic] trait NodeApplicative[M[_]] extends Applicative[Node[M, ?]] {
  implicit val M: Monad[M]

  override def pure[A](a: A): Node[M, A] = Node(a, () => List.empty[Tree[M, A]])
  override def ap[A, B](ff: Node[M, A => B])(fa: Node[M, A]): Node[M, B] = {
    Node(
      ff.value(fa.value),
      () => (ff.children() zip fa.children()).map { case (ff, fa) => Applicative[Tree[M, ?]].ap(ff)(fa) })
  }
}

private[sonic] trait TreeApplicative[M[_]] extends Applicative[Tree[M, ?]] { self =>
  implicit val M: Monad[M]
  val NA = new NodeApplicative[M] { override val M = self.M }

  override def pure[A](a: A): Tree[M, A] = Tree(M.pure(Node(a, () => List.empty[Tree[M, A]])))
  override def ap[A, B](ff: Tree[M, A => B])(fa: Tree[M, A]): Tree[M, B] = {
    Tree(ff.runTree >>= (nff => fa.runTree >>= (nfa => NA.ap(nff)(nfa).pure)))
  }
}

private[sonic] trait TreeAlternative[M[_]]
  extends Alternative[Tree[M, ?]]
  with TreeApplicative[M]
  with TreeMonoidK[M]

private[sonic] trait NodeMonad[M[_]] extends Monad[Node[M, ?]] with NodeFunctor[M] { self =>
  implicit def M: Monad[M]
  override val F = M

  override def pure[A](a: A): Node[M, A] = Node(a, () => List.empty[Tree[M, A]])
  override def flatMap[A, B](tree: Node[M, A])(f: A => Node[M, B]): Node[M, B] = {
    val Node(value, children) = f(tree.value)
    Node(
      value,
      () => tree.children().map(child => Tree(M.map(child.runTree)(node => flatMap(node)(f)))) ++ children())
  }
  override def tailRecM[A, B](a: A)(f: A => Node[M, Either[A, B]]): Node[M, B] = {
    f(a) match {
      case Node(Left(a0), _) => tailRecM[A, B](a0)(f)
      case Node(Right(b), _) => pure(b)
    }
  }
}

private[sonic] trait TreeMonad[M[_]] extends Monad[Tree[M, ?]] {
  implicit def M: Monad[M]

  override def pure[A](a: A): Tree[M, A] = Tree(M.pure(Node(a, () => List.empty[Tree[M, A]])))
  override def flatMap[A, B](tree: Tree[M, A])(f: A => Tree[M, B]): Tree[M, B] = {
    Tree(
      tree.runTree >>= {
        case Node(x, xs) =>
          f(x).runTree >>= {
            case Node(y, ys) =>
              M.pure(Node(y, () => xs().map(x => flatMap(x)(f)) ++ ys()))
          }
      })
  }
  override def tailRecM[A, B](a: A)(f: A => Tree[M, Either[A, B]]): Tree[M, B] = {
    flatMap(f(a)) {
      case Right(b) => Tree(M.pure(Node(b, () => List.empty[Tree[M, B]])))
      case Left(a0) => tailRecM[A, B](a0)(f)
    }
  }
}

private[sonic] trait TreeMonadFilter[M[_]] extends MonadFilter[Tree[M, ?]] with TreeMonad[M] { self =>
  implicit def M: Monad[M]
  implicit def F: MonoidK[M]
  val treeMonoidK = new TreeMonoidK[M] { override val F = self.F }

  def empty[A]: Tree[M, A] = treeMonoidK.empty[A]
}
