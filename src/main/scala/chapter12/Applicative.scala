package chapter12

import chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def ap[A, B](f: F[A => B])(fa: F[A]): F[B] = map2(fa, f)((a, f) => f(a))
  def unit[A](a: => A): F[A]

  // derived combinators
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    ap(ap(unit(f.curried))(fa))(fb) // -> is the same as ap(map(fa)(f.curried))(fb)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(unit(f))(fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb) { case (a, b) => (a, b) }
}
