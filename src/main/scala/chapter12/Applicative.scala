package chapter12

import chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def unit[A](a: => A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldLeft(unit(List.empty[A]))((acc, a) => map2(acc, a)((l, a) => a :: l))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    if (n <= 0) unit(List.empty[A])
    else map2(fa, replicateM(n - 1, fa))((a, acc) => a :: acc)

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb) { case (a, b) => (a, b) }
}
