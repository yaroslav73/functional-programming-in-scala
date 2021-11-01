package chapter12

import chapter06.State

trait Monad[F[_]] extends Applicative[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}

object Monad {
  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] =
    new Monad[({ type f[x] = Either[E, x] })#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
        fa match {
          case Left(e)  => Left(e)
          case Right(a) => f(a)
        }
    }

  def stateMonad[S]: Monad[({ type F[X] = State[S, X] })#F] =
    new Monad[({ type F[X] = State[S, X] })#F] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)
    }
}
