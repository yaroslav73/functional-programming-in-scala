package chapter11

import chapter06.State

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  // TODO: notice flatMap inside foldLeft - it's looks like map2 :)
  def sequence[A](lfa: List[F[A]]): F[List[A]] =
    lfa.foldLeft(unit(List.empty[A]))((acc, fa) => flatMap(fa)(a => map(acc)(list => list :+ a)))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)((b, lb) => b :: lb))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def filterM[A](la: List[A])(f: A => F[Boolean]): F[List[A]] =
    la match {
      case Nil => unit(List.empty[A])
      case h :: t => map2(f(h), filterM(t)(f))((b, l) => if (b) h :: l else l)
    }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => a -> b)
}

object Monad {
  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Option(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List.empty[A]
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  def stateMonad[S]: Monad[({ type F[X] = State[S, X] })#F] =
    new Monad[({ type F[X] = State[S, X] })#F] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)
    }
}
