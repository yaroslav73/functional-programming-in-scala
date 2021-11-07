package chapter13._04_free_monad

import chapter07.Nonblocking.Par
import chapter13.Monad

import scala.annotation.tailrec

sealed trait Free[F[_], A]
final case class Return[F[_], A](a: A) extends Free[F, A]
final case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
final case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f] =
    new Monad[({ type f[a] = Free[F, a] })#f] {
      def unit[A](a: => A): Free[F, A] = Return(a)
      def flatMap[A, B](a: Free[F, A])(f: A => Free[F, B]): Free[F, B] = a.flatMap(f)
    }

  @tailrec
  def runTrampoline[A](a: Free[Function0, A]): A =
    a match {
      case Return(a)  => a
      case Suspend(s) => s()
      case FlatMap(s, f) =>
        s match {
          case Return(a)     => runTrampoline(f(a))
          case Suspend(s)    => runTrampoline(f(s()))
          case FlatMap(s, g) => runTrampoline(FlatMap(FlatMap(a, g), f))
        }
    }
}
