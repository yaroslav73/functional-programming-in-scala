package chapter13._04_free_monad

import chapter07.Nonblocking.Par
import chapter13.Monad
import chapter13._05_console_io.Console
import chapter13._05_console_io.Translate.{consoleToFunction0, consoleToPar, ~>}

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

  @tailrec
  def step[F[_], A](free: Free[F, A]): Free[F, A] =
    free match {
      case FlatMap(FlatMap(sub, f), g) => step(FlatMap(FlatMap(sub, f), g))
      case FlatMap(Return(x), f)       => step(f(x))
      case _                           => free
    }

  def run[F[_], A](a: Free[F, A])(implicit M: Monad[F]): F[A] =
    step(a) match {
      case Return(a)  => M.unit(a)
      case Suspend(s) => s
      case FlatMap(s, f) =>
        s match {
          case Suspend(s) => M.flatMap(s)(a => run(f(a)))
          case _          => sys.error("Impossible, since `step` eliminates this cases")
        }
    }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a)              => G.unit(a)
      case Suspend(s)             => t(s)
      case FlatMap(Suspend(s), f) => G.flatMap(t(s))(a => runFree(f(a))(t))
      case _                      => sys.error("Impossible, since `step` eliminates this cases")
    }

  def translate[F[_], G[_]: Monad, A](free: Free[F, A])(fg: F ~> G): Free[G, A] = {
    val instance = implicitly[Monad[G]]
    Suspend(runFree(free)(fg)(instance))
  }

  implicit val function0Monad: Monad[Function0] = new Monad[Function0] {
    def unit[A](a: => A): () => A = () => a
    def flatMap[A, B](a: () => A)(f: A => () => B): () => B =
      () => f(a())()
  }
  implicit val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
      Par.fork { Par.flatMap(a)(f) }
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    runFree[Console, Par, A](a)(consoleToPar)

  def runConsole[A](a: Free[Console, A]): A =
    runTrampoline(translate(a)(consoleToFunction0))
}
