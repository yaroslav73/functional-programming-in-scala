package chapter13

import scala.annotation.tailrec

sealed trait IO[A] {
  def map[B](f: A => B): IO[B] =
    flatMap(f.andThen(a => Return(a)))

  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap(this, f)
}

/*
  A pure computation that immediately returns an A without any further steps.
  When run sees this constructor, it knows the computation has finished.
 */
final case class Return[A](a: A) extends IO[A]

/*
  A suspension of the computation where resume is a function that takes no arguments,
  but has some effect and yields a result.
 */
final case class Suspend[A](resume: () => A) extends IO[A]

/*
  A composition of two steps. Reifies flatMap as a data constructor rather than a function.
  When run sees this, it should first process the subcomputation sub
  and then continue with k once sub produces a result.
 */
final case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO extends Monad[IO] {
  @tailrec
  def run[A](io: IO[A]): A =
    io match {
      case Return(a)       => a
      case Suspend(resume) => resume()
      case FlatMap(sub, f) =>
        sub match {
          case Return(a)       => run(f(a))
          case Suspend(resume) => run(f(resume))
          case FlatMap(sub, g) => run(sub.flatMap(a => g(a).flatMap(f)))
        }
    }

  def apply[A](a: => A): IO[A] = unit(a)

  def unit[A](a: => A): IO[A] = new IO[A] { def run: A = a }

  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)

  def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }
  sealed class IORef[A](var value: A) {
    def set(a: A): IO[A] = IO { value = a; a }
    def get: IO[A] = IO { value }
    def modify(f: A => A): IO[A] = get.flatMap(a => set(f(a)))
  }
}
