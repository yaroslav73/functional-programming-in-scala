package chapter13._02_io_without_so

import chapter13.Monad

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
          case Suspend(resume) => run(f(resume()))
          case FlatMap(sub, g) => run(sub.flatMap(a => g(a).flatMap(f)))
        }
    }

  def apply[A](a: => A): IO[A] = unit(a)

  def unit[A](a: => A): IO[A] = Return(a)

  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)

  def suspend[A](a: => IO[A]): IO[A] = Suspend(() => ()).flatMap(_ => a)
}
