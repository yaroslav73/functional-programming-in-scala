package chapter13._02_io_without_so_better_naming

import chapter13.Monad

import scala.annotation.tailrec

sealed trait TailRec[A] {
  def map[B](f: A => B): TailRec[B] =
    flatMap(f.andThen(a => Return(a)))

  def flatMap[B](f: A => TailRec[B]): TailRec[B] =
    FlatMap(this, f)
}

/*
  A pure computation that immediately returns an A without any further steps.
  When run sees this constructor, it knows the computation has finished.
 */
final case class Return[A](a: A) extends TailRec[A]

/*
  A suspension of the computation where resume is a function that takes no arguments,
  but has some effect and yields a result.
 */
final case class Suspend[A](resume: () => A) extends TailRec[A]

/*
  A composition of two steps. Reifies flatMap as a data constructor rather than a function.
  When run sees this, it should first process the subcomputation sub
  and then continue with k once sub produces a result.
 */
final case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

object TailRec extends Monad[TailRec] {
  @tailrec
  def run[A](io: TailRec[A]): A =
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

  def apply[A](a: => A): TailRec[A] = unit(a)

  def unit[A](a: => A): TailRec[A] = Return(a)

  def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] = fa.flatMap(f)

  def suspend[A](a: => TailRec[A]): TailRec[A] = Suspend(() => ()).flatMap(_ => a)
}
