package chapter13._03_async_io

import chapter07.Nonblocking._
import chapter13.Monad

import scala.annotation.tailrec

sealed trait Async[A] {
  def map[B](f: A => B): Async[B] =
    flatMap(f.andThen(a => Return(a)))

  def flatMap[B](f: A => Async[B]): Async[B] =
    FlatMap(this, f)
}

/*
  A pure computation that immediately returns an A without any further steps.
  When run sees this constructor, it knows the computation has finished.
 */
final case class Return[A](a: A) extends Async[A]

/*
  A suspension of the computation where resume is a function that takes no arguments,
  but has some effect and yields a result.
 */
final case class Suspend[A](resume: Par[A]) extends Async[A]

/*
  A composition of two steps. Reifies flatMap as a data constructor rather than a function.
  When run sees this, it should first process the subcomputation sub
  and then continue with k once sub produces a result.
 */
final case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

object Async extends Monad[Async] {
  @tailrec
  def step[A](async: Async[A]): Async[A] =
    async match {
      case FlatMap(FlatMap(sub, f), g) => step(sub.flatMap(a => f(a).flatMap(g)))
      case FlatMap(Return(x), f)       => step(f(x))
      case _                           => async
    }

  def run[A](io: Async[A]): Par[A] =
    io match {
      case Return(a)       => Par.unit(a)
      case Suspend(resume) => resume
      case FlatMap(sub, f) =>
        sub match {
          case Suspend(resume) => Par.flatMap(resume)(a => run(f(a)))
          case _               => sys.error("Impossible, since `step` eliminates this cases")
        }
    }

  def apply[A](a: => A): Async[A] = unit(a)

  def unit[A](a: => A): Async[A] = Return(a)

  def flatMap[A, B](fa: Async[A])(f: A => Async[B]): Async[B] = fa.flatMap(f)
}
