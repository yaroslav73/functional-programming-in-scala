package chapter15

import chapter13.Monad
import chapter15.Process.{lift, loop}

sealed trait Process[I, O] {
  def apply(in: LazyList[I]): LazyList[O] =
    this match {
      case Halt() => LazyList.empty[O]
      case Await(recovery) =>
        in match {
          case head #:: tail => recovery(Some(head))(tail)
          case xs            => recovery(None)(xs)
        }
      case Emit(head, tail) => head #:: tail(in)
    }

  def repeat: Process[I, O] = {
    def loop(p: Process[I, O]): Process[I, O] =
      p match {
        case Halt() => loop(this)
        case Await(recovery) =>
          Await {
            case None => recovery(None)
            case elem => loop(recovery(elem))
          }
        case Emit(head, tail) => Emit(head, loop(tail))
      }

    loop(this)
  }

  def |>[O2](p: Process[O, O2]): Process[I, O2] =
    p match {
      case Halt()           => Halt()
      case Emit(head, tail) => Emit(head, this |> tail)
      case Await(f) =>
        this match {
          case Emit(head, tail) => tail |> f(Some(head))
          case Await(g)         => Await(i => g(i) |> p)
          case Halt()           => Halt[I, O]() |> f(None)
        }
    }

  def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] =
    this match {
      case Halt()           => Halt()
      case Emit(head, tail) => f(head) ++ tail.flatMap(f)
      case Await(recovery)  => Await(recovery.andThen(_ flatMap f))
    }

  def ++(p: Process[I, O]): Process[I, O] =
    this match {
      case Halt()           => p
      case Emit(head, tail) => Emit(head, tail ++ p)
      case Await(recovery)  => Await(recovery.andThen(_ ++ p))
    }

  def zipWithIndex: Process[I, (O, Int)] = {
    def loop(n: Int, p: Process[I, O]): Process[I, (O, Int)] =
      p match {
        case Halt()           => Halt()
        case Emit(head, tail) => Emit((head, n), loop(n + 1, tail))
        case Await(recovery) =>
          Await {
            case None => Halt()
            case elem => loop(n, recovery(elem))
          }
      }

    loop(0, this)
  }
}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Await[I, O](recovery: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]

object Process {
  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(value) => Emit(f(value))
      case _           => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] =
    liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(value) if p(value) => Emit(value)
      case _                       => Halt()
    }.repeat

  def take[I](n: Int): Process[I, I] = {
    def loop(n: Int): Process[I, I] =
      Await {
        case Some(value) if n != 0 => Emit(value, loop(n - 1))
        case _                     => Halt()
      }

    loop(n)
  }

  def drop[I](n: Int): Process[I, I] = {
    def loop(n: Int): Process[I, I] =
      Await {
        case Some(_) if n != 0 => loop(n - 1)
        case Some(value)       => Emit(value, loop(n))
        case _                 => Halt()
      }

    loop(n)
  }

  def takeWhile[I](p: I => Boolean): Process[I, I] = filter(p)

  def dropWhile[I](p: I => Boolean): Process[I, I] = {
    Await[I, I] {
      case Some(value) if p(value) => dropWhile(p)
      case Some(value)             => Emit(value)
      case _                       => Halt()
    }.repeat
  }

  def count[I]: Process[I, Int] =
    loop(0)((_, s) => (s + 1, s + 1))

  def sum: Process[Double, Double] =
    loop(0.0)((i, s) => (i + s, i + s))

  def mean: Process[Double, Double] = {
    def loop(counter: Int, acc: Double): Process[Double, Double] =
      Await {
        case Some(value) => Emit((acc + value) / (counter + 1), loop(counter + 1, acc + value))
        case None        => Halt()
      }

    loop(0, 0)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    Await {
      case Some(i) =>
        f(i, z) match {
          case (o, s2) => Emit(o, loop(s2)(f))
        }
      case None => Halt()
    }

  def zip[A, B, C](p1: Process[A, B], p2: Process[A, C]): Process[A, (B, C)] =
    (p1, p2) match {
      case (Halt(), _)                => Halt()
      case (_, Halt())                => Halt()
      case (Emit(b, t1), Emit(c, t2)) => Emit((b, b), zip(t1, t2))
      case (Await(recovery), _)       => Await((oa: Option[A]) => zip(recovery(oa), feed(oa)(p2)))
      case (_, Await(recovery))       => Await((oa: Option[A]) => zip(feed(oa)(p1), recovery(oa)))
    }

  private def feed[A, B](oa: Option[A])(p: Process[A, B]): Process[A, B] =
    p match {
      case Halt()           => Halt()
      case Await(recovery)  => recovery(oa)
      case Emit(head, tail) => Emit(head, feed(oa)(tail))
    }

  def monad[I]: Monad[({ type f[x] = Process[I, x] })#f] =
    new Monad[({ type f[x] = Process[I, x] })#f] {
      def unit[A](a: => A): Process[I, A] = Emit(a)
      def flatMap[A, B](a: Process[I, A])(f: A => Process[I, B]): Process[I, B] = a.flatMap(f)
    }
}
