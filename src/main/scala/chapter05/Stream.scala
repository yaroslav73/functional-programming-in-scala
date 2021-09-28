package chapter05

import chapter05.Stream.{cons, empty}

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] =
    foldRight[Option[A]](None)((elem, _) => Some(elem))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def toList: List[A] = {
    @tailrec
    def loop(stream: Stream[A], acc: List[A]): List[A] = {
      stream match {
        case Empty            => acc
        case Cons(head, tail) => loop(tail(), acc :+ head())
      }
    }

    loop(this, List.empty[A])
  }

  def take(n: Int): Stream[A] =
    Stream.unfold(this, n) {
      case (Cons(head, _), 1)             => Some(head(), (empty, n))
      case (Cons(head, tail), n) if n > 1 => Some(head(), (tail(), n - 1))
      case (_, _)                         => None
    }

  @tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, tail) if n > 0 => tail().drop(n - 1)
      case _                      => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Empty            => None
      case Cons(head, tail) => if (p(head())) Some(head(), tail()) else None
    }

  @tailrec
  final def exist(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) || t().exist(p)
      case _          => false
    }

  @tailrec
  final def forAll(p: A => Boolean): Boolean =
    this match {
      case Cons(head, tail) => p(head()) && tail().forAll(p)
      case _                => true
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((elem, init) => if (p(elem)) cons(elem, init) else init)

  def map[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Empty            => None
      case Cons(head, tail) => Some(f(head()), tail())
    }

  def append[B >: A](b: B): Stream[B] =
    foldRight(Stream(b))((elem, init) => cons(elem, init))

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((head, init) => cons(head, init))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((head, init) => f(head).append(init))

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold(this, that) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _                            => None
    }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold(this, that) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), _)            => Some((Some(h1()), None), (t1(), empty))
      case (_, Cons(h2, t2))            => Some((None, Some(h2())), (empty, t2()))
      case _                            => None
    }

  def hasSubsequence[B >: A](sub: Stream[B]): Boolean =
    tails.exist(s => s.startsWith(sub))

  def startsWith[B >: A](that: Stream[B]): Boolean =
    (this, that) match {
      case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1() startsWith t2()
      case (_, Cons(_, _))                              => false
      case (_, _)                                       => true
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty            => None
      case Cons(head, tail) => Some(cons(head(), tail()), tail())
    }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    this match {
      case Empty            => cons(z, empty)
      case Cons(head, tail) => cons(f(head(), tail().foldRight(z)(f)), tail().scanRight(z)(f))
    }
}

case object Empty extends Stream[Nothing]

/*
  A nonempty stream consists of a head and a tail,
  which are both non-strict. Due to technical
  limitations, these are thunks that must be
  explicitly forced, rather than by-name parameters.
 */
final case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  /*
    A smart c-tor for creating a nonempty stream.
   */
  def cons[A](head: A, tail: => Stream[A]): Stream[A] =
    Cons(() => head, () => tail)

  /*
    A smart c-tor for creating an empty stream
    of particular type.
   */
  def empty[A]: Stream[A] = Empty

  /*
    A convenient variable-argument method for constructing
    a Stream from multiple elements.
   */
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def append[A](s1: => Stream[A], s2: => Stream[A]): Stream[A] =
    s1 match {
      case Empty      => s2
      case Cons(h, t) => Cons(h, () => append(t(), s2))
    }

  def constant[A](a: A): Stream[A] =
    unfold(empty[A])(s => Some(a, s))

  def from(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s + 1))

  def fibs(): Stream[Int] =
    unfold((0, 1)) { case (prev, current) => Some(prev, (current, current + prev)) }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None         => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
}
