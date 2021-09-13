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
    this match {
      case Empty                     => empty
      case Cons(head, tail) if n > 1 => cons(head(), tail().take(n - 1))
      case Cons(head, _) if n == 1   => cons(head(), empty)
    }

  def takeUnfold(count: Int): Stream[A] =
    Stream.unfold(this, count) { pair =>
      if (pair._2 >= 1 && pair._1.headOption.isDefined) Some(pair._1.headOption.get, (pair._1.drop(1), pair._2 - 1))
      else None
    }

  @tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, tail) if n > 0 => tail().drop(n - 1)
      case _                      => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((elem, init) => if (p(elem)) cons(elem, init) else empty)

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
    foldRight(empty[B])((elem, init) => cons(f(elem), init))

  def append[B >: A](b: B): Stream[B] =
    foldRight(Stream(b))((elem, init) => cons(elem, init))

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((head, init) => cons(head, init))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((head, init) => f(head).append(init))

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold(this, s) { streams =>
      if (streams._1.headOption.isDefined && streams._2.headOption.isDefined) {
        Some(f(streams._1.headOption.get, streams._2.headOption.get), (streams._1.drop(1), streams._2.drop(1)))
      } else None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold(this, s) { streams =>
      Some((streams._1.headOption, streams._2.headOption), (streams._1.drop(1), streams._2.drop(1)))
    }

  def hasSubsequence[A](sub: Stream[A]): Boolean =
    sub match {
      case Empty                                   => true
      case Cons(h, t) if this.headOption.isDefined => h() == this.headOption.get && t().hasSubsequence(this.drop(1))
      case _                                       => false
    }

  def startWith[A](s: Stream[A]): Boolean = this hasSubsequence s

  def tails: Stream[Stream[A]] = Stream.unfold(this)(s => if (s.headOption.isDefined) Option(s, s.drop(1)) else None)

  def scanRight[B](init: B)(f: (A, => B) => B): Stream[B] =
    this match {
      case Cons(_, t) => Cons(() => foldRight(init)(f), () => t().scanRight(init)(f))
      case _          => Cons(() => init, () => Empty)
    }

  def scanRightAnswer[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Cons(() => b2, () => p1._2))
    })._2
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
    cons(a, constant(a))

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def fibs(): Stream[Int] = {
    def loop(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, loop(n2 + n1, n1))
    }

    loop(0, 1)
  }

  // I don't understand what should doing unfold function
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((elem, s)) => cons(elem, unfold(s)(f))
      case None            => empty[A]
    }

  def unfoldOnes(): Stream[Int] = unfold(1)((n: Int) => Option(n, 1))

  def unfoldFrom(n: Int): Stream[Int] = unfold(n)((x: Int) => Option(x, x + 1))

  def unfoldConstant[A](a: A): Stream[A] = unfold(a)((a: A) => Option(a, a))

  def unfoldFibs(): Stream[Int] = unfold((0, 1))((n: (Int, Int)) => Option(n._1, (n._2 + n._1, n._1)))
}
