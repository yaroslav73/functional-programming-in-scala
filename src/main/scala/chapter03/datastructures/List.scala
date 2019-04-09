package chapter03.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](elems: A*): List[A] = {
    if (elems.isEmpty) Nil
    else Cons(elems.head, apply(elems.tail: _*))
  }

  def head[A](xs: List[A]): A = xs match {
    case Nil => throw new NoSuchElementException("head of empty list")
    case Cons(h, _) => h
  }


  def setHead[A](xs: List[A], x: A): List[A] = xs match {
    case Nil => Cons(x, Nil)
    case Cons(_, t) => Cons(x, t)
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new UnsupportedOperationException("tail of empty list")
    case Cons(_, t) => t
  }

  // TODO: reverse List
  def init[A](xs: List[A]): List[A] = {
    def loop[A](xs: List[A], res: List[A]): List[A] = {
      xs match {
        case Nil => xs
        case Cons(h, t) =>
          if (t != Nil) loop(t, Cons(h, res))
          else res
      }
    }

    loop(xs, Nil)
  }

  def drop[A](xs: List[A], n: Int): List[A] = {
    if (n != 0) drop(tail(xs), n - 1)
    else xs
  }

  def dropWhile[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
    case Cons(h, t) if p(h) => dropWhile(t)(p)
    case _ => xs
  }

  def foldRight[A, B](xs: List[A], init: B)(f: (A, B) => B): B = xs match {
    case Nil => init
//    case Cons(h, t) => foldRight(t, f(h, init))(f)
    case Cons(h, t) => f(h, foldRight(t, init)(f))
  }

  // TODO which different between right/left fold???
  def foldLeft[A, B](xs: List[A], init: B)(f: (B, A) => B): B = xs match {
    case Nil => init
    case Cons(h, t) => foldLeft(t, f(init, h))(f)
  }

  def length[A](xs: List[A]): Int = foldRight(xs, 0)((_, a: Int) => a + 1)

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(nums: List[Double]): Double = nums match {
    case Nil => 1.0
    case Cons(h @ 0.0, _) => h
    case Cons(h, t) => h * product(t)
  }

  def productFold(nums: List[Double]): Double = foldRight(nums, 1.0)(_ * _)

  def productLeftFOld(nums: List[Double]): Double = foldLeft(nums, 1.0)(_ * _)
}
