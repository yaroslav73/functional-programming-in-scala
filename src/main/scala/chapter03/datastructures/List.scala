package chapter03.datastructures

import scala.annotation.tailrec

sealed trait List[+A] {
  def setHead[B >: A](x: B): List[B] = this match {
    case Nil => Cons(x, Nil)
    case Cons(_, tail) => Cons(x, tail)
  }

  def tail: List[A] = this match {
    case Nil => throw new UnsupportedOperationException("tail of empty list")
    case Cons(_, tail) => tail
  }

  def drop(n: Int): List[A] = this match {
    case Nil => Nil
    case Cons(_, tail) if n > 0 => tail.drop(n - 1)
    case _ => this
  }

  def dropWhile(p: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(head, tail) if p(head) => tail.dropWhile(p)
    case _ => this
  }

  def append[B >: A](that: List[B]): List[B] =
    foldRight(that)((elem, init) => Cons(elem, init))

  def init: List[A] = this match {
    case Nil => throw new UnsupportedOperationException("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, tail.init)
  }

  def foldRight[B](init: B)(f: (A, B) => B): B =
    foldLeft((b: B) => b)((init, elem) => b => init(f(elem, b)))(init)

  @tailrec
  final def foldLeft[B](init: B)(f: (B, A) => B): B = this match {
    case Nil => init
    case Cons(head, tail) => tail.foldLeft(f(init, head))(f)
  }

  def length: Int = foldLeft(0)((init, _) => init + 1)

  def reverse: List[A] = foldLeft(Nil: List[A])((init, elem) => Cons(elem, init))

  def map[B](f: A => B): List[B] = this match {
    case Nil => Nil
    case Cons(head, tail) => Cons(f(head), tail.map(f))
  }

  def filter(p: A => Boolean): List[A] =
    flatMap(e => if (p(e)) List(e) else Nil)

  def flatMap[B](f: A => List[B]): List[B] =
    foldLeft(List.empty[B])((init, elem) => init.append(f(elem)))
}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, override val tail: List[A]) extends List[A]

object List {
  def empty[A]: List[A] = Nil

  def apply[A](elems: A*): List[A] = {
    if (elems.isEmpty) Nil
    else Cons(elems.head, apply(elems.tail: _*))
  }

  def addOne(xs: List[Int]): List[Int] = {
    @tailrec
    def loop(xs: List[Int], acc: List[Int]): List[Int] = xs match {
      case Nil => acc
      case Cons(head, tail) => loop(tail, Cons(head + 1, acc))
    }

    loop(xs, Nil).reverse
  }

  def concat[A](xs: List[List[A]]): List[A] =
    xs.foldLeft(List.empty[A])((init, elem) => init.append(elem))

  def append[A](l1: List[A], l2: List[A]): List[A] = l2 match {
    case Nil => l1
    case Cons(h, t) => Cons(h, append(l1, t))
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
    def loop(xs: List[A], res: List[A]): List[A] = {
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

  def reverse[A](xs: List[A]): List[A] = {
    def loop(xs: List[A], init: List[A]): List[A] = {
      xs match {
        case Nil => init
        case Cons(h, t) => loop(t, Cons(h, init))
      }
    }

    loop(xs, Nil)
  }

  def map[A, B](xs: List[A])(f: A => B): List[B] = xs match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs match {
    case Nil => Nil: List[B]
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  def filter[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
    case Nil => xs
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(_, t) => filter(t)(f)
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = l1 match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h, head(l2)), zipWith(t, tail(l2))(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sub match {
    case Nil => true
    case Cons(h, t) if h == head(sup) => hasSubsequence(t, tail(sup))
    case Cons(_, t) => hasSubsequence(sup, t)
  }

  def filterFlatMap[A](xs: List[A])(f: A => Boolean): List[A] =
    flatMap(xs)(h => if (f(h)) Cons(h, Nil) else Nil)

  def reverseFold[A](xs: List[A]): List[A] = foldLeft(xs, Nil: List[A])((init: List[A], x: A) => Cons(x, init))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  //  def addOne(ints: List[Int]): List[Int] = ints match {
  //    case Nil => ints
  //    case Cons(h, t) => Cons(h + 1, addOne(t))
  //  }

  def addIntLists(l1: List[Int], l2: List[Int]): List[Int] = flatMap(l1)(a => map(l2)(b => a + b))

  def double2String(nums: List[Double]): List[String] = nums match {
    case Nil => Nil: List[String]
    case Cons(h, t) => Cons(h.toString, double2String(t))
  }

  def sumFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(nums: List[Double]): Double = nums match {
    case Nil => 1.0
    case Cons(h@0.0, _) => h
    case Cons(h, t) => h * product(t)
  }

  def productFoldRight(xs: List[Double]): Double = xs.foldRight(1.0) { (elem, init) =>
    if (elem == 0.0) 0.0 else elem * init
  }

  def productFoldLeft(xs: List[Double]): Double = xs.foldLeft(1.0) { (init, elem) =>
    if (elem == 0.0) 0.0 else elem * init
  }
}
