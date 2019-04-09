

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

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new NotImplementedError("method tail not implemented for empty list Nil")
    case Cons(_, t) => t
  }

  def setHead[A](xs: List[A], x: A): List[A] = xs match {
    case Nil => Cons(x, Nil)
    case Cons(_, t) => Cons(x, t)
  }

  def drop[A](xs: List[A], n: Int): List[A] = {
    if (n != 0) drop(tail(xs), n - 1)
    else xs
  }

  def dropWhile[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
    case Cons(h, t) if p(h) => dropWhile(t)(p)
    case _ => xs
  }


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

  def foldRight[A, B](xs: List[A], init: B)(f: (A, B) => B): B = {
    println(s"get: $xs")
    xs match {
      case Nil =>
        println(s"last iteration for init: $init")
        init
      case Cons(h, t) => {
//        foldRight(t, f(h, init))(f)
        println(s"iteration for $h, tail: $t")
        f(h, foldRight(t, init)(f))
      }
    }
  }

  def foldLeft[A, B](xs: List[A], init: B)(f: (B, A) => B): B = {
    println(s"get list: $xs")
    xs match {
      case Nil =>
        println(s"return init: $init")
        init
      case Cons(h, t) =>
        println(s"fold for $h, tail: $t")
        foldLeft(t, f(init, h))(f)
    }
  }

  def length[A](xs: List[A]): Int = foldRight(xs, 0)((_, b: Int) => b + 1)

  def productFold(nums: List[Double]): Double =
    foldRight(nums, 1.0)(_ * _)

  def productLeftFold(nums: List[Double]): Double =
    foldLeft(nums, 1.0)(_ * _)

  def sumLeftFold(nums: List[Int]): Int =
    foldLeft(nums, 0)(_ + _)
}

val l1 = List(1, 2, 3, 4, 5, 6, 7)
val l2 = List(0.0, 1.0, 2.0, 3.0)

//List.tail(l1)
//List.setHead(l1, 7)
//List.drop(l1, 3)
//List.dropWhile(l1)(x => x < 5)
//List.dropWhile(l1)(_ < 5)
//List.init(l1)
//List.foldRight(l1, 0)(_ + _)
//List.productFold(l2)
//List.foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))
//List.length(l1)
//List.length(l2)
//List.productLeftFOld(l2)
List.sumLeftFold(l1)

List.foldRight(l1, 0)((a: Int, b: Int) => a - b)
List.foldLeft(l1, 0)((a: Int, b: Int) => a - b)

