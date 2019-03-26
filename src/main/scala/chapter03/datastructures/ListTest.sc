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
}

val l1 = List(1, 2, 3, 4, 5, 6, 7)

List.tail(l1)
List.setHead(l1, 7)
List.drop(l1, 3)
List.dropWhile(l1)(x => x < 5)
List.dropWhile(l1)(_ < 5)
List.init(l1)

