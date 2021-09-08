sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](elems: A*): List[A] = {
    if (elems.isEmpty) Nil
    else Cons(elems.head, apply(elems.tail: _*))
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
//        case Cons(h, t) => append(t, Cons(h, l2))
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
    def loop(xs: List[A], res: List[A]): List[A] = {
      xs match {
        case Nil => xs
        case Cons(h, t) =>
          if (t != Nil) Cons(h, loop(t, res)) //loop(t, Cons(h, res))
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

  def reverse[A](xs: List[A]): List[A] = {
    def loop(xs: List[A], res: List[A]): List[A] = {
      xs match {
        case Nil => res
        case Cons(h, t) => loop(t, Cons(h, res))
      }
    }

    loop(xs, Nil:List[A])
  }

  def map[A, B](xs: List[A])(f: A => B): List[B] = xs match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs match {
    case Nil => Nil:List[B]
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  def filter[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(_, t) => filter(t)(f)
  }

  def filterFlatMap[A](xs: List[A])(f: A => Boolean): List[A] =
    flatMap(xs)(h => if (f(h)) Cons(h, Nil) else Nil)

  def reverseFold[A](xs: List[A]): List[A] =
    foldLeft(xs, Nil:List[A])((init: List[A], x: A) => Cons(x, init))

  def productFold(nums: List[Double]): Double =
    foldRight(nums, 1.0)(_ * _)

  def productLeftFold(nums: List[Double]): Double =
    foldLeft(nums, 1.0)(_ * _)

  def sumLeftFold(nums: List[Int]): Int =
    foldLeft(nums, 0)(_ + _)

  def addOne(ints: List[Int]): List[Int] = ints match {
    case Nil => ints
    case Cons(h, t) => Cons(h + 1, addOne(t))
  }

  def double2String(nums: List[Double]): List[String] = nums match {
    case Nil => Nil:List[String]
    case Cons(h, t) => Cons(h.toString, double2String(t))
  }

  def addIntLists(l1: List[Int], l2: List[Int]): List[Int] = l1 match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + head(l2), addIntLists(t, tail(l2)))
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
}

val l1 = List(1, 2, 3, 4, 5, 6, 7)
val l2 = List(0.0, 1.0, 2.0, 3.0)

//List.append(List(1, 2, 3), List(7, 6, 5))
//
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
//List.sumLeftFold(l1)
//List.reverse(l1)
//List.reverseFold(l1)
//List.foldRight(l1, 0)((a: Int, b: Int) => { println(s"a: $a, b: $b"); a - b })
//List.foldLeft(l1, 0)((a: Int, b: Int) => { println(s"a: $a, b: $b"); a - b })

List.addOne(l1)
List.double2String(l2)
List.map(l1)(x => x * x)
List.filter(l1)(_ % 2 != 0)
List.flatMap(List(1, 2, 3))(i => List(i, i))
List.filterFlatMap(l1)(_ % 2 == 0)
List.addIntLists(List(1, 2, 3), List(4, 5, 6))
List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ * _)
List.hasSubsequence(List(1, 1, 3, 3, 4, 1, 2, 2), List(4))