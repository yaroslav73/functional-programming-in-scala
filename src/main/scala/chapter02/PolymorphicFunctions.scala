package chapter02

import scala.annotation.tailrec

object PolymorphicFunctions {
  def findFirst(xs: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= xs.length) -1
      else if (xs(n) == key) n
      else loop(n)
    }

    loop(0)
  }

  def findFirst[A](xs: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= xs.length) -1
      else if (p(xs(n))) n
      else loop(n)
    }

    loop(0)
  }

  def isSorted[A](xs: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (xs.isEmpty || xs.length == 1) true
    else {
      @tailrec
      def loop(current: A, xs: Array[A], result: Boolean): Boolean = {
        if (!result) result
        else if (xs.isEmpty) result
        else loop(xs.head, xs.tail, result && ordered(current, xs.head))
      }

      loop(xs.head, xs.tail, result = true)
    }
  }

  def isSortedAnswer[A](xs: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= xs.length - 1) true
      else if (ordered(xs(n), xs(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }
}
