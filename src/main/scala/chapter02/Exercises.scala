package chapter02

import scala.annotation.tailrec

object Exercises {
  def findFirst(xs: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int = if (xs(n) == key) n else loop(n)

    loop(0)
  }

  def findFirst[A](xs: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = if (p(xs(n))) n else loop(n)

    loop(0)
  }

  def isSorted[A](xs: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(current: A, xs: Array[A], result: Boolean): Boolean = {
      if (!result) result
      else if (xs.isEmpty) result
      else loop(xs.head, xs.tail, result && ordered(current, xs.head))
    }

    if (xs.isEmpty || xs.length == 1) true
    else loop(xs.head, xs.tail, result = true)
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

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  // Braces can be removed, because => associates to thee right
  // A => (B => C) is the same as A => B => C
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => f(a, _)

  // Answer:
  def curryAnswer[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  // We can omit type for function argument a
  // it will be inferred by compiler from context
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  // Compose vs andThen
  // f compose g -- firstly evaluation g, then result of eval g passed to f
  // f andThen g -- firstly evaluation f, then result of eval f passed to g
  // f compose g == g andThen f
}
