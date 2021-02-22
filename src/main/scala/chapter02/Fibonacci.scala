package chapter02

import scala.annotation.tailrec

object Fibonacci {
  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, current: Int, previous: Int): Int = {
      if (n == 0 || n == 1) current
      else if (n == 2) current
      else loop(n - 1, current = current + previous, previous = current)
    }

    loop(n, 1, 0)
  }
}
