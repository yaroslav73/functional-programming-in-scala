package chapter02

import chapter02.Fibonacci.fib
import org.scalatest.wordspec.AnyWordSpec

class FibonacciSpec extends AnyWordSpec {
  "a fib" when {
    "called with 5" should {
      "return 3" in {
        assertResult(3)(fib(5))
      }
    }

    "called with 11" should {
      "return 55" in {
        assertResult(55)(fib(11))
      }
    }

    "called with 23" should {
      "return 17711" in {
        assertResult(17711)(fib(23))
      }
    }

    "called with 38" should {
      "return 24157817" in {
        assertResult(24157817)(fib(38))
      }
    }
  }
}
