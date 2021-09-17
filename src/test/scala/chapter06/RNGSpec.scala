package chapter06

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RNGSpec extends AnyWordSpec with Matchers {
  "A Simple RNG" when {
    "nextInt with the same seed should return same value" in {
      val rng1 = RNG.SimpleRNG(42)
      val rng2 = RNG.SimpleRNG(42)

      val (n1, nextRNG1) = rng1.nextInt
      val (n2, nextRNG2) = rng2.nextInt

      n1 shouldBe n2
      nextRNG1 shouldBe nextRNG2
    }

    "nextInt with different seed return different values" in {
      val rng1 = RNG.SimpleRNG(42)
      val rng2 = RNG.SimpleRNG(24)

      val (n1, nextRNG1) = rng1.nextInt
      val (n2, nextRNG2) = rng2.nextInt

      n1 should not be n2
      nextRNG1 should not be nextRNG2
    }

    "nonNegativeInt should not return negative values" in {
      // TODO it possible to replace with Int.MaxValue?
      for (n <- 0 to 1000000) {
        val rng = RNG.SimpleRNG(n)
        val (number, _) = RNG.nonNegativeInt(rng)
        number should be >= 0
      }
    }

    "nonNegativeInt should return positive value for Int.MinValue seed" in {
      val rng = RNG.SimpleRNG(Int.MinValue)
      val (number, _) = RNG.nonNegativeInt(rng)
      number should be >= 0
    }

    "double method should return a double between 0 and 1, not including 1" in {
      for (n <- 0 to 1000000) {
        val rng = RNG.SimpleRNG(n)
        val (number, _) = RNG.double(rng)
        number should (be < 1.0 and be >= 0.0)
      }
    }

    "double method should return a double between 0 and 1, not including 1 for Int.MinValue seed" in {
      val rng = RNG.SimpleRNG(Int.MinValue)
      val (number, _) = RNG.double(rng)
      number should be >= 0.0
    }

    "intDouble should return pair of int and non-negative double between 0 and 1" in {
      for (n <- 1 to 1000000) {
        val rng = RNG.SimpleRNG(n)
        val ((intResult, doubleResult), _) = RNG.intDouble(rng)

        intResult should be <= Int.MaxValue
        doubleResult should (be < 1.0 and be >= 0.0)
      }
    }

    "doubleInt should return same values as intDouble with same seed" in {
      val rng = RNG.SimpleRNG(77)
      val ((doubleResult1, intResult1), _) = RNG.doubleInt(rng)
      val ((intResult2, doubleResult2), _) = RNG.intDouble(rng)

      doubleResult1 shouldBe doubleResult2
      intResult1 shouldBe intResult2
    }

    "double3 should return 3 double value and new RNG" in {
      val rng = RNG.SimpleRNG(123)
      val ((d1, d2, d3), _) = RNG.double3(rng)

      d1 should (be >= 0.0 and be < 1.0)
      d2 should (be >= 0.0 and be < 1.0)
      d3 should (be >= 0.0 and be < 1.0)
    }

    "call ints should return List of random integer" in {
      val initRng = SimpleRNG(1)
      val (list, _) = initRng.ints(7)(initRng)
      assert(list.size == 7)
      assert(!list.contains(0))
    }

    "call sequence should return Rand[List[A]]" in {
      val rng = SimpleRNG(1)
      assert(rng.sequence(List(rng.unit(1), rng.unit(2), rng.unit(3)))(rng)._1 == List(1, 2, 3))
    }

    "call nonNegativeLessThan should return Rand[Int] with correct boundary" in {
      val rng = SimpleRNG(5)
      assert(rng.nonNegativeLessThan(6)(rng)._1 == 0)
    }
  }
}
