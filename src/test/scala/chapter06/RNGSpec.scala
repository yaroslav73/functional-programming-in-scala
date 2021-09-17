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
        number should be >= 0.0
        number should be < 1.0
      }
    }

    "double method should return a double between 0 and 1, not including 1 for Int.MinValue seed" in {
      val rng = RNG.SimpleRNG(Int.MinValue)
      val (number, _) = RNG.double(rng)
      number should be >= 0.0
    }

    "call intDouble should return pair of int that should non-negative and double between 0 and 1" in {
      for (n <- 1 to 1000000) {
        val rng = SimpleRNG(n)
        val ((intVal, doubleVal), _) = rng.intDouble(rng)
        assert(intVal >= 0)
        assert(doubleVal < 1)
        assert(doubleVal >= 0)
      }
    }

    "doubleInt should return same values as intDouble with same seed" in {
      val rng = SimpleRNG(77)
      val ((dVal1, iVal1), _) = rng.doubleInt(rng)
      val ((iVal2, dVal2), _) = rng.intDouble(rng)

      assert(dVal1 == dVal2)
      assert(iVal1 == iVal2)
    }

    "double3 should return 3 double value and new RNG" in {
      val rng = SimpleRNG(123)
      val ((d1, d2, d3), _) = rng.double3(rng)

      assert(d1 >= 0 && d1 < 1)
      assert(d2 >= 0 && d2 < 1)
      assert(d3 >= 0 && d3 < 1)
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
