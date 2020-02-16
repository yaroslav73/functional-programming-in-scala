package chapter06

import org.scalatest.wordspec.AnyWordSpec

class RNGSpec extends AnyWordSpec {
  "A Simple RNG" when {
    "call nextInt with the same seed should return same value" in {
      val rng1 = SimpleRNG(42)
      val rng2 = SimpleRNG(42)

      val (n1, _) = rng1.nextInt
      val (n2, _) = rng2.nextInt

      assert(n1 == n2)
    }

    "call nextInt with different seed return different values" in {
      val rng1 = SimpleRNG(42)
      val rng2 = SimpleRNG(24)

      val (n1, newRng1) = rng1.nextInt
      val (n2, newRng2) = rng2.nextInt

      assert(n1 != n2)
      assert(newRng1 != newRng2)
    }

    "call nonNegativeInt should not return negative values" in {
      for (n <- 1 to 1000000) {
        val rng = SimpleRNG(n)
        assert(rng.nonNegativeInt(rng)._1 >= 0)
      }
    }

    "call nonNegativeInt should return positive value for Int.MinValue seed" in {
      val rng = SimpleRNG(Int.MinValue)
      assert(rng.nonNegativeInt(rng)._1 >= 0)
    }

    "call double method should return a double between 0 and 1, not including 1" in {
      for (n <- 0 to 1000000) {
        val rng = SimpleRNG(n)
        val doubleRNGValue = rng.double(rng)._1
        assert(doubleRNGValue >= 0 && doubleRNGValue < 1)
      }
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
      assert(rng.sequence(List(rng.int, rng.int, rng.int))(rng)._1 == List(384748, -1151252339, -549383847))
    }
  }
}
