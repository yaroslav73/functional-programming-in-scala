package chapter06

import org.scalatest.WordSpec

class RNGSpec extends WordSpec {
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
      val rng = SimpleRNG(0)
      for (n <- 1 to 1000000) assert(rng.nonNegativeInt(SimpleRNG(n))._1 >= 0)
    }

    "call double should return a double between 0 and 1, not including 1" in {
      val initRng = SimpleRNG(0)
      for (n <- 1 to 1000000) {
        val doubleRng = initRng.double(SimpleRNG(n))
        assert(doubleRng._1 >= 0 && doubleRng._1 < 1)
      }
    }

    "call intDouble should return pair of int that should non-negative and double between 0 and 1" in {
      val initRng = SimpleRNG(0)
      for (n <- 1 to 1000000) {
        val ((intVal, doubleVal), rng) =initRng.intDouble(SimpleRNG(n))
        assert(intVal >= 0)
        assert(doubleVal < 1)
        assert(doubleVal >= 0)
      }
    }

    "call ints should return List of random integer" in {
      val initRng = SimpleRNG(1)
      val (list, _) = initRng.ints(7)(initRng)
      assert(list.size == 7)
      assert(list.count(_ != list.head) == 6)
    }
  }
}
