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
  }
}
