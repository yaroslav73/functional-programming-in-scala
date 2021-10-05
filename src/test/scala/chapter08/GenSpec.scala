package chapter08

import chapter06.RNG
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class GenSpec extends AnyWordSpecLike with Matchers {
  "Gen" should {
    "generate random Int between 'start' and 'stopExclusive' with choose" in {
      val rng = RNG.SimpleRNG(5)

      val gen = Gen.choose(8, 10)
      val (n, _) = gen.sample.run(rng)

      (8 until 10) should contain (n)
    }
  }
}
