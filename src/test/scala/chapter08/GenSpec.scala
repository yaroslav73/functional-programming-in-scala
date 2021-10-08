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

      (8 until 10) should contain(n)
    }

    "generate random pair Int between 'start' and 'stopExclusive' with choose" in {
      val rng = RNG.SimpleRNG(5)

      val gen = Gen.pairInt(8, 10)
      val (n, _) = gen.sample.run(rng)

      n shouldBe (8, 8)
    }

    "evaluate Gen from any value with unit" in {
      val rng = RNG.SimpleRNG(5)

      val gen = Gen.unit("hello")
      val (s, _) = gen.sample.run(rng)

      s shouldBe "hello"
    }

    "generator for boolean" in {
      val rng1 = RNG.SimpleRNG(5)
      val rng2 = RNG.SimpleRNG(6)

      val gen = Gen.boolean
      val (res1, _) = gen.sample.run(rng1)
      val (res2, _) = gen.sample.run(rng2)

      res1 shouldBe true
      res2 shouldBe false
    }

    "generator list of size n with A values" in {
      val rng = RNG.SimpleRNG(5)

      val gen = Gen.listOfN(5, Gen.boolean)
      val (list, _) = gen.sample.run(rng)

      println(list)

      list should have size 5
      list shouldBe List(true, true, true, true, false)
    }

    "wrap generate value to Option with optA" in {
      val rng = RNG.SimpleRNG(5)

      val gen = Gen.pairInt(8, 10).optA
      val (n, _) = gen.sample.run(rng)

      n shouldBe Option((8, 8))
    }
  }
}
