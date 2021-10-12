package chapter08

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class PropSpec extends AnyWordSpecLike with Matchers {
  "Prop" should {
    "check && condition forAll cases" in {
      val intList = Gen.listOfN(10, Gen.choose(0, 100))
      val prop = Prop.forAll(intList)(ns => ns.reverse.reverse == ns) &&
        Prop.forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)

      Prop.run(prop.check(true))
    }

    "check non empty list" in {
      val smallInt = Gen.choose(-10, 10)
      val maxProp = Prop.forAll(Gen.listOf(smallInt)) { ns => // Gen.listOf not working, replace with Gen.listOfOne
        println(ns)
        val max = ns.max
        !ns.exists(_ > max)
      }

      Prop.run(maxProp)
    }

    "check high-order function" in {
      val int = Gen.choose(1, 100)
      val isEven = (i: Int) => i % 2 == 0
      val takeWhileProp = Prop.forAll(Gen.listOf(int))(ns => ns.takeWhile(isEven).forall(isEven))

      Prop.run(takeWhileProp)
    }
  }
}
