package chapter06

import chapter06.Machine.{Coin, Turn}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MachineSpec extends AnyWordSpec with Matchers {
  "A Candy Dispenser Machine" should {
    "simulateMachine should process list of Inputs in right way #1" in {
      val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
      val machine = Machine(locked = true, 10, 100)
      val expected = Machine(locked = true, 6, 104)

      Machine.simulateMachine(inputs).run(machine) shouldBe ((expected.candies, expected.coins), expected)
    }

    "simulateMachine should process list of Inputs in right way #2" in {
      val inputs = List(Coin, Turn, Turn, Turn, Coin, Coin, Coin, Turn, Coin)
      val machine = Machine(locked = true, 10, 100)
      val expected = Machine(locked = false, 8, 103)

      Machine.simulateMachine(inputs).run(machine) shouldBe ((expected.candies, expected.coins), expected)
    }

    "simulateMachine should process list of Inputs in right way #3" in {
      val inputs = List(Coin, Turn, Turn, Turn, Coin, Coin, Coin, Turn, Coin, Turn, Coin, Turn)
      val machine = Machine(locked = true, 3, 0)
      val expected = Machine(locked = true, 0, 3)

      Machine.simulateMachine(inputs).run(machine) shouldBe ((expected.candies, expected.coins), expected)
    }
  }
}
