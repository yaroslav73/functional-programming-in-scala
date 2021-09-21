package chapter06

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CandyDispenserSpec extends AnyWordSpec with Matchers {
  "A Candy Dispenser" should {
    "doesn't change state when machine that's out candy" in {
      val machine = MachineOld(locked = true, 0, 100)

      machine.changeState(Coin).run(machine) mustBe ((machine.candies, machine.coins), machine)
      machine.changeState(Turn).run(machine) mustBe ((machine.candies, machine.coins), machine)
    }

    "inserting a coin an unlocking machine does nothing" in {
      val machine = MachineOld(locked = false, 10, 100)

      machine.changeState(Coin).run(machine) mustBe ((machine.candies, machine.coins), machine)
    }

    "turning the know on a locked machine does nothing" in {
      val machine = MachineOld(locked = true, 10, 100)

      machine.changeState(Turn).run(machine) mustBe ((machine.candies, machine.coins), machine)
    }

    "inserting a coin into a locked machine will cause it to unlock if there's any candy left" in {
      val machine = MachineOld(locked = true, 10, 100)
      val expected = MachineOld(locked = false, 10, 101)

      machine.changeState(Coin).run(machine) mustBe ((expected.candies, expected.coins), expected)
    }

    "turning the knob on an unlocked machine will cause it to dispense candy and become to locked" in {
      val machine = MachineOld(locked = false, 10, 100)
      val expected = MachineOld(locked = true, 9, 100)

      machine.changeState(Turn).run(machine) mustBe ((expected.candies, expected.coins), expected)
    }

    "simulateMachine should process list of Inputs in right way #1" in {
      val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
      val machine = MachineOld(locked = true, 10, 100)
      val expected = MachineOld(locked = true, 6, 104)

      machine.simulateMachine(inputs).run(machine) mustBe ((expected.candies, expected.coins), expected)
    }

    "simulateMachine should process list of Inputs in right way #2" in {
      val inputs = List(Coin, Turn, Turn, Turn, Coin, Coin, Coin, Turn, Coin)
      val machine = MachineOld(locked = true, 10, 100)
      val expected = MachineOld(locked = false, 8, 103)

      machine.simulateMachine(inputs).run(machine) mustBe ((expected.candies, expected.coins), expected)
    }

    "simulateMachine should process list of Inputs in right way #3" in {
      val inputs = List(Coin, Turn, Turn, Turn, Coin, Coin, Coin, Turn, Coin, Turn, Coin, Turn)
      val machine = MachineOld(locked = true, 3, 0)
      val expected = MachineOld(locked = true, 0, 3)

      machine.simulateMachine(inputs).run(machine) mustBe ((expected.candies, expected.coins), expected)
    }
  }
}
