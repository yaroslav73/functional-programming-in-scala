package chapter06

import scala.annotation.tailrec

/**
  * The rules of the machine are as follows:
  * - Inserting a coin into a locked machine will cause it to unlock if there's any candy left.
  * - Turning the knob on an unlocked machine will cause it to dispense candy and become to locked.
  * - Turning the know on a locked machine does nothing.
  * - Inserting a coin an unlocking machine does nothing.
  * - A machine that's out candy ignores all inputs.
  */

final case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  // State[S, +A] == S => (A, S)
  // State[Machine, (Int, Int)] == Machine => ((Int, Int), Machine)
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State { machine =>
      @tailrec
      def loop(inputs: List[Input], state: State[Machine, (Int, Int)], m: Machine): ((Int, Int), Machine) = {
        inputs match {
          case Nil => state.run(m)
          case head :: tail =>
            val ((candies, coins), machine) = state.run(m)
            val updatedMachine = head match {
              case Coin if machine.locked && machine.candies > 0 => machine.copy(locked = false, coins = coins + 1)
              case Turn if !machine.locked                       => machine.copy(locked = true, candies = candies - 1)
              case _                                             => machine
            }
            loop(tail, State(updatedMachine => (updatedMachine.candies -> updatedMachine.coins, updatedMachine)), updatedMachine)
        }
      }

      loop(inputs, State(machine => (machine.candies -> machine.coins, machine)), machine)
    }

  def main(args: Array[String]): Unit = {
    val machine = Machine(locked = true, candies = 5, coins = 10)
    val state = Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
    println(state.run(machine))
  }
}
