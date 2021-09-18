package chapter06

/**
 * The rules of the machine are as follows:
 * - Inserting a coin into a locked machine will cause it to unlock if there's any candy left.
 * - Turning the knob on an unlocked machine will cause it to dispense candy and become to locked.
 * - Turning the know on a locked machine does nothing.
 * - Inserting a coin an unlocking machine does nothing.
 * - A machine that's out candy ignores all inputs.
 */

sealed trait Input
case object Coin extends Input
case object Turn extends Input

final case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def changeState(input: Input): StateOld[Machine, (Int, Int)] = StateOld { machine =>
    input match {
      case Coin => machine match {
        case Machine(_, 0, _) => ((machine.candies, machine.coins), machine)
        case Machine(false, _, _) => ((machine.candies, machine.coins), machine)
        case Machine(true, _, coin) => ((machine.candies, coin + 1), Machine(locked = false, machine.candies, coin + 1))
      }
      case Turn => machine match {
        case Machine(_, 0, _) => ((machine.candies, machine.coins), machine)
        case Machine(true, _, _) => ((machine.candies, machine.coins), machine)
        case Machine(false, candy, _) => ((candy - 1, machine.coins), Machine(locked = true, candy - 1, machine.coins))
      }
    }
  }

  def simulateMachine(inputs: List[Input]): StateOld[Machine, (Int, Int)] = {
    def loop(inputs: List[Input], initialState: StateOld[Machine, (Int, Int)], machine: Machine): StateOld[Machine, (Int, Int)] = {
      if (inputs.isEmpty) initialState
      else {
        val (state, m) = changeState(inputs.head).run(machine)
        loop(inputs.tail, StateOld { _ => (state, m) }, m)
      }
    }

    loop(inputs, StateOld { _ => ((this.candies, this.coins), this) }, this)
  }
}

object Run extends App {
  val machine = Machine(true, 5, 10)
  //    println(machine.changeState(Coin).run(machine))
  //    println(machine.changeState(Coin).run(machine)._2.changeState(Turn).run(machine))
  //    println(machine.changeState(Coin).run(machine)._2.changeState(Turn).run(machine)._2.changeState(Coin).run(machine))
  val machineState = machine.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
  val updateMachine = machineState.run(machine)
  println(updateMachine)
}
