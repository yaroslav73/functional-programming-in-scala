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
case object CoinOld extends Input
case object TurnOld extends Input

final case class MachineOld(locked: Boolean, candies: Int, coins: Int) {
  def changeState(input: Input): StateOld[MachineOld, (Int, Int)] = StateOld { machine =>
    input match {
      case CoinOld => machine match {
        case MachineOld(_, 0, _) => ((machine.candies, machine.coins), machine)
        case MachineOld(false, _, _) => ((machine.candies, machine.coins), machine)
        case MachineOld(true, _, coin) => ((machine.candies, coin + 1), MachineOld(locked = false, machine.candies, coin + 1))
      }
      case TurnOld => machine match {
        case MachineOld(_, 0, _) => ((machine.candies, machine.coins), machine)
        case MachineOld(true, _, _) => ((machine.candies, machine.coins), machine)
        case MachineOld(false, candy, _) => ((candy - 1, machine.coins), MachineOld(locked = true, candy - 1, machine.coins))
      }
    }
  }

  def simulateMachine(inputs: List[Input]): StateOld[MachineOld, (Int, Int)] = {
    def loop(inputs: List[Input], initialState: StateOld[MachineOld, (Int, Int)], machine: MachineOld): StateOld[MachineOld, (Int, Int)] = {
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
  val machine = MachineOld(true, 5, 10)
  //    println(machine.changeState(Coin).run(machine))
  //    println(machine.changeState(Coin).run(machine)._2.changeState(Turn).run(machine))
  //    println(machine.changeState(Coin).run(machine)._2.changeState(Turn).run(machine)._2.changeState(Coin).run(machine))
  val machineState = machine.simulateMachine(List(CoinOld, TurnOld, CoinOld, TurnOld, CoinOld, TurnOld, CoinOld, TurnOld))
  val updateMachine = machineState.run(machine)
  println(updateMachine)
}
