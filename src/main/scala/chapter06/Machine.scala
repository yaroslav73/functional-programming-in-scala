package chapter06

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

  def simulateMachine(inputs: List[Input]): StateOld[Machine, (Int, Int)] = ???

  def main(args: Array[String]): Unit = {}
}
