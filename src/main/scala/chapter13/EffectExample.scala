package chapter13

object EffectExample extends App {
  final case class Player(name: String, score: Int)

  def contest(p1: Player, p2: Player): Unit = {
    if (p1.score > p2.score) println(s"${p1.name} is the winner!")
    else if (p2.score > p1.score) println(s"${p2.name} is the winner!")
    else println("It's a draw!")
  }
}
