package chapter13

object EffectExample extends App {
  final case class Player(name: String, score: Int)

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p2.score > p1.score) Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String =
    p match {
      case Some(Player(name, _)) => s"$name is the winner!"
      case None                  => "It's a draw!"
    }

  def contest(p1: Player, p2: Player): Unit =
    println(winnerMsg(winner(p1, p2)))
}
