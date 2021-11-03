package chapter13

import scala.io.StdIn

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

  def PrintLine(msg: String): IO[Unit] = new IO[Unit] { def run: Unit = println(msg) }
  def ReadLine: IO[String] = new IO[String] { def run: String = StdIn.readLine() }

  def contest(p1: Player, p2: Player): IO[Unit] =
    PrintLine(winnerMsg(winner(p1, p2)))

  val p1 = Player("Ann", 73)
  val p2 = Player("Den", 72)
  contest(p1, p2).run

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def convert: IO[Unit] =
    for {
      _ <- PrintLine("Enter the temperature in degrees Fahrenheit: ")
      t <- ReadLine.map(_.toDouble)
      _ <- PrintLine(fahrenheitToCelsius(t).toString)
    } yield ()

  convert.run

  val helpString = """
  | The Amazing Factorial REPL, v2.0
  | q - quit
  | <number> - compute the factorial of the given number
  | <anything else> - bomb with horrible error
  """.trim.stripMargin

  def factorial(n: Int): IO[Int] =
    for {
      acc <- IO.ref(1)
      _ <- IO.foreachM[Int]((1 to n).to(LazyList))(i => acc.modify(_ * i).skip)
      result <- acc.get
    } yield result

  def factorialREPL: IO[Unit] =
    IO.sequence_(
      PrintLine(helpString),
      IO.doWhile { ReadLine } { line =>
        IO.when(!line.equalsIgnoreCase("q")) {
          for {
            n <- factorial(line.toInt)
            _ <- PrintLine(s"Factorial: $n")
          } yield ()
        }
      }
    )

  factorialREPL.run
}
