package chapter08

import chapter06.RNG
import chapter08.Prop.{Result, TestCases}

case class Prop(run: (TestCases, RNG) => Result) {
  def check(): Result = ???
  def &&(p: Prop): Prop = ???
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override def isFalsified: Boolean = true
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = false
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop =
    Prop { (n, rng) =>
      randomStream(gen)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map {
          case (a, i) =>
            try {
              if (f(a)) Passed else Falsified(a.toString, i)
            } catch {
              case e: Exception => Falsified(buildMessage(a, e), n)
            }
        }
        .find(_.isFalsified)
        .getOrElse(Passed)
    }

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMessage[A](s: A, e: Exception): String =
    s"""
       |Test case: $s
       |generated an exception: ${e.getMessage}
       |stack trace: ${e.getStackTrace.mkString("\n")}
       |""".stripMargin
}
