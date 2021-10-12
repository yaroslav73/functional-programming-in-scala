package chapter08

import chapter06.RNG
import chapter08.Prop.{MaxSize, Result, TestCases}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def check(p: => Boolean): Prop = {
    lazy val result = p
    Prop.forAll(Gen.unit(()))(_ => result)
  }

  def &&(p: Prop): Prop =
    Prop { (max, n, rng) =>
      run(max, n, rng) match {
        case Prop.Passed => p.run(max, n, rng)
        case failure     => failure
      }
    }

  def ||(p: Prop): Prop =
    Prop { (max, n, rng) =>
      run(max, n, rng) match {
        case Prop.Falsified(failure, _) => p.tag(failure).run(max, n, rng)
        case passed                     => passed
      }
    }

  private def tag(msg: String): Prop =
    Prop { (max, n, rng) =>
      run(max, n, rng) match {
        case Prop.Falsified(failure, successes) => Prop.Falsified(msg + "\n" + failure, successes)
        case passed                             => passed
      }
    }
}

object Prop {
  type MaxSize = Int
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

  def run(p: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = RNG.SimpleRNG(System.currentTimeMillis())): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Passed                        => println(s"+ OK, passed $testCases tests.")
      case Falsified(failure, successes) => println(s"!!! Falsified after $successes passed tests:\n$failure")
    }
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop =
    Prop { (_, n, rng) =>
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

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
    Prop { (max, n, rng) =>
      val casesPerSize: Int = (n + (max - 1)) / max
      val props: LazyList[Prop] = LazyList.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props
        .map(p =>
          Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
          }
        )
        .toList
        .reduce(_ && _)

      prop.run(max, n, rng)
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
