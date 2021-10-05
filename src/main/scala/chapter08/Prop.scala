package chapter08

import chapter08.Prop.{FailedCase, SuccessCount}

trait Prop {
  def check(): Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop = ???
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}
