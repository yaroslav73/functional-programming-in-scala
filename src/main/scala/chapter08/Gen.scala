package chapter08

import chapter06.{RNG, State}

final case class Gen[A](sample: State[RNG, A])

object Gen {
  // type Rand[+A] = RNG => (A, RNG)
  // nonNegativeInt accept RNG and return (Int, RNG), so it's == RNG => (Int, RNG)
  // State[S, +A] == S => (A, S)
  // State[RNG, Int] == RNG => (Int, RNG)
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map { n => start + n % (stopExclusive - start) })
}
