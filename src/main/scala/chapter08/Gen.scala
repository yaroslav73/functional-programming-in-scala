package chapter08

import chapter06.{RNG, State}

final case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def optA: Gen[Option[A]] = Gen(this.sample.map(a => Option(a)))
}

object Gen {
  // type Rand[+A] = RNG => (A, RNG)
  // nonNegativeInt accept RNG and return (Int, RNG), so it's == RNG => (Int, RNG)
  // State[S, +A] == S => (A, S)
  // State[RNG, Int] == RNG => (Int, RNG)
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map { n => start + n % (stopExclusive - start) })

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(gen.sample)))

  def pairInt(star: Int, stopExclusive: Int): Gen[(Int, Int)] =
    Gen(choose(star, stopExclusive).sample.flatMap(i => choose(star, stopExclusive).sample.map(y => (i, y))))
}
