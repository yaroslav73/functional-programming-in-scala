package chapter08

import chapter06.{RNG, State}

final case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(n: Int): Gen[List[A]] =
    Gen.listOfN(n, this)

  def listOfN(n: Gen[Int]): Gen[List[A]] =
    n.flatMap(i => this.listOfN(i))

  def union(g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(b => if (b) g1 else g2)

  def optA: Gen[Option[A]] = Gen(this.sample.map(a => Option(a)))

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](that: Gen[B]): Gen[(A, B)] = flatMap(a => that.map(b => (a, b)))
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

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

  def pairInt(star: Int, stopExclusive: Int): Gen[(Int, Int)] =
    Gen(choose(star, stopExclusive).sample.flatMap(i => choose(star, stopExclusive).sample.map(y => (i, y))))
}
