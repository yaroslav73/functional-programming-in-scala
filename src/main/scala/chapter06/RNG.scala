package chapter06

import scala.annotation.tailrec

trait RNG {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] =
    rng => rng.nextInt

  def nextInt: (Int, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)
}

object RNG {
  final case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (n, nextRNG) if n == Int.MinValue => (Int.MaxValue, nextRNG)
      case (n, nextRNG) if n < 0             => (-n, nextRNG)
      case res @ (n, _) if n >= 0            => res
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRNG) = rng.nextInt
    val (d, resultRNG) = double(nextRNG)
    (i -> d, resultRNG)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), nextRNG) = intDouble(rng)
    (d -> i, nextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, nextRNG) = double(rng2)
    ((d1, d2, d3), nextRNG)
  }

  def ints(n: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(n: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      val (i, nextRNG) = rng.nextInt
      if (n == 0) (acc, nextRNG)
      else loop(n - 1, nextRNG, acc :+ i)
    }

    loop(n, rng, List.empty)
  }
}
