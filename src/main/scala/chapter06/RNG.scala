package chapter06

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
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

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] =
    rng => rng.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, ns) = s(rng)
      (f(a), ns)
    }

  def map2[A, B, C](ra: Rand[A])(rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra)(rb)((_, _))

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = {
    @tailrec
    def loop(rs: List[Rand[A]], acc: List[A], nextRNG: RNG): (List[A], RNG) = {
      rs match {
        case Nil => (acc, nextRNG)
        case head :: tail =>
          val (a, nr) = head(nextRNG)
          loop(tail, acc :+ a, nr)
      }
    }

    loop(rs, List.empty[A], _)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (n, nextRNG) if n == Int.MinValue => (Int.MaxValue, nextRNG)
      case (n, nextRNG) if n < 0             => (-n, nextRNG)
      case res @ (n, _) if n >= 0            => res
    }
  }

  def double: Rand[Double] =
    map(nonNegativeInt)(n => n / (Int.MaxValue.toDouble + 1))

  def intDouble: Rand[(Int, Double)] =
    both(int, double)

  def doubleInt: Rand[(Double, Int)] =
    both(double, int)

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
