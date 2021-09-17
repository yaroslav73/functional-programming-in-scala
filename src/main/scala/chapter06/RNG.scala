package chapter06

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
}
