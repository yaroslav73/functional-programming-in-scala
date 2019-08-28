package chapter06

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, newRng) = nextInt
    if (n < 0) (n >>> 1, newRng) else (n, newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, newRng) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), newRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intValue, firstR) = nonNegativeInt(rng)
    val (doubleValue, secondR) = double(firstR)
    ((intValue, doubleValue), secondR)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((intValue, doubleValue), r) = intDouble(rng)
    ((doubleValue, intValue), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (firstDouble, r1) = double(rng)
    val (secondDouble, r2) = double(r1)
    val (thirdDouble, r3) = double(r2)
    ((firstDouble, secondDouble, thirdDouble), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (nextRandom, nextRNG) = rng.nextInt
    if (count == 1) (List(nextRandom), nextRNG)
    else (List(nextRandom) ::: ints(count - 1)(nextRNG)._1, nextRNG)
  }
}
