package chapter06

case class SimpleRNG(seed: Long) extends RNG {

  type Rand[+A] = RNG => (A, RNG)

  override def nextInt: (Int, RNG) = {
    val newSeed: Long = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG: RNG = SimpleRNG(newSeed)
    val n: Int = (newSeed >>> 16).toInt
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
    val (intValue, rng1) = nonNegativeInt(rng)
    val (doubleValue, rng2) = double(rng1)
    ((intValue, doubleValue), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((intValue, doubleValue), r) = intDouble(rng)
    ((doubleValue, intValue), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (first, r1) = double(rng)
    val (second, r2) = double(r1)
    val (third, r3) = double(r2)
    ((first, second, third), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int, rng: RNG, init: List[Int]): (List[Int], RNG) = {
      val (n, newRng) = rng.nextInt
      val res = n :: init
      if (count == 1) (res, newRng)
      else loop(count - 1, newRng, res)
    }

    loop(count, rng, Nil)
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(n => n - n % 2)

  def doubleMap: Rand[Double] = map(nonNegativeInt)(n => n / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs match {
      case Nil => (List.empty[A], rng)
      case head :: tail => (List(head(rng)._1), rng) //(List(head(rng)._1) ::: sequence(tail)._2)
    }
  }

  def s[A](fs: List[RNG => (A, RNG)]): (List[A], RNG) = ???
}
