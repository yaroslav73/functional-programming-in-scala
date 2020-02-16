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

  def double: Rand[Double] =
    map(nonNegativeInt)(n => n / (Int.MaxValue.toDouble + 1))

  def intDouble: Rand[(Int, Double)] = rng => {
    val (intValue, rng1) = nonNegativeInt(rng)
    val (doubleValue, rng2) = double(rng1)
    ((intValue, doubleValue), rng2)
  }

  def doubleInt: Rand[(Double, Int)] = rng => {
    val ((intValue, doubleValue), r) = intDouble(rng)
    ((doubleValue, intValue), r)
  }

  def double3: Rand[(Double, Double, Double)] = rng => {
    val (first, r1) = double(rng)
    val (second, r2) = double(r1)
    val (third, r3) = double(r2)
    ((first, second, third), r3)
  }

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(n => n - n % 2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // TODO you can improve it
  // You need to recursively iterate over the list.
  // Remember that you can use `foldLeft` or `foldRight`
  // instead of writing a recursive definition.
  // You can also reuse the `map2` function you just wrote.
  // As a test case for your implementation, we should expect
  // `sequence(List(unit(1), unit(2), unit(3)))(r)._1` to return `List(1, 2, 3)`.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs match {
    case Nil => (List.empty[A], rng)
    case head :: tail =>
      val (value, newRng) = head(rng)
      (List(value) ::: sequence(tail)(newRng)._1, newRng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }
}
