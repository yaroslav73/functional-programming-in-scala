package chapter08

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(size: Int): Gen[A] = forSize(size)

  def map[B](f: A => B): SGen[B] = SGen(forSize andThen (_ map f))

  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(forSize andThen (_ flatMap f))

  def **[B](that: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** that(n))
}
