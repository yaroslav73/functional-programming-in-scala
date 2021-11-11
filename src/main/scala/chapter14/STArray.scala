package chapter14

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.length)

  def write(i: Int, a: A): ST[S, Unit] =
    (s: S) => (value(i) = a, s)

  def read(i: Int): ST[S, A] = ST(value(i))

  def fill(xs: Map[Int, A]): ST[S, Unit] =
    xs.foldLeft(ST[S, Unit](())) {
      case (acc, (i, a)) => acc.flatMap(_ => write(i, a))
    }

  def swap(i: Int, j: Int): ST[S, Unit] =
    for {
      x <- read(i)
      y <- read(j)
      _ <- write(j, x)
      _ <- write(i, y)
    } yield ()

  def freeze: ST[S, List[A]] = ST(value.toList)
}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(
      new STArray[S, A] {
        protected lazy val value: Array[A] = Array.fill(sz)(v)
      }
    )

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(
      new STArray[S, A] {
        protected lazy val value: Array[A] = xs.toArray
      }
    )
}
