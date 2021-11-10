package chapter14

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.length)

  def write(i: Int, a: A): ST[S, Unit] =
    (s: S) => (value(i) = a, s)

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)
}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST {
      new STArray[S, A] {
        protected lazy val value: Array[A] = Array.fill(sz)(v)
      }
    }
}
