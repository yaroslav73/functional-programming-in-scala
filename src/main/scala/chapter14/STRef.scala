package chapter14

sealed trait STRef[S, A] {
  protected var cell: A
  def read: ST[S, A] = ST(cell)
  def write(a: A): ST[S, Unit] =
    (s: S) => {
      cell = a
      ((), s)
    }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] =
    ST(new STRef[S, A] {
      protected var cell: A = a
    })
}
