package chapter07

trait Par[A] {}

object Par {
  type Par[A] = ExecutorService => Future[A]

  // Promotes a constant value to a parallel computation.
  def unit[A](a: A): Par[A] = ???

  // Combines the result of two parallel computations with a binary function.
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???

  // marks a computation for concurrent evaluation.
  // The evaluation wont actually occur until forced by run.
  def fork[A](a: => Par[A]): Par[A] = ???

  // Wraps it's unevaluated argument in a Par and marks it for concurrent evaluation.
  def lazyUnit[A](a: => A): Par[A] = ???

  // Extracts a value from a Par by actually performing the computation.
  def run[A](es: ExecutorService)(par: Par[A]): Future[A] = par(es)
}
