package chapter07

trait Par[A] {
}

object Par {
  def unit[A](a: A): Par[A] = ???

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???

  def fork[A](a: => Par[A]): Par[A] = ???

  def lazyUnit[A](a: => A): Par[A] = ???

  def run[A](par: Par[A]): A = ???
}
