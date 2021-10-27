package chapter11

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
  }

  def main(args: Array[String]): Unit = {
    val hello = Id("Hello, ")
    val monad = Id("Monad!")

    val res = hello flatMap { a =>
      monad flatMap { b =>
        Id(a + b)
      }
    }

    println(res)
  }
}
