package chapter11

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R]: Monad[({ type f[x] = Reader[R, x] })#f] =
    new Monad[({ type f[x] = Reader[R, x] })#f] {
      def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
      def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader { (r: R) =>
        val a = fa.run(r)
        f(a).run(r)
      }
    }
}
