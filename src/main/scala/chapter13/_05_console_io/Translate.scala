package chapter13._05_console_io

import chapter07.Nonblocking.Par

trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}

object Translate {
  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0: Console ~> Function0 = new (Console ~> Function0) {
    override def apply[A](f: Console[A]): () => A = f.toThunk
  }

  val consoleToPar: Console ~> Par = new (Console ~> Par) {
    override def apply[A](f: Console[A]): Par[A] = f.toPar
  }
}
