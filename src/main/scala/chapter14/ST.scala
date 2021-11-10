package chapter14

trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] =
    (s: S) => {
      val (a, newS) = self.run(s)
      (f(a), newS)
    }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] =
    (s: S) => {
      val (a, newS) = self.run(s)
      f(a).run(newS)
    }
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val cache = a
    (s: S) => (cache, s)
  }

  def runST[A](runnableST: RunnableST[A]): A =
    runnableST.apply[Unit].run(())._1
}
