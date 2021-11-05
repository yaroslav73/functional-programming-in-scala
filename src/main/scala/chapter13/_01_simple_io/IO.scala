package chapter13._01_simple_io

import chapter13.Monad

trait IO[A] { self =>
  def run: A

  def map[B](f: A => B): IO[B] =
    new IO[B] {
      def run: B = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      def run: B = f(self.run).run
    }
}

object IO extends Monad[IO] {
  def apply[A](a: => A): IO[A] = unit(a)

  def unit[A](a: => A): IO[A] = new IO[A] { def run: A = a }

  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)

  def ref[A](a: A): IO[IORef[A]] = IO { new IORef(a) }
  sealed class IORef[A](var value: A) {
    def set(a: A): IO[A] = IO { value = a; a }
    def get: IO[A] = IO { value }
    def modify(f: A => A): IO[A] = get.flatMap(a => set(f(a)))
  }
}
