package chapter06

import chapter06.State.unit

import scala.annotation.tailrec

final case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](that: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => that.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, ns) = run(s)
      f(a).run(ns)
    }
}

object State {
  def unit[A, S](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[A, S](states: List[State[S, A]]): State[S, List[A]] =
    State { s =>
      @tailrec
      def loop(states: List[State[S, A]], acc: List[A], ns: S): State[S, List[A]] = {
        states match {
          case Nil => State(s => (acc, s))
          case head :: tail =>
            val (a, s) = head.run(ns)
            loop(tail, acc :+ a, s)
        }
      }

      loop(states, List.empty[A], s).run(s)
    }
}
