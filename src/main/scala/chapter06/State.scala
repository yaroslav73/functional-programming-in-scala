package chapter06

// type State[S, +A] = S => (A, S)
// type Rand[+A] = State[RNG, A]
case class State[S, +A](run: S => (A, S)) {
  def map[A, B](s: State[S, A])(f: A => B): State[S, B] = State { state =>
    val (a, s1) = s.run(state)
    (f(a), s1)
  }

  def flatMap[A, B](s: State[S, A])(g: A => State[S, B]): State[S, B] = State { state =>
    val (a, s1) = s.run(state)
    g(a).run(s1)
  }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State { state =>
    fs match {
      case Nil =>
        val (_, s1) = run(state)
        (Nil, s1)
      case head :: tail =>
        val (a, s) = head.run(state)
        (List(a) ::: sequence(tail).run(s)._1, s)
    }
  }
}

object State {
  def unit[S, A](a: A): State[S, A] = State { s: S => (a, s) }

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = State { state =>
    val (a, s1) = sa.run(state)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  }
}
