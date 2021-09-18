package chapter06

// type State[S, +A] = S => (A, S)
// type Rand[+A] = State[RNG, A]
case class StateOld[S, +A](run: S => (A, S)) {

  import chapter06.StateOld._

  def map[A, B](s: StateOld[S, A])(f: A => B): StateOld[S, B] = flatMap(s)(a => unit(f(a)))

  def flatMap[A, B](s: StateOld[S, A])(g: A => StateOld[S, B]): StateOld[S, B] = StateOld { state =>
    val (a, s1) = s.run(state)
    g(a).run(s1)
  }
}

object StateOld {
  def unit[S, A](a: A): StateOld[S, A] = StateOld { s: S => (a, s) }

  def map2[S, A, B, C](sa: StateOld[S, A], sb: StateOld[S, B])(f: (A, B) => C): StateOld[S, C] = StateOld { state =>
    val (a, s1) = sa.run(state)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  }

  // TODO implement it with foldRight
  def sequence[S, A](fs: List[StateOld[S, A]]): StateOld[S, List[A]] = StateOld { state =>
    fs match {
      case Nil => (Nil, state)
      case head :: tail =>
        val (a, s) = head.run(state)
        (List(a) ::: sequence(tail).run(s)._1, s)
    }
  }
}
