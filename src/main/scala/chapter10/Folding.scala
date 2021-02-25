package chapter10

object Folding {
  def concatenate[A](xs: List[A], m: Monoid[A]): A =
    xs.foldLeft(m.zero)(m.op)

  // If using map we will go over the list twice :(
  // Solution with one foldLeft better
  def foldMap[A, B](xs: List[A], m: Monoid[B])(f: A => B): B = {
    // xs.map(f).foldLeft(m.zero)(m.op)
    xs.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }

  def foldLeft[A, B: Monoid](xs: List[A])(zero: B)(f: (B, A) => B): B = {
    val instance = implicitly[Monoid[B]]
    foldMap(xs, instance)(f(zero, _))
  }

  def foldRight[A, B: Monoid](xs: List[A])(zero: B)(f: (A, B) => B): B = {
    val instance = implicitly[Monoid[B]]
    foldMap(xs, instance)(f(_, zero))
  }

  // Solution from author:
  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, Monoid.endoMonoid[B])(f.curried)(z)

  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, Monoid.dual(Monoid.endoMonoid[B]))(a => b => f(b, a))(z)
}
