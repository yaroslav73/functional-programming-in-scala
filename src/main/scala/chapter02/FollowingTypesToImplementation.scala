package chapter02

object FollowingTypesToImplementation {
  def partial[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  // Braces can be removed, because => associates to thee right
  // A => (B => C) is the same as A => B => C
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => f(a, _)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  // We can omit type for function argument a
  // it will be inferred by compiler from context
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
