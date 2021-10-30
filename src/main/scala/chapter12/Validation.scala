package chapter12

sealed trait Validation[+E, +A]
object Validation {
  final case class Failure[E](head: E, tail: Vector[E] = Vector.empty[E]) extends Validation[E, Nothing]
  final case class Success[A](value: A) extends Validation[Nothing, A]

  def applicativeValidation[E]: Applicative[({ type f[x] = Validation[E, x] })#f] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
        (fa, fb) match {
          case (Success(a), Success(b))           => Success(f(a, b))
          case (Failure(ah, at), Failure(bh, bt)) => Failure(ah, (bh +: bt) ++ at)
          case (f @ Failure(_, _), _)             => f
          case (_, f @ Failure(_, _))             => f
        }
      }
    }
}
