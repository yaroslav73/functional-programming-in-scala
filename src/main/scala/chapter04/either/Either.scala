package chapter04.either

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case left @ Left(_) => left
      case Right(value)   => Right(f(value))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(value) => f(value)
      case Left(value)  => Left(value)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(_) => this
      case Left(_)  => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap { a1 =>
      b map { b1 =>
        f(a1, b1)
      }
    }
}

final case class Left[+E](value: E) extends Either[E, Nothing]
final case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // TODO refactor methods below
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es match {
      case Nil          => Right(List.empty[A])
      case head :: tail => sequence(tail) flatMap { list => head map { e => e :: list } }
    }

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sequence(es map f)
}
