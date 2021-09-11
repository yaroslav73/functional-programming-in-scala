package chapter04.option

sealed trait Option[+A] {
  // Apply f if the option is not None
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(value) => Some(f(value))
      case None        => None
    }

  // Apply f, which may fail, to the Option if not None
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  // The B >: A says that B type parameter must be a supertype of A
  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(value) => value
      case None        => default
    }

  // Don't evaluate ob unless needed
  def orElse[B >: A](that: Option[B]): Option[B] = map(Some(_)).getOrElse(that)

  // Convert Some to None if the value doesn't satisfy f
  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
}

case class Some[+A](value: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  // Combine two Option values using binary function.
  // If either value is None, then return value is too.
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a => b.map(b => f(a, b)))

  def sequence[A](xs: List[Option[A]]): Option[List[A]] =
    traverse(xs)(a => a)

  def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] =
    xs.foldRight[Option[List[B]]](Some(Nil))((elem, init) => map2(f(elem), init)(_ :: _))

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
}
