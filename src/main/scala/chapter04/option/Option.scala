package chapter04.option

sealed trait Option[+A] {
  // Apply f if the option is not None
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  // Apply f, which may fail, to the Option if not None
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(value) => f(value)
    case None => None
  }

  // The B >: A says that B type parameter must be a supertype of A
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  // Don't evaluate ob unless needed
  def orElse[B >: A](ob: Option[B]): Option[B] = this match {
    case Some(value) => Some(value)
    case None => ob
  }

  // Convert Some to None if the value doesn't satisfy f
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(value) if f(value) => Some(value)
    case _ => None
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
}

case class Some[+A](value: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  // Combine two Option values using binary function.
  // If either value is None, then return value is too.
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap  { i =>
      b map { j => f(i, j) }
    }
  }

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
}
