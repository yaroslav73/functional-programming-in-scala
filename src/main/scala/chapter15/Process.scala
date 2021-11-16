package chapter15

sealed trait Process[I, O] {
  def apply(in: LazyList[I]): LazyList[O] =
    this match {
      case Halt() => LazyList.empty[O]
      case Await(recovery) =>
        in match {
          case head #:: tail => recovery(Some(head))(tail)
          case xs            => recovery(None)(xs)
        }
      case Emit(head, tail) => head #:: tail(in)
    }

  def repeat: Process[I, O] = {
    def loop(p: Process[I, O]): Process[I, O] =
      p match {
        case Halt() => loop(this)
        case Await(recovery) =>
          Await {
            case None => recovery(None)
            case elem => loop(recovery(elem))
          }
        case Emit(head, tail) => Emit(head, loop(tail))
      }

    loop(this)
  }

  def |>[O2](p: Process[O, O2]): Process[I, O2] =
    p match {
      case Halt()           => Halt()
      case Emit(head, tail) => Emit(head, this |> tail)
      case Await(f) =>
        this match {
          case Emit(head, tail) => tail |> f(Some(head))
          case Await(g)         => Await(i => g(i) |> p)
          case Halt()           => Halt[I, O]() |> f(None)
        }
    }
}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Await[I, O](recovery: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]

object Process {
  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(value) => Emit(f(value))
      case _           => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] =
    liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(value) if p(value) => Emit(value)
      case _                       => Halt()
    }.repeat

  def take[I](n: Int): Process[I, I] = {
    def loop(n: Int): Process[I, I] =
      Await {
        case Some(value) if n != 0 => Emit(value, loop(n - 1))
        case _                     => Halt()
      }

    loop(n)
  }

  def drop[I](n: Int): Process[I, I] = {
    def loop(n: Int): Process[I, I] =
      Await {
        case Some(_) if n != 0 => loop(n - 1)
        case Some(value)       => Emit(value, loop(n))
        case _                 => Halt()
      }

    loop(n)
  }

  def takeWhile[I](p: I => Boolean): Process[I, I] = filter(p)

  def dropWhile[I](p: I => Boolean): Process[I, I] = {
    Await[I, I] {
      case Some(value) if p(value) => dropWhile(p)
      case Some(value)             => Emit(value)
      case _                       => Halt()
    }.repeat
  }

  def count[I]: Process[I, Int] =
    loop(0)((_, s) => (s + 1, s + 1))

  def sum: Process[Double, Double] =
    loop(0.0)((i, s) => (i + s, i + s))

  def mean: Process[Double, Double] = {
    def loop(counter: Int, acc: Double): Process[Double, Double] =
      Await {
        case Some(value) => Emit((acc + value) / (counter + 1), loop(counter + 1, acc + value))
        case None        => Halt()
      }

    loop(0, 0)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    Await {
      case Some(i) =>
        f(i, z) match {
          case (o, s2) => Emit(o, loop(s2)(f))
        }
      case None => Halt()
    }
}
