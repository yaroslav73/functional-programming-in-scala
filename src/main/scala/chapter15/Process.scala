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

  def sum: Process[Double, Double] = {
    def loop(acc: Double): Process[Double, Double] =
      Await {
        case Some(value) => Emit(acc + value, loop(acc + value))
        case None        => Halt()
      }

    loop(0.0)
  }
}
