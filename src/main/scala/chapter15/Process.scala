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
}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Await[I, O](recovery: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]

object Process {
  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(value) => Emit(f(value))
      case None        => Halt()
    }
}
