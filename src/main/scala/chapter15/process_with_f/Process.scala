package chapter15.process_with_f

trait Process[F[_], O] {
  import Process._

  def onHalt(f: Throwable => Process[F, O]): Process[F, O] =
    this match {
      case Halt(error)              => f(error)
      case Emit(head, tail)         => Emit(head, tail.onHalt(f))
      case Await(request, recovery) => Await(request, recovery.andThen(p => p.onHalt(f)))
    }

  def ++(p: Process[F, O]): Process[F, O] =
    this.onHalt {
      case End   => p
      case error => Halt(error)
    }
}

object Process {
  // The recovery function now takes an Either so we can handle errors.
  final case class Await[F[_], A, O](request: F[A], recovery: Either[Throwable, A] => Process[F, O]) extends Process[F, O]
  final case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]
  // Halt due to err, which could be an actual error or End indicating normal termination
  final case class Halt[F[_], O](error: Throwable) extends Process[F, O]

  // An Exception that indicates normal termination.
  // This allows us to use Scala’s exception mechanism for control flow.
  case object End extends Exception
  // An Exception that indicates forceful termination.
  // We’ll see how this is used later.
  case object Kill extends Exception
}
