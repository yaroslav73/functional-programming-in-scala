package chapter15.process_with_f

import chapter13._02_io_without_so.IO

import scala.annotation.tailrec

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

  def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] =
    this match {
      case Halt(error)              => Halt(error)
      case Emit(head, tail)         => Try(f(head) ++ tail.flatMap(f))
      case Await(request, recovery) => Await(request, recovery.andThen(_.flatMap(f)))
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

  def Try[F[_], O](p: Process[F, O]): Process[F, O] =
    try p
    catch { case e: Throwable => Halt(e) }

  def await[F[_], A, O](request: F[A])(recovery: Either[Throwable, A] => Process[F, O]): Process[F, O] =
    Await(request, recovery)

  def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] =
    IO {
      val E = java.util.concurrent.Executors.newFixedThreadPool(4)
      @tailrec
      def loop(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] =
        cur match {
          case Emit(head, tail) => loop(tail, acc :+ head)
          case Halt(End)        => acc
          case Halt(error)      => throw error
          case Await(request, recovery) =>
            val next =
              try recovery(Right(IO.run(request)))
              catch { case error: Throwable => recovery(Left(error)) }
            loop(next, acc)
        }

      try loop(src, IndexedSeq.empty[O])
      finally E.shutdown()
    }
}
