package chapter07

abstract class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}
