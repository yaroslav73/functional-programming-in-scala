package chapter07

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}
