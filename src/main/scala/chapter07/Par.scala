package chapter07
import scala.annotation.tailrec
import scala.concurrent.duration.TimeUnit

trait Par[A] {}

object Par {
  type Par[A] = ExecutorService => Future[A]

  // Promotes a constant value to a parallel computation.
  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // Combines the result of two parallel computations with a binary function.
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(pa, map2(pb, pc)((b, c) => (b, c))) { (a, pair) =>
      val (b, c) = pair
      f(a, b, c)
    }

  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] =
    map2(map2(pa, pb)((a, b) => (a, b)), map2(pc, pd)((c, d) => (c, d))) { (p1, p2) =>
      val (a, b) = p1
      val (c, d) = p2
      f(a, b, c, d)
    }

  // marks a computation for concurrent evaluation.
  // The evaluation wont actually occur until forced by run.
  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) =>
      es.submit(new Callable[A] {
        override def call: A = a(es).get
      })

  // Wraps it's unevaluated argument in a Par and marks it for concurrent evaluation.
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val psb = ps.map(a => asyncF(f)(a))
    sequence(psb)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List.empty[A]))((elem, init) => map2(elem, init)(_ :: _))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars = as.map(asyncF(a => if (f(a)) List(a) else List.empty))
    map(sequence(pars))(_.flatten)
  }

  // Extracts a value from a Par by actually performing the computation.
  def run[A](es: ExecutorService)(par: Par[A]): Future[A] = par(es)

  private final case class UnitFuture[A](get: A) extends Future[A] {
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def cancel(evenIfRunning: Boolean): Boolean = false
    override def isDone: Boolean = true
    override def isCancelled: Boolean = false
  }
}
