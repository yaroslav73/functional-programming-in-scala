package chapter12

import chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def ap[A, B](f: F[A => B])(fa: F[A]): F[B] = map2(fa, f)((a, f) => f(a))
  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    ap(ap(unit(f.curried))(fa))(fb) // -> is the same as ap(map(fa)(f.curried))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    ap(ap(ap(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    ap(ap(ap(ap(unit(f.curried))(fa))(fb))(fc))(fd)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def sequenceMap[K, V](mfv: Map[K, F[V]]): F[Map[K, V]] =
    mfv.foldRight(unit(Map.empty[K, V])) { case ((k, v), acc) => map2(v, acc)((a, map) => map + (k -> a)) }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb) { case (a, b) => (a, b) }

  def product[G[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    val self = this
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def ap[A, B](f: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) = {
        val (fa, ga) = p
        val (fab, gab) = f
        (self.ap(fab)(fa), G.ap(gab)(ga))
      }
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] = {
    val self = this
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga, fgb) { case (ga, gb) => G.map2(ga, gb)(f) }
    }
  }
}

object Applicative {
  val lazyListApplicative: Applicative[LazyList] = new Applicative[LazyList] {
    def unit[A](a: => A): LazyList[A] = LazyList.continually(a) // The infinite, constant stream

    override def map2[A, B, C](fa: LazyList[A], fb: LazyList[B])(f: (A, B) => C): LazyList[C] =
      fa.zip(fb).map(f.tupled)

    override def sequence[A](fas: List[LazyList[A]]): LazyList[List[A]] =
      fas.foldRight(unit(List.empty[A]))((acc, a) => map2(acc, a)((a, b) => a :: b))
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    override def unit[A](a: => A): List[A] = List(a)
  }

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      fa.zip(fb).map { case (a, b) => f(a, b) }
  }

  def main(args: Array[String]): Unit = {
    val ll1 = LazyList.continually("a")
    val ll2 = LazyList.continually("b")
    val ll3 = LazyList.continually("c")

    val lll = List(ll1, ll2, ll3)

    println(lll)
    println(lll.flatMap(_.take(3).toList))
    println(lll.map(_.take(3).toList))
    println(lazyListApplicative.sequence(lll).take(4).toList)
  }
}
