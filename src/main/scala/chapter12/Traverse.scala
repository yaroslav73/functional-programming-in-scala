package chapter12

import language._
import language.implicitConversions

trait Traverse[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)
}

object Traverse {
  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    override def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
      val G = implicitly[Applicative[G]]
      fa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None    => G.unit(None)
      }
    }
  }

  val listTravers: Traverse[List] = new Traverse[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List.empty[B]))((a, glb) => G.map2(f(a), glb)(_ :: _))
  }

  def main(args: Array[String]): Unit = {
    import Applicative._

//    val res = optionTraverse.traverse(Option.empty[Int])(a => List(a))
    val res = listTravers.traverse(List.empty[Int])(a => Option(a))
    println(res)
  }
}
