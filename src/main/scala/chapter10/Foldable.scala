package chapter10

import chapter03.datastructures.{Branch, Leaf, Tree}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A, B](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
  def toList[A](as: F[A]): List[A] = foldRight(as)(List.empty[A])((a, list) => a :: list)
}

object Foldable {
  object FoldableList extends Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  object FoldableIndexedSeq extends Foldable[IndexedSeq] {
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      Monoid.foldMapV(as, mb)(f)
  }

  object FoldableOption extends Foldable[Option] {
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case Some(a) => f(a)
        case None    => mb.zero
      }
  }

  object FoldableTree extends Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(a)      => f(a, z)
        case Branch(l, r) => foldRight(r)(foldRight(l)(z)(f))(f)
      }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(a)      => f(z, a)
        case Branch(l, r) => foldLeft(l)(foldLeft(r)(z)(f))(f)
      }

    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case Leaf(a)      => f(a)
        case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
      }
  }
}
