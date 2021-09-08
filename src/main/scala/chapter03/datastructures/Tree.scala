package chapter03.datastructures

sealed trait Tree[+A] {
  def size: Int

  def depth: Int
}

final case class Leaf[A](value: A) extends Tree[A] {
  override def size: Int = 1

  override def depth: Int = 1
}

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def size: Int = left.size + right.size

  override def depth: Int = this match {
    case Branch(Leaf(_), Leaf(_)) => 2
    case Branch(left, Leaf(_)) => left.depth
    case Branch(Leaf(_), right) => right.depth
    case Branch(left, right) => left.depth + right.depth + 1
  }
}

object Tree {
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }


  def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _)

  //  def maximum(tree: Tree[Int]): Int = fold(tree)(elem => elem)((l, r) => l max r)

  //  def depth[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => 1 + (l max r))

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(elem => Leaf(f(elem)): Tree[B])((l, r) => Branch(l, r))

  def oldMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(oldMap(left)(f), oldMap(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }
}
