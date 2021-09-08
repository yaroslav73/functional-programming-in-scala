package chapter03.datastructures

sealed trait Tree[+A] {
  def size: Int
}
case class Leaf[A](value: A) extends Tree[A] {
  override def size: Int = 1
}
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def size: Int = left.size + right.size
}

object Tree {
  def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _)

  def maximum(tree: Tree[Int]): Int = fold(tree)(elem => elem)((l, r) => l max r)

  def depth[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => 1 + (l max r))

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
