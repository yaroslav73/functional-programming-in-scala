package chapter03.datastructures

sealed trait Tree[+A] {
  def size: Int

  def depth: Int

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(left.map(f), right.map(f))
  }

  def fold[B](f: A => B)(ff: (B, B) => B): B = this match {
    case Leaf(value) => f(value)
    case Branch(left, right) => ff(left.fold(f)(ff), right.fold(f)(ff))
  }
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
  def maximum(tree: Tree[Int]): Int = tree.fold(v => v)(_ max _)
}
