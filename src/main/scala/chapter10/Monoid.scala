package chapter10

/*
 * Monoid consists of the following:
 * - some type A
 * - an associative binary operation, op, that takes two values of typ A
 *   and combines them into one: op(x, y), z) == op(x, op(y, z))
 *   for any of x: A, y: A, z: A
 * - a value, zero: A, that is an identity for that operation:
 *   op(x, zero) == op(zero, x) == x for any x: A
 *
 * Last two point called the monoid laws.
 */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def listMonoid[A]: Monoid[List[A]] =
    new Monoid[List[A]] {
      override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
      override def zero: List[A] = Nil
    }

  // Uncomment code below:
  //  def optionMonoid[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
  //    val instance: Monoid[A] = implicitly[Monoid[A]]
  //    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.flatMap(v1 => a1.map(v2 => instance.op(v1, v2)))
  //    override def zero: Option[A] = None
  //  }

  // There is a choice of implementation here as well.
  // Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
  def endoMonoid[A]: Monoid[A => A] =
    new Monoid[A => A] {
      override def op(f: A => A, g: A => A): A => A = f andThen g
      override def zero: A => A = (a: A) => a
    }

  // Solution from author:
  // Notice that we have a choice in how we implement `op`.
  // We can compose the options in either order. Both of those implementations
  // satisfy the monoid laws, but they are not equivalent.
  // This is true in general - that is, every monoid has a _dual_ where the
  // `op` combines things in the opposite order. Monoids like `booleanOr` and
  // `intAddition` are equivalent to their duals because their `op` is commutative
  // as well as associative.
  def optionMonoid[A]: Monoid[Option[A]] =
    new Monoid[Option[A]] {
      def op(x: Option[A], y: Option[A]): Option[A] = x orElse y
      val zero: Option[A] = None
    }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] =
    new Monoid[A] {
      def op(x: A, y: A): A = m.op(y, x)
      val zero: A = m.zero
    }

  // Now we can have both monoids on hand:
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) m.zero
    else if (as.length == 1) f(as.head)
    else {
      val (p1, p2) = as.splitAt(as.length / 2)
      m.op(foldMapV(p1, m)(f), foldMapV(p2, m)(f))
    }
  }

  /*
   * Hard: Use foldMap to detect whether a given IndexedSeq[Int] is ordered.
   * You’ll need to come up with a creative Monoid.
   */
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = {
        (o1, o2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) => Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None)                              => x
          case (None, x)                              => x
        }
      }

      override def zero: Option[(Int, Int, Boolean)] = None
    }

    Folding.foldMap(ints.toList, m)(i => Some((i, i, true))).forall { case (_, _, p) => p }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(a: (A, B), b: (A, B)): (A, B) =
        (a, b) match {
          case ((a1, a2), (b1, b2)) => (A.op(a1, b1), B.op(a2, b2))
        }

      def zero: (A, B) = (A.zero, B.zero)
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }

      def zero: Map[K, V] = Map.empty
    }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(f: A => B, g: A => B): A => B = a => B.op(f(a), g(a))
      def zero: A => B = _ => B.zero
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))

  def main(args: Array[String]): Unit = {
    println(ordered(IndexedSeq(1, 2, 3, 4)))
    println(ordered(IndexedSeq(7, 2, 8, 3)))
    println(ordered(IndexedSeq(7, 6, 5, 5)))

    val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))
    val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
    val m2 = Map("o1" -> Map("i2" -> 3))
    val m3 = M.op(m1, m2)
    println(m3)

    val b = bag(Vector("a", "rose", "is", "a", "rose"))
    println(b)

    val PM: Monoid[(Int, Int)] = productMonoid(intAddition, intAddition)
    val p = Foldable.FoldableList.foldMap(List(1, 2, 3, 4))(a => (1, a))(PM)
    println(s"p: $p, p mean: ${ val (a, b) = p; b / a }")
  }
}
