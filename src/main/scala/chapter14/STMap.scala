package chapter14

import scala.collection.mutable

sealed trait STMap[S, K, V] {
  protected def table: mutable.HashMap[K, V]

  def size: ST[S, Int] = ST(table.size)

  // Get the value under a key
  def apply(k: K): ST[S, V] = ST(table(k))

  // Get the value under a key, or None if the key does not exist
  def get(k: K): ST[S, Option[V]] = ST(table.get(k))

  // Add a value under a key
  def +=(kv: (K, V)): ST[S, Unit] = ST(table += kv)

  // Remove a key
  def -=(k: K): ST[S, Unit] = ST(table.remove(k))
}

object STMap {
  def empty[S, K, V]: ST[S, STMap[S, K, V]] =
    ST(
      new STMap[S, K, V] {
        protected def table: mutable.HashMap[K, V] = mutable.HashMap.empty
      }
    )

  def fromMap[S, K, V](m: Map[K, V]): ST[S, STMap[S, K, V]] =
    ST(
      new STMap[S, K, V] {
        protected def table: mutable.HashMap[K, V] = mutable.HashMap.from(m)
      }
    )
}
