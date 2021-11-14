package chapter15

object ProcessExampleApp extends App {
  val p01 = Process.liftOne((x: Int) => x * 2)

  val xs01 = p01(LazyList(1, 2, 3)).toList
  println(xs01)

  val p02 = Process.lift((x: Int) => x * 2)
  val xs02 = p02(LazyList(1, 2, 3)).toList
  println(xs02)

  val even = Process.filter((x: Int) => x % 2 == 0)
  val evens = even(LazyList(1, 2, 3, 4)).toList
  println(evens)

  val s = Process.sum(LazyList(1, 2, 3, 4, 5)).toList
  println(s)

  val take = Process.take(3)(LazyList(1, 2, 3, 4, 5)).toList
  println(s"Take 3: $take")

  val drop = Process.drop(3)(LazyList(1, 2, 3, 4, 5)).toList
  println(s"Drop 3: $drop")

  val takeWhile = Process.takeWhile((x: Int) => x <= 3)(LazyList(1, 2, 3, 4, 5)).toList
  println(s"Take while _ <= 4: $takeWhile")

  val dropWhile = Process.dropWhile((x: Int) => x <= 3)(LazyList(1, 2, 3, 4, 5)).toList
  println(s"Drop while _ <= 4: $dropWhile")

  val count = Process.count(LazyList("a", "b", "c")).toList
  println(s"Count: $count")

  val mean = Process.mean(LazyList(1, 2, 3, 4, 5)).toList
  println(s"Mean: $mean")
}
