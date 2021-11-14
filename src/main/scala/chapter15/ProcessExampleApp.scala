package chapter15

object ProcessExampleApp extends App {
  val p = Process.liftOne((x: Int) => x * 2)

  val xs = p(LazyList(1, 2, 3)).toList

  println(xs)
}
