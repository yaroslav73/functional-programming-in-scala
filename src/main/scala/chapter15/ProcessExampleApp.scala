package chapter15

import chapter13._02_io_without_so.IO

import java.io.File

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

  val sum = Process.sum(LazyList(1, 2, 3, 4, 5)).toList
  println(s"Sum: $sum")

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

  val zipWithIndexed = Process.take(3).zipWithIndex(LazyList("a", "b", "c")).toList
  println(s"ZipWithIndexed: $zipWithIndexed")

  val file = new File("src/main/resources/Fahrenheit.txt")
  val result = Process.processFile(file, Process.count.|>(Process.exists(_ > 20)), false)(_ || _)
  println(s"Fahrenheit.txt count exist _ > 20? = ${IO.run(result)}")

  def convertFahrenheit: Process[String, String] =
    Process.filter((line: String) => !line.startsWith("#")) |>
      Process.filter(line => line.trim.nonEmpty) |>
      Process.lift(line => toCelsius(line.toDouble).toString)

  def toCelsius(fahrenheit: Double): Double =
    (5.0 / 9.0) * (fahrenheit - 32.0)
}
