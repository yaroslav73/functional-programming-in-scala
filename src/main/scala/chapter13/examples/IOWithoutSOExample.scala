package chapter13.examples

import chapter13._
import chapter13._02_io_without_so.{IO, Return}

object IOWithoutSOExample extends App {
  def PrintLine(msg: String): _01_simple_io.IO[Unit] = _01_simple_io.IO { println(msg) }
  val p = _01_simple_io.IO.forever(PrintLine("Still going..."))

//  TODO if run get StackOverflow Error
//  p.run

  val f: Int => IO[Int] = (i: Int) => Return(i)

  val g: Int => IO[Int] =
    List.fill(10000)(f).foldLeft(f) {
      (a: Int => IO[Int], b: Int => IO[Int]) =>
        { (x: Int) =>
          IO.suspend(a(x).flatMap(b))
        }
    }

  val gFortyTwo = g(42)
  println(s"g(42) = $gFortyTwo")
  println(s"IO.run(g(42)) = ${IO.run(gFortyTwo)}")
}
