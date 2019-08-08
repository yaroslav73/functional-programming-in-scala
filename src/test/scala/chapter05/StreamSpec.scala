package chapter05

import org.scalatest.WordSpec

class StreamSpec extends WordSpec {

  "A Stream" when {
    "call function fibs should be generates stream of Fibonacci numbers" in {
      val fibs = Stream.fibs().take(8)
      assert(fibs.toList == List(0, 1, 1, 2, 3, 5, 8, 13))
    }

    "call unfold function" in {
      val unfolded = Stream.unfold(0)((n: Int) => Option(n, n + 1))
      assert(unfolded.take(5).toList == List(0, 1, 2, 3, 4))
    }

    "call unfoldOnes and take first 3 elements should return List(1, 1, 1)" in {
      assert(Stream.unfoldOnes().take(3).toList == List(1, 1, 1))
    }

    "call unfoldFrom(5) and take first 3 elements should return List(5, 6, 7)" in {
      assert(Stream.unfoldFrom(5).take(3).toList == List(5, 6, 7))
    }

    "call unfoldConstant('Hello') and take first 3 elements should return List('Hello', 'Hello','Hello')" in {
      assert(Stream.unfoldConstant("Hello").take(3).toList == List("Hello", "Hello", "Hello"))
    }

    "call function unfoldFibs should be generates stream of Fibonacci numbers" in {
      val fibs = Stream.unfoldFibs().take(8)
      assert(fibs.toList == List(0, 1, 1, 2, 3, 5, 8, 13))
    }

    "call function mapUnfold should be apply map function to stream" in {
      val stream = Stream(1, 2, 3, 4, 5)
      assert(stream.mapUnfold(n => n * n).toList == List(1, 4, 9, 16, 25))
    }

    "call function takeUnfold(3) should be return stream of 3 items" in {
      val stream = Stream(1, 2, 3, 4, 5, 6, 7)
      assert(stream.takeUnfold(3).toList == List(1, 2, 3))
    }

    "call function takeWhileUnfold(_ < 3) should be return stream of 2 items" in {
      val stream = Stream(1, 2, 3, 4, 5, 6, 7)
      assert(stream.takeWhileUnfold(_ < 3).toList == List(1, 2))
    }

    "call function zipWith with Stream('one', 'two', 'three') should be return ..." in {
      val s1 = Stream("one", "two", "three")
      val s2 = Stream(1, 2, 3)
      assert(s1.zipWith(s2)((e1, e2) => (e1, e2)).toList == List(("one", 1), ("two", 2), ("three", 3)))
    }

    "call function zipWith with Stream('one', 'one', 'one') should be return ..." in {
      val s1 = Stream.constant("one")
      val s2 = Stream.constant(1)
      assert(s1.zipWith(s2)((e1, e2) => (e1, e2)).take(3).toList == List(("one", 1), ("one", 1), ("one", 1)))
    }

    "call function zipAll with Stream('one', 'one', 'one') should be return ..." in {
//      val s1 = Stream.constant("one")
      val s1 = Stream("one", "two")
      val s2 = Stream.constant(1)
      assert(s1.zipAll(s2).take(3).toList == List((Option("one"), Option(1)), (Option("two"), Option(1)), (None, Option(1))))
    }

    "check hasSubsequence function" in {
      val s1 = Stream.constant("one")
      val s2 = Stream("one", "one")
      assert(s1.hasSubsequence(s2))
    }

    "check startWith function" in {
      val s1 = Stream.constant("one")
      val s2 = Stream("one", "one")
      assert(s1 startWith s2)
    }

    "check tails function" in {
      val s1 = Stream("one", "two", "three")
      assert(s1.tails.map(_.toList).toList == List(List("one", "two", "three"), List("two", "three"), List("three")))
    }

    "check scanRight function" in {
      val s1 = Stream(1, 2, 3)
      assert(s1.scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
    }
  }
}
