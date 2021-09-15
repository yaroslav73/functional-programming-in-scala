package chapter05

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StreamSpec extends AnyWordSpec with Matchers {

  "A Stream" should {
    "headOption should return first element for non-empty Stream or None" in {
      val nonEmptyStream = Stream(1, 2, 3, 4, 5)
      val emptyStream = Stream.empty[Int]

      nonEmptyStream.headOption shouldBe Some(1)
      emptyStream.headOption shouldBe None
    }

    "toList should convert Stream to List" in {
      val nonEmptyStream = Stream(1, 2, 3, 4, 5)
      val emptyStream = Stream.empty[Int]

      nonEmptyStream.toList shouldBe List(1, 2, 3, 4, 5)
      emptyStream.toList shouldBe List.empty[Int]
    }

    "take(n) should return first n elements of Stream" in {
      val nonEmptyStream = Stream(1, 2, 3, 4, 5)
      val emptyStream = Stream.empty[Int]

      nonEmptyStream.take(3).toList shouldBe List(1, 2, 3)
      emptyStream.take(3) shouldBe Stream.empty[Int]
    }

    "drop(n) should drop first n elements of Stream" in {
      val nonEmptyStream = Stream(1, 2, 3, 4, 5)
      val emptyStream = Stream.empty[Int]

      nonEmptyStream.drop(3).toList shouldBe List(4, 5)
      emptyStream.drop(3) shouldBe Stream.empty[Int]
    }

    "takeWhile should return all starting elements of Stream that match the given predicate" in {
      val nonEmptyStream = Stream(1, 2, 3, 4, 5)
      val emptyStream = Stream.empty[Int]

      nonEmptyStream.takeWhile(_ < 4).toList shouldBe List(1, 2, 3)
      emptyStream.takeWhile(_ < 0) shouldBe Stream.empty[Int]
    }

    "exist should return true if element present in Stream and false otherwise" in {
      val nonEmptyStream = Stream(1, 2, 3, 4, 5)
      val emptyStream = Stream.empty[Int]

      nonEmptyStream.exist(_ == 3) shouldBe true
      nonEmptyStream.exist(_ > 3) shouldBe true
      nonEmptyStream.exist(_ >= 10) shouldBe false
      emptyStream.exist(_ == 3) shouldBe false
    }

    "forAll should return true if all elements match the given predicate" in {
      val nonEmptyStream = Stream(1, 2, 3, 4, 5)
      val emptyStream = Stream.empty[Int]

      nonEmptyStream.forAll(_ == 3) shouldBe false
      nonEmptyStream.forAll(_ < 6) shouldBe true
      nonEmptyStream.forAll(_ < 5) shouldBe false
      emptyStream.forAll(_ == 3) shouldBe true
    }

    "map should return transformed Stream with function f" in {
      val nonEmptyStream = Stream(1, 2, 3, 4, 5)
      val emptyStream = Stream.empty[Int]

      nonEmptyStream.map(_ * 3).toList shouldBe List(3, 6, 9, 12, 15)
      nonEmptyStream.map(_ * 3).map(_.toString).toList shouldBe List("3", "6", "9", "12", "15")
      emptyStream.map(_ * 3) shouldBe Stream.empty[Int]
      emptyStream.map(_ * 3).toList shouldBe List.empty[Int]
    }

    "filter should return filtered Stream with elements match with predicate" in {
      val nonEmptyStream = Stream(1, 2, 3, 4, 5)
      val emptyStream = Stream.empty[Int]

      nonEmptyStream.filter(_ % 2 == 0).toList shouldBe List(2, 4)
      nonEmptyStream.filter(_ % 2 != 0).toList shouldBe List(1, 3, 5)
      emptyStream.filter(_ == 3) shouldBe Stream.empty[Int]
    }

    "append should return Stream with appended element" in {
      val nonEmptyStream = Stream(1, 2, 3, 4, 5)
      val emptyStream = Stream.empty[Int]

      nonEmptyStream.append(6).toList shouldBe List(1, 2, 3, 4, 5, 6)
      emptyStream.append(1).toList shouldBe List(1)
    }

    "append should return Stream with elements of appending Stream" in {
      val nonEmptyStream = Stream(1, 2, 3, 4, 5)
      val emptyStream = Stream.empty[Int]

      nonEmptyStream.append(Stream(6, 7, 8)).toList shouldBe List(1, 2, 3, 4, 5, 6, 7, 8)
      nonEmptyStream.append(emptyStream).toList shouldBe List(1, 2, 3, 4, 5)
      emptyStream.append(Stream(1, 2, 3)).toList shouldBe List(1, 2, 3)
      emptyStream.append(emptyStream) shouldBe Stream.empty[Int]
    }

    "flatMap should return transformed Stream with function f" in {
      val nonEmptyStream = Stream(1, 2, 3, 4, 5)
      val emptyStream = Stream.empty[Int]

      nonEmptyStream.flatMap(n => Stream(n -> n * n)).toList shouldBe List(1 -> 1, 2 -> 4, 3 -> 9, 4 -> 16, 5 -> 25)
      emptyStream.flatMap(n => Stream(n -> n * n)) shouldBe Stream.empty[Int]
    }

    "find should return Some element or None otherwise" in {
      val nonEmptyStream = Stream(1, 2, 3, 4, 5)
      val emptyStream = Stream.empty[Int]

      nonEmptyStream.find(_ == 3) shouldBe Some(3)
      nonEmptyStream.find(_ % 2 == 0) shouldBe Some(2)
      nonEmptyStream.find(_ > 7) shouldBe None
      emptyStream.find(_ % 2 == 0) shouldBe None
    }

    "constant should return infinite stream" in {
      val infiniteStream = Stream.constant(1)

      infiniteStream.take(3).toList shouldBe List(1, 1, 1)
      infiniteStream.take(737).toList shouldBe List.fill(737)(1)
    }

    "from should return infinite stream of values n, n+1, n+2 and so on" in {
      val infiniteStream = Stream.from(37)

      infiniteStream.take(3).toList shouldBe List(37, 38, 39)
    }

    "fibs should be generates infinite Stream of Fibonacci numbers" in {
      val fibs = Stream.fibs().take(8)
      fibs.toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13)
    }

    "call unfold function" in {
      val unfolded = Stream.unfold(0)((n: Int) => Option(n, n + 1))
      unfolded.take(5).toList shouldBe List(0, 1, 2, 3, 4)
    }

    "call function zipWith with Stream('one', 'two', 'three') should be return ..." in {
      val s1 = Stream("one", "two", "three")
      val s2 = Stream(1, 2, 3)
      s1.zipWith(s2)((e1, e2) => (e1, e2)).toList shouldBe List(("one", 1), ("two", 2), ("three", 3))
    }

    "call function zipWith with Stream('one', 'one', 'one') should be return ..." in {
      val s1 = Stream.constant("one")
      val s2 = Stream.constant(1)
      s1.zipWith(s2)((e1, e2) => (e1, e2)).take(3).toList shouldBe List(("one", 1), ("one", 1), ("one", 1))
    }

    "call function zipAll with Stream('one', 'one', 'one') should be return ..." in {
      val s1 = Stream("one", "two")
      val s2 = Stream.constant(1)
      s1.zipAll(s2).take(3).toList shouldBe List((Option("one"), Option(1)), (Option("two"), Option(1)), (None, Option(1)))
    }

    "check hasSubsequence function" in {
      val s1 = Stream.constant("one")
      val s2 = Stream("one", "one")
      s1.hasSubsequence(s2)
    }

    "check startWith function" in {
      val s1 = Stream.constant("one")
      val s2 = Stream("one", "one")
      s1 startWith s2
    }

    "check tails function" in {
      val s1 = Stream("one", "two", "three")
      s1.tails.map(_.toList).toList shouldBe List(List("one", "two", "three"), List("two", "three"), List("three"))
    }

    "check scanRight function" in {
      val s1 = Stream(1, 2, 3)
      s1.scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
    }
  }
}
