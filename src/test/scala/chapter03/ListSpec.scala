package chapter03

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ListSpec extends AnyWordSpec with Matchers {
  "List" should {
    import chapter03.datastructures._

    "return all element except first when call .tail for non empty list" in {
      val list = List(1, 2, 3, 4)

      list.tail shouldBe List(2, 3, 4)
    }

    "throw UnsupportedOperationException when call .tail for empty list" in {
      val list: List[Int] = Nil

      assertThrows[UnsupportedOperationException] {
        list.tail
      }
    }

    "replace first element of non-empty list with setHead" in {
      val list = List(1, 2, 3)

      List.setHead(list, 7) shouldBe List(7, 2, 3)
    }

    "set first element for empty list with setHead" in {
      val list: List[Int] = Nil

      List.setHead(list, 7) shouldBe List(7)
    }

    "remove first n element for non-empty list with .drop(n)" in {
      val list = List(1, 2, 3, 4, 5)

      list.drop(3) shouldBe List(4, 5)
    }

    "return same list for if n =< 0 with .drop call" in {
      val list = List(1, 2, 3, 4, 5)

      list.drop(0) shouldBe list
      list.drop(-3) shouldBe list
    }

    "return empty list for any .drop call" in {
      val list: List[Int] = Nil

      list.drop(3) shouldBe Nil
      list.drop(-3) shouldBe Nil
      list.drop(0) shouldBe Nil
    }

    "remove first n element for non-empty list while predicate return true with .dropWhile" in {
      val list = List(1, 2, 3, 4, 5)

      list.dropWhile(_ < 4) shouldBe List(4, 5)
    }

    "return empty list for any .dropWhile call" in {
      val list: List[Int] = Nil

      list.dropWhile(_ > 3) shouldBe Nil
      list.dropWhile(_ => true) shouldBe Nil
    }

    "return list that appended l1 to l2" in {
      val nonEmptyOne = List(1, 2, 3)
      val nonEmptyTwo = List(3, 4, 5)
      val emptyOne: List[Int] = Nil
      val emptyTwo: List[Int] = Nil

      nonEmptyOne.append(nonEmptyTwo) shouldBe List(1, 2, 3, 3, 4, 5)
      nonEmptyOne.append(emptyTwo) shouldBe nonEmptyOne
      emptyOne.append(nonEmptyTwo) shouldBe nonEmptyTwo
      emptyOne.append(emptyTwo) shouldBe Nil
    }
  }
}
