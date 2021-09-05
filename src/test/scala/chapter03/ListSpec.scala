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

      list.setHead(7) shouldBe List(7, 2, 3)
    }

    "set first element for empty list with setHead" in {
      val list: List[Int] = Nil

      list.setHead("Hello") shouldBe List("Hello")
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

    "return list without last element when call .init for non-empty list" in {
      val list = List(1, 2, 3, 4, 5)

      list.init shouldBe List(1, 2, 3, 4)
      list.init.init shouldBe List(1, 2, 3)
    }

    "throw UnsupportedOperationException when call .init for empty list" in {
      val list: List[Int] = Nil

      assertThrows[UnsupportedOperationException] {
        list.tail
        List(1).tail.tail
      }
    }

    "return sum and concatenate with foldRight" in {
      val listInts = List(1, 2, 3, 4, 5)
      val listStrings = List("hello", "darkness", "my", "old", "friend")

      listInts.foldRight(0)(_ + _) shouldBe 15
      listStrings.foldRight("")(_ + _) shouldBe "hellodarknessmyoldfriend"
    }

    "return product for List[Double] when call productFoldRight" in {
      val list: List[Double] = List(1, 2, 3, 4, 5)
      val listWithZero: List[Double] = List(1, 2, 0, 4, 5)

      List.productFoldRight(list) shouldBe 120.0
      List.productFoldRight(listWithZero) shouldBe 0.0
    }

    "return same list when pass as init Nil and Cons as function foldRight" in {
      val list = List(1, 2, 3)

      list.foldRight(Nil: List[Int])(Cons(_, _)) shouldBe list
    }

    "return length of list" in {
      val list = List(1, 2, 3, 4, 5)
      val emptyList: List[Int] = Nil

      list.length shouldBe 5
      emptyList.length shouldBe 0
    }

    "return sum and concatenate with foldLeft" in {
      val listInts = List(1, 2, 3, 4, 5)
      val listStrings = List("hello", "darkness", "my", "old", "friend")

      listInts.foldRight(0)(_ + _) shouldBe 15
      listStrings.foldRight("")(_ + _) shouldBe "hellodarknessmyoldfriend"
    }

    "return reversed list" in {
      val list = List(1, 2, 3)

      list.reverse shouldBe List(3, 2, 1)
    }

    "return new List[Int] with incremented each element by 1" in {
      val list = List(1, 2, 3)
      val emptyList: List[Int] = Nil

      List.addOne(list) shouldBe List(2, 3, 4)
      List.addOne(emptyList) shouldBe Nil
    }

    "return new List[String] from List[Double]" in {
      val list = List(1.0, 2.0, 3.0)
      val emptyList: List[Double] = Nil

      List.double2String(list) shouldBe List("1.0", "2.0", "3.0")
      List.double2String(emptyList) shouldBe Nil
    }

    "return new List[Int] with incremented each element by 1 with using map function" in {
      val list = List(1, 2, 3)
      val emptyList: List[Int] = Nil

      list.map(_ + 1) shouldBe List(2, 3, 4)
      emptyList.map(_ + 1) shouldBe Nil
    }

    "return filtered list with only even numbers" in {
      val list = List(1, 2, 3, 4, 5)
      val emptyList: List[Int] = Nil

      list.filter(_ % 2 == 0) shouldBe List(1, 3, 5)
      emptyList.filter(_ % 2 == 0) shouldBe Nil
    }
  }
}
