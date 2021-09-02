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
  }
}
