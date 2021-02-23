package chapter02

import chapter02.PolymorphicFunctions.isSorted
import org.scalatest.wordspec.AnyWordSpec

class PolymorphicFunctionsSpec extends AnyWordSpec {
  "a isSorted" when {
    "called with Array(1, 2, 3, 4, 5)" should {
      "return true" in {
        assertResult(true)(isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => a < b))
      }
    }

    "called with Array(1, 4, 2, 3, 5)" should {
      "return false" in {
        assertResult(false)(isSorted(Array(1, 4, 2, 3, 5), (a: Int, b: Int) => a > b))
      }
    }

    "called with Array(7, 3, 8)" should {
      "return false" in {
        assertResult(false)(isSorted(Array(7, 3, 8), (a: Int, b: Int) => a < b))
      }
    }

    "called with Array(5, 4, 3, 2, 1)" should {
      "return false" in {
        assertResult(false)(isSorted(Array(5, 4, 3, 2, 1), (a: Int, b: Int) => a < b))
      }
    }
  }
}
