package chapter04

import org.scalatest.WordSpec
import chapter04.option._

class OptionSpec extends WordSpec {
  "An Option" when {
    "apply map that increase doubles Some(6)" should {
      "should return Some(12)" in {
        val some: Option[Int] = Some(6)
        assert(some.map(_ * 2) == Some(12))
      }
    }

    "apply map to None" should {
      "should return None" in {
        val none: Option[Int] = None
        assert(none.map(_ * 10) == None)
      }
    }

    "apply flatMap to Some('username')" should {
      "should return Some('username')" in {
        val none: Option[String] = Some("username")
        assert(none.flatMap(Some(_)) == Some("username"))
      }
    }

    "apply flatMap to None" should {
      "should return None" in {
        val none: Option[String] = None
        assert(none.flatMap(Some(_)) == None)
      }
    }
  }
}
