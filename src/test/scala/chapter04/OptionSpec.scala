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

    "apply getOrElse to Some('username')" should {
      "should return 'username'" in {
        val some: Option[String] = Some("admin")
        assert(some.getOrElse("anonymous") == "admin")
      }
    }

    "apply getOrElse to None" should {
      "should return default value" in {
        val none: Option[String] = None
        assert(none.getOrElse("anonymous") == "anonymous")
      }
    }

    "apply orElse to Some(13)" should {
      "should return Some(13)" in {
        val some: Option[Int] = Some(13)
        assert(some.orElse(Some(0)) == Some(13))
      }
    }

    "apply orElse to None" should {
      "should evaluate default value" in {
        val none: Option[Double] = None
        assert(none.getOrElse(Some(2d * 7)) == Some(14d))
      }
    }

    "apply filter to Some(13)" should {
      "should return Some(13) if it's not even number" in {
        val some: Option[Int] = Some(13)
        assert(some.filter(_ % 2 != 0) == Some(13))
      }
    }

    "apply filter to Some(Double.NegativeInfinity)" should {
      "should return None if it's not positive infinity" in {
        val some: Option[Double] = Some(Double.NegativeInfinity)
        assert(some.filter(_.isPosInfinity) == None)
      }
    }

    "apply filter to None" should {
      "should return None" in {
        val none: Option[Double] = None
        assert(none.filter(_.isNaN) == None)
      }
    }
  }
}
