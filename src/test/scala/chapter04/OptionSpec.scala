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

    "apply map2 and sum on two Some(Int)" should {
      "return Some(sum of two Int)" in {
        val a: Option[Int] = Some(7)
        val b: Option[Int] = Some(30)

        assert(Option.map2(a, b)(_ + _) == Some(37))
      }
    }

    "apply map2 and sum on Some(Int) and None" should {
      "return None" in {
        val a: Option[Int] = Some(7)
        val b: Option[Int] = None

        assert(Option.map2(a, b)(_ + _) == None)
      }
    }

    "apply map2 and sum on two None" should {
      "return None" in {
        val a: Option[Int] = None
        val b: Option[Int] = None

        assert(Option.map2(a, b)(_ + _) == None)
      }
    }

    "apply sequence to List[Option[String]]" should {
      "should return Option[List[String]]" in {
        val listOfOption: List[Option[String]] = List(Some("one"), Some("two"), Some("three"))
        assert(Option.sequence(listOfOption) == Some(List("one", "two", "three")))
      }
    }

    "apply sequence to List[Option[String]] that contains None" should {
      "should return None" in {
        val listOfOption: List[Option[String]] = List(Some("one"), None, Some("three"))
        assert(Option.sequence(listOfOption) == None)
      }
    }

    "apply sequence to List[None]" should {
      "should return None" in {
        val listOfOption: List[Option[String]] = List(None)
        assert(Option.sequence(listOfOption) == None)
      }
    }

    "apply traverse to List[String] with function that parse String to Int" should {
      "should return Option[List[Int]] if all OK" in {
        val list: List[String] = List("1", "21", "121", "73")
        assert(Option.traverse(list)(parseToInt) == Some(List(1, 21, 121, 73)))
      }
    }

    "apply traverse to List[String] with function that parse String to Int" should {
      "should return None if List contains wrong String" in {
        val list: List[String] = List("1", "21a", "73")
        assert(Option.traverse(list)(parseToInt) == None)
      }
    }

    "apply traverse to empty List[String] with function that parse String to Int" should {
      "should return Option[List.empty[Int]]" in {
        val list: List[String] = List()
        assert(Option.traverse(list)(parseToInt) == Some(List.empty[Int]))
      }
    }
  }

  private def parseToInt(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch { case e: NumberFormatException => None}
}
