package chapter04

import chapter04.option._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OptionSpec extends AnyWordSpec with Matchers {
  "An Option" when {
    "apply map that increase doubles Some(6)" should {
      "should return Some(12)" in {
        val some: Option[Int] = Some(6)
        some.map(_ * 2) shouldBe Some(12)
      }
    }

    "apply map to None" should {
      "should return None" in {
        val none: Option[Int] = None
        none.map(_ * 10) shouldBe None
      }
    }

    "apply flatMap to Some('username')" should {
      "should return Some('username')" in {
        val none: Option[String] = Some("username")
        none.flatMap(Some(_)) shouldBe Some("username")
      }
    }

    "apply flatMap to None" should {
      "should return None" in {
        val none: Option[String] = None
        none.flatMap(Some(_)) shouldBe None
      }
    }

    "apply getOrElse to Some('username')" should {
      "should return 'username'" in {
        val some: Option[String] = Some("admin")
        some.getOrElse("anonymous") shouldBe "admin"
      }
    }

    "apply getOrElse to None" should {
      "should return default value" in {
        val none: Option[String] = None
        none.getOrElse("anonymous") shouldBe "anonymous"
      }
    }

    "apply orElse to Some(13)" should {
      "should return Some(13)" in {
        val some: Option[Int] = Some(13)
        some.orElse(Some(0)) shouldBe Some(13)
      }
    }

    "apply orElse to None" should {
      "should evaluate default value" in {
        val none: Option[Double] = None
        none.getOrElse(Some(2d * 7)) shouldBe Some(14d)
      }
    }

    "apply filter to Some(13)" should {
      "should return Some(13) if it's not even number" in {
        val some: Option[Int] = Some(13)
        some.filter(_ % 2 != 0) shouldBe Some(13)
      }
    }

    "apply filter to Some(Double.NegativeInfinity)" should {
      "should return None if it's not positive infinity" in {
        val some: Option[Double] = Some(Double.NegativeInfinity)
        some.filter(_.isPosInfinity) shouldBe None
      }
    }

    "apply filter to None" should {
      "should return None" in {
        val none: Option[Double] = None
        none.filter(_.isNaN) shouldBe None
      }
    }

    "apply map2 and sum on two Some(Int)" should {
      "return Some(sum of two Int)" in {
        val a: Option[Int] = Some(7)
        val b: Option[Int] = Some(30)

        Option.map2(a, b)(_ + _) shouldBe Some(37)
      }
    }

    "apply map2 and sum on Some(Int) and None" should {
      "return None" in {
        val a: Option[Int] = Some(7)
        val b: Option[Int] = None

        Option.map2(a, b)(_ + _) shouldBe None
      }
    }

    "apply map2 and sum on two None" should {
      "return None" in {
        val a: Option[Int] = None
        val b: Option[Int] = None

        Option.map2(a, b)(_ + _) shouldBe None
      }
    }

    "apply sequence to List[Option[String]]" should {
      "should return Option[List[String]]" in {
        val listOfOption: List[Option[String]] = List(Some("one"), Some("two"), Some("three"))
        Option.sequence(listOfOption) shouldBe Some(List("one", "two", "three"))
      }
    }

    "apply sequence to List[Option[String]] that contains None" should {
      "should return None" in {
        val listOfOption: List[Option[String]] = List(Some("one"), None, Some("three"))
        Option.sequence(listOfOption) shouldBe None
      }
    }

    "apply sequence to List[None]" should {
      "should return None" in {
        val listOfOption: List[Option[String]] = List(None)
        Option.sequence(listOfOption) shouldBe None
      }
    }

    "apply traverse to List[String] with function that parse String to Int" should {
      "should return Option[List[Int]] if all OK" in {
        val list: List[String] = List("1", "21", "121", "73")
        Option.traverse(list)(parseToInt) shouldBe Some(List(1, 21, 121, 73))
      }
    }

    "apply traverse to List[String] with function that parse String to Int" should {
      "should return None if List contains wrong String" in {
        val list: List[String] = List("1", "21a", "73")
        Option.traverse(list)(parseToInt) shouldBe None
      }
    }

    "apply traverse to empty List[String] with function that parse String to Int" should {
      "should return Option[List.empty[Int]]" in {
        val list: List[String] = List()
        Option.traverse(list)(parseToInt) shouldBe Some(List.empty[Int])
      }
    }
  }

  private def parseToInt(s: String): Option[Int] =
    try {
      Some(s.toInt)
    } catch { case e: NumberFormatException => None }
}
