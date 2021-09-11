package chapter04

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EitherSpec extends AnyWordSpec with Matchers {
  import chapter04.either._

  "A Either" when {
    "apply map with x2 function to Right(Int)" should {
      "should return Right with value multiple by 2" in {
        val r: Either[String, Int] = Right(7)
        r.map(_ * 2) shouldBe Right(14)
      }
    }

    "apply map with x3 function to Left[String]" should {
      "should return same Left[String]" in {
        val r: Either[String, Int] = Left("Something went wrong...")
        r.map(_ * 3) shouldBe Left("Something went wrong...")
      }
    }

    "apply flatMap to transform Either[String, AnyRef] into Either[String, Int]" should {
      "should return Right(5) if success" in {
        val res: Either[String, AnyRef] = Right("Scala")
        res.flatMap(tryStringLength) shouldBe Right(5)
      }
    }

    "apply flatMap to transform Either[String, AnyRef] into Either[String, Int]" should {
      "should return Left if failure" in {
        case class Person(name: String)
        val person = Person("Nobody")
        val res: Either[String, AnyRef] = Right(person)
        res.flatMap(tryStringLength) shouldBe Left("Person(Nobody) is not a String :(")
      }
    }

    "apply orElse(Right(17)) to Either[String, Int]" should {
      "should return Right(13) if it's Right" in {
        val res: Either[String, Int] = Right(13)
        res.orElse(Right(17)) shouldBe Right(13)
      }
    }

    "apply orElse(Right(17)) to Either[String, Int]" should {
      "should return Right(17) if it's Left" in {
        val res: Either[String, Int] = Left("Something went wrong...")
        res.orElse(Right(17)) shouldBe Right(17)
      }
    }

    "apply map2 for two Either (2 Right) and division first by second" should {
      "should return Right(2.6) if success" in {
        val first: Either[String, Int] = Right(13)
        val second: Either[String, String] = Right("Scala")
        first.map2(second)(_ / _.length.toDouble) shouldBe Right(13 / 5.0)
      }
    }

    "apply map2 for two Either (Right/Left) and division first by second" should {
      "should return Left if failure" in {
        val first: Either[String, Int] = Right(13)
        val second: Either[String, Int] = Left("Something went wrong...")
        first.map2(second)(_ / _) shouldBe Left("Something went wrong...")
      }
    }

    "apply map2 for two Either(2 Left) and division first by second" should {
      "should return Left if failure" in {
        val first: Either[String, Int] = Left("Something went wrong...")
        val second: Either[String, Int] = Left("Something went wrong...")
        first.map2(second)(_ / _) shouldBe Left("Something went wrong...")
      }
    }

    "apply sequence for List[Either[String, Int]] without Left" should {
      "should return Either[String, List[Int]]" in {
        val list: List[Either[String, Int]] = List(Right(1), Right(2), Right(3))
        Either.sequence(list) shouldBe Right(List(1, 2, 3))
      }
    }

    "apply sequence for List[Either[String, Int]] with Left" should {
      "should return Left[String, List[Int]]" in {
        val list: List[Either[String, Int]] = List(Right(1), Left("Something went wrong..."), Right(3))
        Either.sequence(list) shouldBe Left("Something went wrong...")
      }
    }

    "apply traverse by List[String] with method tryStringLength" should {
      "should return Right[String, List[Int]]" in {
        val list: List[String] = List("hello", "darkness", "my", "old", "friend")
        Either.traverse(list)(tryStringLength) shouldBe Right(List(5, 8, 2, 3, 6))
      }
    }

    "apply traverse by List[String] with method tryStringLength" should {
      "should return Left[String, List[Int]]" in {
        case class Person(name: String)
        val list: List[AnyRef] = List("hello", "darkness", "my", Person("Andrew"), "friend")
        Either.traverse(list)(tryStringLength) shouldBe Left("Person(Andrew) is not a String :(")
      }
    }
  }

  def tryStringLength(any: AnyRef): Either[String, Int] =
    any match {
      case s: String => Right(s.length)
      case _         => Left(s"$any is not a String :(")
    }
}
