package chapter04

import org.scalatest.WordSpec

class EitherSpec extends WordSpec {
  import chapter04.either._

  "A Either" when {
    "apply map with x2 function to Right(Int)" should {
      "should return Right with value multiple by 2" in {
        val r: Either[String, Int] = Right(7)
        assert(r.map(_ * 2) == Right(14))
      }
    }

    "apply map with x3 function to Left[String]" should {
      "should return same Left[String]" in {
        val r: Either[String, Int] = Left("Something went wrong...")
        assert(r.map(_ * 3) == Left("Something went wrong..."))
      }
    }

    "apply flatMap to transform Either[String, AnyRef] into Either[String, Int]" should {
      "should return Right(5) if success" in {
        val res: Either[String, AnyRef] = Right("Scala")
        assert(res.flatMap(tryStringLength) == Right(5))
      }
    }

    "apply flatMap to transform Either[String, AnyRef] into Either[String, Int]" should {
      "should return Left if failure" in {
        case class Person(name: String)
        val person = Person("Nobody")
        val res: Either[String, AnyRef] = Right(person)
        assert(res.flatMap(tryStringLength) == Left("Person(Nobody) is not a String :("))
      }
    }

    "apply orElse(Right(17)) to Either[String, Int]" should {
      "should return Right(13) if it's Right" in {
        val res: Either[String, Int] = Right(13)
        assert(res.orElse(Right(17)) == Right(13))
      }
    }

    "apply orElse(Right(17)) to Either[String, Int]" should {
      "should return Right(17) if it's Left" in {
        val res: Either[String, Int] = Left("Something went wrong...")
        assert(res.orElse(Right(17)) == Right(17))
      }
    }

    "apply map2 for two Either (2 Right) and division first by second" should {
      "should return Right(2.6) if success" in {
        val first: Either[String, Int] = Right(13)
        val second: Either[String, String] = Right("Scala")
        assert(first.map2(second)(_ / _.length.toDouble) == Right(13 / 5.0))
      }
    }

    "apply map2 for two Either (Right/Left) and division first by second" should {
      "should return Left if failure" in {
        val first: Either[String, Int] = Right(13)
        val second: Either[String, Int] = Left("Something went wrong...")
        assert(first.map2(second)(_ / _) == Left("Something went wrong..."))
      }
    }

    "apply map2 for two Either(2 Left) and division first by second" should {
      "should return Left if failure" in {
        val first: Either[String, Int] = Left("Something went wrong...")
        val second: Either[String, Int] = Left("Something went wrong...")
        assert(first.map2(second)(_ / _) == Left("Something went wrong..."))
      }
    }

    "apply sequence for List[Either[String, Int]] without Left" should {
      "should return Either[String, List[Int]]" in {
        val list: List[Either[String, Int]] = List(Right(1), Right(2), Right(3))
        assert(Either.sequence(list) == Right(List(1, 2, 3)))
      }
    }

    "apply sequence for List[Either[String, Int]] with Left" should {
      "should return Left[String, List[Int]]" in {
        val list: List[Either[String, Int]] = List(Right(1), Left("Something went wrong..."), Right(3))
        assert(Either.sequence(list) == Left("Something went wrong..."))
      }
    }

    "apply traverse by List[String] with method tryStringLength" should {
      "should return Right[String, List[Int]]" in {
        val list: List[String] = List("hello", "darkness", "my", "old", "friend")
        assert(Either.traverse(list)(tryStringLength) == Right(List(5, 8, 2, 3, 6)))
      }
    }

    "apply traverse by List[String] with method tryStringLength" should {
      "should return Left[String, List[Int]]" in {
        case class Person(name: String)
        val list: List[AnyRef] = List("hello", "darkness", "my", Person("Andrew"), "friend")
        assert(Either.traverse(list)(tryStringLength) == Left("Person(Andrew) is not a String :("))
      }
    }
  }

  def tryStringLength(any: AnyRef): Either[String, Int] = any match {
    case s: String => Right(s.length)
    case _ => Left(s"$any is not a String :(")
  }
}
