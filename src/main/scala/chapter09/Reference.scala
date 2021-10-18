package chapter09

import ReferenceTypes._

import scala.language.implicitConversions
import scala.util.matching.Regex

object ReferenceTypes {

  /** A parser is a kind of state action that can fail. */
  type Parser[+A] = Location => Result[A]

  trait Result[+A]
  final case class Success[+A](value: A, charsConsumed: Int) extends Result[A]
  final case class Failure[+A](error: ParseError) extends Result[Nothing]

  /** Returns -1 if s1.startsWith(s2), otherwise returns the
    * first index where the two strings differed. If s2 is
    * longer than s1, returns s1.length. */
  def firstNonMatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i + offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length - offset >= s2.length) -1
    else s1.length - offset
  }
}

object Reference extends Parsers[Parser] {
  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = ???

  override def slice[A](p: Parser[A]): Parser[String] =
    location =>
      p(location) match {
        case f @ Failure(_)            => f
        case Success(_, charsConsumed) => Success(???, charsConsumed) // location.slice(n)
      }

  // consume no characters and succeed with the given value
  override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def attempt[A](p: Parser[A]): Parser[A] = ???

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  override def regex(r: Regex): Parser[String] = {
    val msg = s"regex: $r"
    location =>
      r.findPrefixOf(location.input) match {
        case Some(value) => Success(value, value.length)
        case None        => Failure(???) // location.toError(msg), false
      }
  }

  override def string(s: String): Parser[String] = {
    val msg = s"'s'"
    location => {
      val i = firstNonMatchingIndex(location.input, s, location.offset)
      if (i == -1) Success(s, s.length)
      else Failure(???) // location.advanceBy(i).toError(msg), i != 0
    }
  }
}
