package chapter09

import chapter08.{Gen, Prop}

import scala.language.implicitConversions

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  def many[A](p: Parser[A]): Parser[List[A]]

  def manyOne[A](p: Parser[A]): Parser[List[A]]

  def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)]

  def slice[A](p: Parser[A]): Parser[String]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def map2[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(s => s))(in)
  }
}
