package fpinscala.parsing

import fpinscala.generative_testing.Prop.forAll
import fpinscala.generative_testing.{Gen, Prop}

trait Parsers[ParseError <: Throwable, Parser[+_]] { self =>
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def unit[A](a: A): Parser[A]
  def succeed[A](a: A) =
    string("") map (_ => a)
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def listOfN[A](p: Parser[A], n: Int): Parser[List[A]] =
    sequence(List.fill(n)(p))

  // 9.1 define many with map2
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())
  // 9.1 define many1 with many
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)
  def slice[A](p: Parser[A]): Parser[String]
  def map[A, B](p: Parser[A])(f: A => B): Parser[B]
  // Combines two parsers assuming the first is successful
  def product[A,B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)]
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def sequence[A](ps: List[Parser[A]]): Parser[List[A]] =
    ps.foldLeft(unit(List.empty): Parser[List[A]])((as, pa) => map2(pa, as)(_ :: _))

  def reduce[A, B](p: Parser[List[A]])(f: (A, A) => B): Parser[B] =
    p.map(_.reduce(f))
  // 9.1 Define a method map2 using product to combine two parsers
  def map2[A,B,C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    product(p1, p2).map(f.tupled)
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  val numA: Parser[Int] = char('a').many.slice.map(_.size)
  val numAsBs: Parser[(Int, Int)] =
    char('a').many.slice.map(_.size) **
      char('b').many1.slice.map(_.size)

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  case class ParserOps[A](p1: Parser[A]) {
    def |[B>:A](p2:Parser[B]): Parser[B] = self.or(p1, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p1, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p1, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p1, p2)
    def many: Parser[List[A]] = self.many(p1)
    def many1: Parser[List[A]] = self.many1(p1)
    def slice: Parser[String] = self.slice(p1)
    def map[B](f: A => B): Parser[B] = self.map(p1)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p1)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)
    def succeedLaw[A](in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(s))(s) == Right(s))
  }
}

