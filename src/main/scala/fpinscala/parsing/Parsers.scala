package fpinscala.parsing

import fpinscala.generative_testing.Prop.forAll
import fpinscala.generative_testing.{Gen, Prop}

import scala.annotation.tailrec
import scala.util.matching.Regex

trait Parsers[ParseError <: Throwable, Parser[+_]] { self =>
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def fork[A](p: => Parser[A]): Parser[A] = p
  def unit[A](a: A): Parser[A]
  def succeed[A](a: A) =
    string("") map (_ => a)
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def listOfN[A](p: Parser[A], n: Int): Parser[List[A]] = {
    @tailrec
    def go(p: Parser[A], n: Int, acc: Parser[List[A]]): Parser[List[A]] =
      if (n <= 0) acc
      else go(p, n - 1, map2(acc, p)((list, a) => a :: list))

    go(p, n, succeed(List.empty))
  }

  // 9.1 define many with map2
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())
  // 9.1 define many1 with many
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)
  def slice[A](p: Parser[A]): Parser[String]
  // 9.8 Implement map using flatMap
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))
  // Combines two parsers assuming the first is successful
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p1.flatMap(a => p2.map(b => (a, b)))

  // 9.7 Implement map2 and product using flatMap
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def sequence[A](ps: List[Parser[A]]): Parser[List[A]] =
    ps.foldLeft(unit(List.empty): Parser[List[A]])((as, pa) => map2(pa, as)(_ :: _))

  def reduce[A, B](p: Parser[List[A]])(f: (A, A) => B): Parser[B] =
    p.map(_.reduce(f))
  // 9.1 Define a method map2 using product to combine two parsers
  def map2[A,B,C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    product(p1, p2).map(f.tupled)
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  val numA: Parser[Int] = char('a').many.slice.map(_.size)
  val numAsBs: Parser[(Int, Int)] =
    char('a').many.slice.map(_.size) **
      char('b').many1.slice.map(_.size)
  // 9.6 Use flatMap to write a context-sensensitive parser that can parse a number, n, followed
  // by n times the character for example "0" and "4aaaa"
  val nNumAs: Parser[String] =
  """\d""".r
    .flatMap(n => listOfN(char('a'), n.toInt))
    .map(_.mkString)

  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
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
    def fork: Parser[A] = self.fork(p1)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)
    def succeedLaw[A](in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(s))(s) == Right(s))
    def productAssociativityLaw[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C])(in: Gen[String]): Prop =
      equal(product(p1, product(p2, p3)).map(t => (t._1, t._2._1, t._2._2)),
        product(product(p1, p2), p3).map(t => (t._1, t._1._2, t._2)))(in)
    def productMapLaw[A, B](p1: Parser[A], p2: Parser[A])(f: A => B)(in: Gen[String]): Prop =
      equal(p1.map(f) ** p2.map(f), p1 ** p2 map {case (a, b) => (f(a), f(b))})(in)
  }
}

object Parsers {
//  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
//    import P._
//    val spaces = char(' ').many.slice
//  }

  sealed trait JSON
  case object JNull extends JSON
  case class JDouble(get: Double) extends JSON
  case class JInt(get: Int) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: List[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}