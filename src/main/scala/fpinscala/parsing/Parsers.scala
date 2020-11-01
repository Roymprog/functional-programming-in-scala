package fpinscala.parsing

import fpinscala.generative_testing.Prop.forAll
import fpinscala.generative_testing.{Gen, Prop}

import scala.annotation.tailrec
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] { self =>
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def unit[A](a: A): Parser[A]
  def succeed[A](a: A): Parser[A] =
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

  def unbiasLeft[A,B,C](p: Parser[((A, B), C)]): Parser[(A,B,C)] =
    p.map(t => (t._1._1, t._1._2, t._2))
  def unbiasRight[A,B,C](p: Parser[(A, (B, C))]): Parser[(A,B,C)] =
    p.map(t => (t._1, t._2._1, t._2._2))

  // 9.1 Define a method map2 using product to combine two parsers
  def map2[A,B,C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    product(p1, p2).map(f.tupled)
  def map3[A,B,C,D](p1: Parser[A], p2: Parser[B], p3: Parser[C])(f: (A, B, C) => D): Parser[D] =
    unbiasLeft(product(product(p1, p2), p3)).map(f.tupled)
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  val numA: Parser[Int] = char('a').many.slice.map(_.length)
  val numAsBs: Parser[(Int, Int)] =
    char('a').many.slice.map(_.length) **
      char('b').many1.slice.map(_.length)
  // 9.6 Use flatMap to write a context-sensensitive parser that can parse a number, n, followed
  // by n times the character for example "0" and "4aaaa"
  val nNumAs: Parser[String] =
  digit
    .flatMap(n => listOfN(char('a'), n.toInt))
    .map(_.mkString)

  def whitespace: Parser[String] = """\s""".r
  def digit: Parser[String] = """\d""".r
  def integer: Parser[String] = digit.many1.slice
  def double: Parser[String] = map3(integer, string("."), integer)(_ + _ + _)
  // Exponential numbers can be optimized by adding some way to add an optional suffix
  def expInteger: Parser[String] = map3(integer, """[eE]""".r, integer)(_+_+_)
  def expDouble: Parser[String] = map3(double, """[eE]""".r, integer)(_+_+_)
  // Will parse integers and doubles
  def plainNumber: Parser[String] =
    integer or double
  def expNumber: Parser[String] =
    expInteger or expDouble
  // Number does not support negative numbers yet
  def number: Parser[String] =
    plainNumber or expNumber

  def ignoreWhiteSpace[A](p1: Parser[A]): Parser[A] = map3(whitespace, p1, whitespace)((_, a, _) => a)

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
    def map2[B,C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p1, p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p1)(f)
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
  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._

    val openBrace = string("{")
    val closeBrace = string("}")
    val openBracket = ignoreWhiteSpace(string("["))
    val closeBracket = ignoreWhiteSpace(string("]"))
    val comma = ignoreWhiteSpace(string(","))
    def boolean: Parser[JBool] = string("true").map(_ => JBool(true)) or string("false").map(_ => JBool(false))
    def nill: Parser[JNull.type] = string("null").map(_ => JNull)
    def value: Parser[JSON] =
      nill or boolean or stringLiteral or nr or jarray or jobject
    def stringLiteral: Parser[JString] =
      map3(string("\""), """\w""".r, string("\""))((_, s, _) => JString(s))
    def nr: Parser[JSON] =
      number.map(s => JNumber(s.toDouble))
    def element: Parser[JSON] =
      ignoreWhiteSpace(value)
    def elements: Parser[JArray] =
      map2(element, map2(comma, elements)((_, j) => j))((a,l) => JArray(a :: l.get))
    def jarray: Parser[JArray] =
      product(openBracket, closeBracket).map(_ => JArray(List.empty)) or
        map3(openBracket, elements, closeBracket)((_, l, _) => l)
    def member: Parser[(String, JSON)] =
      map3(ignoreWhiteSpace(stringLiteral), string(":"), element)((a, _, c) => (a.get, c))
    def members: Parser[JObject] =
      member.map2(comma.map2(members)((_, j) => j))((a,l) => JObject(l.get + a))
    def jobject: Parser[JSON] =
      map3(openBrace, ignoreWhiteSpace(string("")), closeBrace)((_, b, _) => b).map(_ => JObject(Map.empty)) or
      map3(openBrace, members, closeBrace)((_, b, _) => b)

    ignoreWhiteSpace(value)
  }

  sealed trait JSON
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JString(get: String) extends JSON
  case class JArray(get: List[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}