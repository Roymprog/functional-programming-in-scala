package fpinscala.parsing

import scala.util.matching.Regex

object InitialParsers extends Parsers[InitialParser] {
  override def slice[A](p: InitialParser[A]): InitialParser[String] =
    InitialParser(s => p.parse(s).map(t => (t._1, t._1)))

  override def flatMap[A, B](p: InitialParser[A])(f: A => InitialParser[B]): InitialParser[B] =
    InitialParser(s => {
      for {
        res1 <- p.parse(s)
        res2 <- f(res1._2).parse(s.slice(p.offSet + res1._1.length, s.length))
      } yield (res1._1 + res2._1, res2._2)
      //      p.parse(s).flatMap {
      //        t => f(t._2)
      //          .parse(s.slice(p.offSet + t._1.length, s.length))
      //          .map(res => (s + res._1, res._2))
      //      }
    })

  override def or[A](p1: InitialParser[A], p2: => InitialParser[A]): InitialParser[A] =
    InitialParser(s => {
      p1.parse(s) match {
        case Left((committed, pe)) =>
          if (committed) Left((committed, pe))
          else p2.parse(s)
        case Right(r) => Right(r)
      }
    })


  override implicit def string(s: String): InitialParser[String] =
    InitialParser((input: String) => {
      val error = (index: Int) => ParseError(List((Location(input, index), s"Expected $s, but got $input")))

      val zipped = input.zip(s)
      if (zipped.length < input.length) Left((true, error(zipped.length)))
      else zipped.indexWhere(chars => chars._1 != chars._2) match {
        case -1 => Right((s,s))
        case 0 => Left((false, error(0)))
        case index => Left((true, error(index)))
      }
    })

  override implicit def regex(r: Regex): InitialParser[String] =
    InitialParser((input: String) => {
      val error = ParseError(List((Location(input, 0), s"Expected match with regex ${r.toString()}, but got $input")))
      r.findPrefixOf(input)
        .map(s => Right((s, s)))
        .getOrElse(Left((false, error)))
    })

  override def label[A](msg: String)(p: InitialParser[A]): InitialParser[A] =
    InitialParser(s => {
      p.parse(s) match {
        case Left((committed, ParseError((location, _) :: tail))) => Left((committed, ParseError((location, msg) :: tail)))
        case Right(r) => Right(r)
      }
    }, p.offSet)

  override def scope[A](msg: String)(p: InitialParser[A]): InitialParser[A] =
    InitialParser(s => {
      p.parse(s) match {
        case Left((committed, ParseError((location, err) :: tail))) => Left((committed, ParseError((location, msg) :: (location, err) :: tail)))
        case Right(r) => Right(r)
      }
    }, p.offSet)

  override def attempt[A](p: InitialParser[A]): InitialParser[A] =
    InitialParser(s => {
      p.parse(s) match {
        case Left((_, pe)) => Left((true, pe))
        case Right(r) => Right(r)
      }
    })

  override def run[A](p: InitialParser[A])(input: String): Either[ParseError, A] =
    p.parse(input) match {
      case Left((_, pe)) => Left(pe)
      case Right((s, a)) =>
        if (a == input) Left(ParseError(List((Location(input, s.length), "Could not parse entire input"))))
        else Right(a)
    }

  override def errorMessage(p: ParseError): String = p.stack.head._2

  override def errorLocation(p: ParseError): Location = p.stack.head._1

  override def succeed[A](a: A): InitialParser[A] = InitialParser(s => Right((s,a)))
}

// How do I know if a parser is committed on failure or not?
// regex parser cannot return the amount of characters parsed.
// How do I keep track of the offset when parsing?
case class InitialParser[+A](
                             parse: String => Either[(Boolean, ParseError), (String, A)],
                             offSet: Int = 0,
                           )