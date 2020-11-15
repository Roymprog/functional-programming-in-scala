package fpinscala.parsing

import org.scalatest.flatspec
import flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParsersTest extends AnyFlatSpec with Matchers {
  import Parsers._
  import Parsers.MyParsers._

  "Running parser" should "succeed for string" in {
    val p = string("abc")
    val result = MyParsers.run(p)("abc")

    result should be ('right)
    result.right.get should equal ("abc")
  }

  it should "succeed with mapping string parsers" in {
    val p = string("abc")
    val length = map(p)(_.length)
    val result = MyParsers.run(length)("abc")

    result should be ('right)
    result.right.get should equal (3)
  }

  it should "succeed for flatmapped of strings" in {
    val product = slice(flatMap(digit)(a => string( "a" * a.toInt)))
    val result = MyParsers.run(product)("4aaaa")

    result should be ('right)
    result.right.get should equal ("4aaaa")
  }

  it should "fail parser if not match entire input" in {
    val p = string("abc")
    val result = MyParsers.run(p)("abcd")

    result should be ('left)
  }

  it should "succeed wrapping a parser with whitespace" in {
    val p = ignoreWhiteSpace(string("a"))
    val result = MyParsers.run(p)(" a ")

    result should be ('right)
    result.right.get should equal ("a")
  }
}
