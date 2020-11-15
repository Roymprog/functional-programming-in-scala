import fpinscala.parsing.Parsers._
import MyParsers._

val p = string("abc")
val p2 = string("def")

run(p)("abcd")
run(p or p2)("def")
run(p ** p2)("abcdefg")
