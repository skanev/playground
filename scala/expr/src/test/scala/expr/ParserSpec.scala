package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import Parser.parse
import Operator._

class ParserSpec extends Spec with ShouldMatchers {
  val examples = Array(
    "1" -> Num(1)
    , "X" -> Name("X")
    , "1 + X" -> BinOp(Add, Num(1), Name("X"))
    , "X * 2" -> BinOp(Mul, Name("X"), Num(2))
    , "2 ** X" -> BinOp(Pow, Num(2), Name("X"))
    , "1 ** 2 + 3 * 4" -> BinOp(Add, BinOp(Pow, Num(1), Num(2)), BinOp(Mul, Num(3), Num(4)))
  )

  for ((input, ast) <- examples) {
    it("parses " + input) {
      expect(parse(input)) { ast }
    }
  }

  it("throws an exception on invalid input") {
    intercept[BadInputException] { parse("+ + +") }
  }
}
