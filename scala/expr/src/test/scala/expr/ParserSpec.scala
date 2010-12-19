package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import Parser.parse

class ParserSpec extends Spec with ShouldMatchers {
  val examples = Array(
    "1" -> Num(1)
    , "X" -> Name("X")
    , "1 + X" -> BinOp("+", Num(1), Name("X"))
    , "X * 2" -> BinOp("*", Name("X"), Num(2))
    , "2 ** X" -> BinOp("**", Num(2), Name("X"))
    , "1 ** 2 + 3 * 4" -> BinOp("+", BinOp("**", Num(1), Num(2)), BinOp("*", Num(3), Num(4)))
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
