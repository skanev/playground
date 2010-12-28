package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import Parser.parse
import BinOp._
import expr.{BinOp => O}

class ParserSpec extends Spec with ShouldMatchers {
  val examples = Array(
    "1" -> Num(1)
    , "X" -> Name("X")
    , "1 + X" -> BinOp(O.+, Num(1), Name("X"))
    , "1 - 2" -> BinOp(O.-, Num(1), Num(2))
    , "X * 2" -> BinOp(*, Name("X"), Num(2))
    , "X / Y" -> BinOp(/, Name("X"), Name("Y"))
    , "2 ^ X" -> BinOp(^, Num(2), Name("X"))
    , "1 + 2 + 3" -> BinOp(O.+, BinOp(O.+, Num(1), Num(2)), Num(3))
    , "1 + 2 + 3 + 4" -> BinOp(O.+, BinOp(O.+, BinOp(O.+, Num(1), Num(2)), Num(3)), Num(4))
    , "1 - 2 + 3" -> BinOp(O.+, BinOp(O.-, Num(1), Num(2)), Num(3))
    , "1 + 2 - 3" -> BinOp(O.+, Num(1), BinOp(O.-, Num(2), Num(3)))
    , "1 ^ 2 + 3 * 4" -> BinOp(O.+, BinOp(^, Num(1), Num(2)), BinOp(*, Num(3), Num(4)))
    , "2 / 3 ^ 4" -> BinOp(O./, Num(2), BinOp(O.^, Num(3), Num(4)))
    , "2 ^ 3 / 4" -> BinOp(O./, BinOp(O.^, Num(2), Num(3)), Num(4))
    , "foo(1, 2)" -> Call("foo", List(Num(1), Num(2)))
    , "foo(X + Y)" -> Call("foo", List(BinOp(O.+, Name("X"), Name("Y"))))
    , "a(b(), 3 + c())" -> Call("a", List(Call("b", List()), BinOp(O.+, Num(3), Call("c", List()))))
  )

  for ((input, ast) <- examples) {
    it("parses " + input) {
      expect(ast) { parse(input) }
    }
  }

  it("throws an exception on invalid input") {
    val invalidInput = "+ + +"
    intercept[BadInputException] { parse(invalidInput) }
  }
}
