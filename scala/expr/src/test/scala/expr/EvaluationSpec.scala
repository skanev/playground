package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import Expr.parse

class EvaluationSpec extends Spec with ShouldMatchers {
  val add = Lambda(List("X", "Y"), Name("X") + Name("Y"))
  val twice = Lambda(List("X"), Call("add", List(Name("X"), Name("X"))))
  val negate = ScalaCode.define1("negate") { x => -x }

  val env = Env.empty
    .extend("X", 1)
    .extend("Y", 2)
    .extend("add", add)
    .extend("twice", twice)
    .extend("negate", negate)

  val examples = Array(
    "1" -> 1
    , "2 + 3" -> 5
    , "X + Y" -> 3
    , "Y ^ (X * 4)" -> 16
    , "4 - Y" -> 2
    , "9 / 3" -> 3
    , "add(1, 2)" -> 3
    , "twice(2)" -> 4
    , "negate(42)" -> -42
  )

  for((input, expectation) <- examples) {
    it("evaluates " + input + " to " + expectation) {
      expect(expectation) { parse(input).eval(env) }
    }

    it("can evaluate with actors " + input + " to " + expectation) {
      expect(expectation) { parse(input).aeval(env) }
    }
  }
}
