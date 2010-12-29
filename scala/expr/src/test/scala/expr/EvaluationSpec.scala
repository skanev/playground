package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

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
      val ast = Parser.parse(input)
      val result = ast.eval(env)
      expect(expectation) { result }
    }

    it("can concurrently evaluate " + input + " to " + expectation) {
      val ast = Parser.parse(input)
      val result = ast.ceval(env)
      expect(expectation) { result }
    }
  }
}
