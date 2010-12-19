package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class EvaluatorSpec extends Spec with ShouldMatchers {
  val env = Map[String, Double]("X" -> 1, "Y" -> 2)

  val examples = Array(
    "1" -> 1
    , "2 + 3" -> 5
    , "X + Y" -> 3
    , "Y ^ (X * 4)" -> 16
    , "4 - Y" -> 2
  )

  for((input, expectation) <- examples) {
    it("evaluates " + input + " to " + expectation) {
      val ast = Parser.parse(input)
      val result = Evaluator.eval(ast, env)
      expect(expectation) { result }
    }
  }
}
