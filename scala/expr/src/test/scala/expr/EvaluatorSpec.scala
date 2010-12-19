package expr

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class EvaluatorSpec extends Spec with ShouldMatchers {
  def expectValueOf(expr: Expr)(expectation: => Double) {
    expect(Evaluator.eval(expr))(expectation)
  }

  def expectValueOf(expr: Expr, env: Map[String, Double])(expectation: => Double) {
    expect(Evaluator.eval(expr, env))(expectation)
  }

  it("evaluates simple addition") {
    val expr = BinOp("+", Num(1), Num(2))
    expectValueOf(expr) { 3 }
  }

  it("evaluates variables") {
    val env = Map("X" -> 2.0)
    val expr = BinOp("+", Name("X"), Num(1))
    expectValueOf(expr, env) { 3 }
  }
}
