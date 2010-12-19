package expr

import Math.pow

object Evaluator {
  def eval(expression: Expr): Double = eval(expression, Map())

  def eval(expression: Expr, env: Map[String, Double]): Double = {
    def e(expr: Expr) = eval(expr, env)

    expression match {
      case Num(x) => x
      case Name(x) => env(x)
      case BinOp("+", x, y) => e(x) + e(y)
      case BinOp("*", x, y) => e(x) * e(y)
      case BinOp("**", x, y) => pow(e(x), e(y))
      case _ => error("Cannot evaluate expression: " + expression)
    }
  }
}
