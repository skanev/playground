package expr

import Math.pow
import Operator._

object Evaluator {
  def eval(expression: Expr): Double = eval(expression, Map())

  def eval(expression: Expr, env: Map[String, Double]): Double = {
    def e(expr: Expr) = eval(expr, env)

    expression match {
      case Num(x) => x
      case Name(x) => env(x)
      case BinOp(Add, x, y) => e(x) + e(y)
      case BinOp(Mul, x, y) => e(x) * e(y)
      case BinOp(Pow, x, y) => pow(e(x), e(y))
      case _ => error("Cannot evaluate expression: " + expression)
    }
  }
}
