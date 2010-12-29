package expr

import Math.pow
import BinOp._

object Evaluation {
  def eval(expression: Expr, env: Env): Double = {
    def e(expr: Expr) = eval(expr, env)

    expression match {
      case Num(x) => x
      case Name(x) => env.variable(x)
      case Call(name, args) => env.function(name).eval(env, args.map(e))
      case x + y => e(x) + e(y)
      case x - y => e(x) - e(y)
      case x * y => e(x) * e(y)
      case x / y => e(x) / e(y)
      case x ^ y => pow(e(x), e(y))
      case _ => error("Cannot evaluate expression: " + expression)
    }
  }
}
