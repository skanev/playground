package expr

import scala.actors.Futures.future
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

  def ceval(expression: Expr, env: Env): Double = {
    def e(expr: Expr) = ceval(expr, env)

    def parallelBinOp(leftExpr: Expr, rightExpr: Expr)(fun: (Double, Double) => Double): Double = {
      val left = future { leftExpr.eval(env) }
      val right = future { rightExpr.eval(env) }
      fun(left(), right())
    }

    def parallelize(items: Seq[Expr])(fun: Seq[Double] => Double): Double = {
      fun(items.map { expr => future { e(expr) } }.map { f => f() })
    }

    expression match {
      case Num(x) => x
      case Name(x) => env.variable(x)
      case Call(name, args) => parallelize(args) { params => env.function(name).eval(env, params) }
      case x + y => parallelBinOp(x, y) { _ + _ }
      case x - y => parallelBinOp(x, y) { _ - _ }
      case x * y => parallelBinOp(x, y) { _ * _ }
      case x / y => parallelBinOp(x, y) { _ / _ }
      case x ^ y => parallelBinOp(x, y) { pow(_, _) }
      case _ => error("Cannot evaluate expression: " + expression)
    }
  }
}
