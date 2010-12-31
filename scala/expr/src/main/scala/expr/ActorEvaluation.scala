package expr

import scala.actors.Actor
import scala.actors.Actor.{actor, react, reply, loopWhile, mkBody}

import BinOp._

case class PartialResult(index: Int, num: Double)

class ActorEvaluation(env: Env) {
  def mapReduce(exprs: List[Expr], index: Int, target: Actor)(reduce: Array[Double] => Double): Unit = {
    val mapper = actor {
      val evaluated: Array[Double] = Array.make(exprs.length, 0.0)

      times(exprs.length) {
        react { case PartialResult(n, value) => evaluated(n) = value }
      } andThen {
        target ! PartialResult(index, reduce(evaluated))
      }
    }

    for ((e, i) <- exprs.zipWithIndex) evalTo(e, i, mapper)
  }

  def evalTo(expr: Expr, index: Int, target: Actor): Unit = {
    def respond(answer: => Double) = actor { target ! PartialResult(index, answer) }
    def binOp(x: Expr, y: Expr)(op: (Double, Double) => Double) =
      mapReduce(List(x, y), index, target)(xs => op(xs(0), xs(1)))
    def function(name: String, args: Seq[Expr]) =
      mapReduce(args.toList, index, target) { xs => env.function(name).eval(env, xs.toList) }

    expr match {
      case Num(num) => respond { num }
      case Name(name) => respond { env.variable(name) }
      case Call(name, args) => function(name, args)
      case x + y => binOp(x, y) { _ + _ }
      case x - y => binOp(x, y) { _ - _ }
      case x * y => binOp(x, y) { _ * _ }
      case x / y => binOp(x, y) { _ / _ }
      case x ^ y => binOp(x, y) { Math.pow(_, _) }
    }
  }

  def eval(expr: Expr): Double = {
    val waiter = actor {
      var result = 0.0
      mkBody {
        react { case PartialResult(-1, num) => result = num }
      } andThen {
        react { case 'Eval => reply(result) }
      }
    }

    evalTo(expr, -1, waiter)
    (waiter !? 'Eval) match { case result: Double => result }
  }

  private def times(n: Int)(body: => Unit) = {
    var counter = n
    loopWhile(counter > 0) { counter -= 1; body }
  }
}
