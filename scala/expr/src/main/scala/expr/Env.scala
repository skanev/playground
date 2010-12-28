package expr

object Env {
  sealed abstract class Value { def repr: String }
  case class Variable(val number: Double) extends Value { def repr = number.toString }
  case class Function(val code: Callable) extends Value { def repr = code.toString }

  def empty = new Env(Map())
}

class Env(val mapping: Map[String, Env.Value]) {
  import Env.{Value, Variable, Function}

  def apply(name: String): Value = mapping(name)

  def extend(name: String, value: Env.Value): Env = new Env(mapping + (name -> value))
  def extend(name: String, number: Double): Env = extend(name, Variable(number))
  def extend(name: String, code: Callable): Env = extend(name, Function(code))
  def extend(pairs: Seq[(String, Double)]): Env = new Env(mapping ++ pairs.map { p => (p._1, Variable(p._2)) })

  def names: Set[String] = Set() ++ mapping.keys

  def variable(name: String): Double = {
    mapping.get(name) match {
      case Some(Variable(x)) => x
      case _ => throw new ExprException("Undefined variable: " + name)
    }
  }

  def function(name: String): Callable = {
    mapping.get(name) match {
      case Some(Function(lambda)) => lambda
      case _ => throw new ExprException("Undefined function: " + name)
    }
  }
}
