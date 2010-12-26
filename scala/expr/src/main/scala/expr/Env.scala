package expr

object Env {
  sealed abstract class Value
  case class Variable(val number: Double) extends Value
  case class Function(val code: Callable) extends Value

  def empty = new Env(Map())
}

class Env(val mapping: Map[String, Env.Value]) {
  def apply(name: String) = mapping(name)
  def names: Set[String] = Set() ++ mapping.keys

  def extend(name: String, value: Env.Value): Env = new Env(mapping + (name -> value))
  def extend(name: String, number: Double): Env = extend(name, Env.Variable(number))
  def extend(name: String, code: Callable): Env = extend(name, Env.Function(code))
  def extend(pairs: Seq[(String, Double)]): Env = new Env(mapping ++ pairs.map { p => (p._1, Env.Variable(p._2)) })

  def variable(name: String): Double = {
    this(name) match {
      case Env.Variable(x) => x
      case _ => throw new NoSuchElementException("variable: " + name)
    }
  }

  def function(name: String): Callable = {
    this(name) match {
      case Env.Function(lambda) => lambda
      case _ => throw new NoSuchElementException("function: " + name)
    }
  }
}
