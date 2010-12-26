package expr

object Env {
  sealed abstract class Value
  case class Variable(val number: Double) extends Value
  case class Function(val code: Lambda) extends Value
}

class Env(val mapping: Map[String, Env.Value]) {
  def this() { this(Map[String, Env.Value]()) }

  def apply(name: String) = mapping(name)

  def withVariable(name: String, number: Double): Env = {
    new Env(mapping + (name -> Env.Variable(number)))
  }

  def withVariables(pairs: Seq[(String, Double)]): Env = {
    new Env(mapping ++ pairs.map(x => (x._1, Env.Variable(x._2))))
  }

  def withFunction(name: String, code: Lambda): Env = {
    new Env(mapping + (name -> Env.Function(code)))
  }

  def variable(name: String): Double = {
    this(name) match {
      case Env.Variable(x) => x
      case _ => throw new NoSuchElementException("variable: " + name)
    }
  }

  def function(name: String): Lambda = {
    this(name) match {
      case Env.Function(lambda) => lambda
      case _ => throw new NoSuchElementException("function: " + name)
    }
  }
}
