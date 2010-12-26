package expr

object Env {
  sealed abstract class Value
  case class Number(val number: Double) extends Value
}

class Env(val mapping: Map[String, Env.Value]) {
  def this() { this(Map[String, Env.Value]()) }

  def apply(name: String) = mapping(name)

  def withVariable(name: String, number: Double): Env = {
    new Env(mapping + (name -> Env.Number(number)))
  }

  def withVariables(pairs: Seq[(String, Double)]): Env = {
    new Env(mapping ++ pairs.map(x => (x._1, Env.Number(x._2))))
  }

  def variable(name: String): Double = {
    this(name) match {
      case Env.Number(x) => x
      case _ => throw new NoSuchElementException("variable: " + name)
    }
  }
}
