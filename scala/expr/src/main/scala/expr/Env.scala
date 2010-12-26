package expr

object Env {
  sealed abstract class Value
  case class Number(val number: Double) extends Value
}

class Env(val mapping: Map[String, Env.Value]) {
  def this() { this(Map[String, Env.Value]()) }
  def this(pairs: (String, Double)*) { this(Map() ++ pairs.map(x => (x._1, Env.Number(x._2)))) }

  def apply(name: String) = mapping(name)

  def variable(name: String): Double = {
    this(name) match {
      case Env.Number(x) => x
      case _ => throw new NoSuchElementException("variable: " + name)
    }
  }
}
