class Person(name: String, catchPhrase: String) {
  def greet() = println(greeting)
  protected def greeting() = "Hi, I am %s. %s".format(name, catchPhrase)
}

object Censor {
  val censoredWords = Map(
    "Shoot" -> "Puckey",
    "Darn" -> "Breans"
  )
}

trait Censor extends Person {
  override protected def greeting(): String = {
    (super.greeting /: Censor.censoredWords) { (greeting, replacement) =>
      greeting.replace(replacement._1, replacement._2)
    }
  }
}

var homer = new Person("Homer Simpsons", "Darn. Shoot! Darn it to Shoot") with Censor
homer.greet()
