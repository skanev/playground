class Person(name: String, catchPhrase: String) {
  def greet() = println(greeting)
  protected def greeting() = "Hi, I am %s. %s".format(name, catchPhrase)
}

object Censor {
  val censoredWords = readWordsFromFile

  private def readWordsFromFile: Map[String, String] = {
    val emptyMap: Map[String, String] = Map.empty
    val linesInFile = scala.io.Source.fromFile("censored_words.txt").getLines
    linesInFile.
      map(_.trim.split(": +")).
      filter(_.length == 2).
      foldLeft(emptyMap) { (map, words) => map + (words(0) -> words(1)) }
  }
}

trait Censor extends Person {
  override protected def greeting(): String = {
    (super.greeting /: Censor.censoredWords) { (greeting, replacement) =>
      greeting.replace(replacement._1, replacement._2)
    }
  }
}

var homer = new Person("Homer Simpsons", "Darn. Shoot! Darn it to Shoot.") with Censor
homer.greet()

