package expr.repl

object JLineShell extends Shell {
  val reader = new jline.ConsoleReader()
  override def read(): String = reader.readLine("> ")
  override def write(output: String) = print(output)
}
