package expr.repl

object ConsoleShell extends Shell {
  override def read(): String = {
    print("> ")
    Console.readLine
  }

  override def write(output: String) = print(output)
}
