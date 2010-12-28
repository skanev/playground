package expr.repl

trait Shell {
  def read(): String
  def write(output: String)
  def writeln(output: String) = write(output + "\n")
}
