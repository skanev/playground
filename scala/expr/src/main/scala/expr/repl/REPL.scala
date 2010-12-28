package expr.repl

import expr.BadInputException
import Command._

trait Shell {
  def read(): String
  def write(output: String)
  def writeln(output: String) = write(output + "\n")
}

object ConsoleShell extends Shell {
  override def read(): String = {
    print("> ")
    Console.readLine
  }
  override def write(output: String) = print(output)
}

object Main {
  def main(args: Array[String]) {
    new REPL(ConsoleShell).start()
  }
}

class REPL(val shell: Shell) {
  var env = Env.empty

  def start() = processNextLine()

  private def processNextLine() {
    try {
      processCommand(readCommand())
    } catch {
      case ex: BadInputException => shell.writeln("ERROR: Unparsable input")
      case ex: ExitSignal =>
        shell.writeln("Bye!")
        return
    }
    processNextLine()
  }

  private def processCommand(command: Command) {
    command match {
      case Exit() => throw new ExitSignal
      case Eval(expr) => shell.writeln("= " + Evaluator.eval(expr, env))
    }
  }

  private def readCommand(): Command = {
    Command.parse(shell.read)
  }
}
