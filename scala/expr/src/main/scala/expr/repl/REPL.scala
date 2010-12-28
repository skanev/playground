package expr.repl

import expr.BadInputException
import Command._

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
