package expr.repl

import expr.BadInputException
import Command._
import Evaluator.eval

class REPL(val shell: Shell) {
  var env = Env.empty

  def start() = processNextLine()

  private def processNextLine() {
    try {
      processCommand(readCommand())
    } catch {
      case ex: BadInputException => shell.writeln("ERROR: Unparsable input")
      case ex: UndefinedNameException => shell.writeln("ERROR: " + ex.message)
      case ex: ExitSignal => return
    }
    processNextLine()
  }

  private def processCommand(command: Command) {
    command match {
      case Exit() => throw new ExitSignal
      case Eval(expr) => shell.writeln("= " + eval(expr, env))
      case Assign(name, expr) => env = env.extend(name, eval(expr, env))
    }
  }

  private def readCommand(): Command = {
    Command.parse(shell.read)
  }
}
