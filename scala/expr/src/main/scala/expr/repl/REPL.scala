package expr.repl

import expr.BadInputException
import Command._

object Main {
  def main(args: Array[String]) {
    new REPL().start()
  }
}

class REPL {
  var env = Env.empty

  def start() = processNextLine()

  private def processNextLine() {
    try {
      processCommand(readCommand())
    } catch {
      case ex: BadInputException => println("ERROR: Unparsable input")
      case ex: ExitSignal => return
    }
    processNextLine()
  }

  private def processCommand(command: Command) {
    command match {
      case Exit() => throw new ExitSignal
      case Eval(expr) => println("= " + Evaluator.eval(expr, env))
    }
  }

  private def readCommand(): Command = {
    print("> ")
    Command.parse(Console.readLine)
  }
}
